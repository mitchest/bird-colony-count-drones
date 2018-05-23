// Barmah Millewa Forest
// Table of Contents

// 0. Constants
// 1. Loading the data
// 2. Calculate features
// 3. Train classifier and fit model
// 4. Export

// 0. Constants
var EXPORT = true
var NUM_TREES = 500
var SEED = 42
var NUM_SITES = 5 // out of 19
var NON_NEST_POINTS = 1e5
var REDUCER_SCALE = 0.05
var EXPORT_SCALE = 0.1
var USE_RGB = false

var filename = function (){
  return '/users/mitchest/drone-nest-count/barmah_rf_rscale' + 
     REDUCER_SCALE*100 + '_exp_scale' + EXPORT_SCALE*100 + '_sites' + NUM_SITES
}
var OUTPUT_FILE = filename()

// 1. Loading the data
// load image data
var ortho = ee.Image("users/mitchest/drone-nest-count/barmah_ortho")
// dsm's are a good choice if features of interest are distinct, in this case there's too much lignum that 'looks' like nests
// var dsm = ee.Image("users/mitchest/drone-nest-count/")

// load nest and other feature data
var nests = ee.FeatureCollection('ft:1Bbqc8wSjCmhG9xn2ZZ0O1p2UkopKI5QHIhHKipz2') //identified nests
var colony = ee.FeatureCollection("users/mitchest/drone-nest-count/barmah_colony_boundary") //colony boundary

var training_sites = ee.FeatureCollection("users/mitchest/drone-nest-count/barmah_training_sites") //potnetial image training sites
training_sites = training_sites.limit(NUM_SITES)

// choose training data
var buff_points = function (feature) {
  return feature.buffer(30)
}

var training_buffers = training_sites.map(buff_points)
var nests_training = nests.filterBounds(training_buffers)

Map.addLayer({ eeObject: ortho, name: 'ortho' })
Map.addLayer({ eeObject: training_buffers, name: 'training areas', shown: false })
Map.addLayer({ eeObject: nests, name: 'all nests' })
Map.addLayer({ eeObject: nests_training, name: 'training nests', shown: false })
if(!EXPORT) Map.centerObject(nests_training, 24)

// 2. Calculating features
var calculate_predictors = function () {
  // Create a more normalised brightness image
  var white = ortho.select('b1').add(ortho.select('b2').divide(ortho.select('b3'))).rename('white')
  // Local image 'topography' through laplacian kernel
  var white_lapl8 = white.convolve(ee.Kernel.laplacian8())

  // metrics to try distinguish vegetation (awesome if you fly a multispec sensor)
  var grvi = ortho.expression(
    '(g - r) / (g + r)', {
      'g': ortho.select('b2'),
      'r': ortho.select('b1')
    }).rename('grvi')

  var gbri = ortho.expression(
    'g / (b + g + r)', {
      'b': ortho.select('b3'),
      'g': ortho.select('b2'),
      'r': ortho.select('b1')
    }).rename('gbri')

  // glcm metrics on the ortho image (can choose when to do on normalised image and/or ortho data)
  var white_glcm = white.toUint16().glcmTexture({ size: 1 }).select(1, 3, 4, 16)
  var blue_glcm = ortho.select('b3').glcmTexture({ size: 1 }).select(16)

  var boxcar = ee.Kernel.square({
    radius: 5, units: 'pixels', normalize: true
  })

  var blue_glcm_smooth = blue_glcm.convolve(boxcar).rename('blue_glcm')

  // add trianing data at this point together
  var train_composite = white.addBands([white_lapl8, grvi, gbri, white_glcm, blue_glcm_smooth])
  if(USE_RGB) train_composite = train_composite.addBands(ortho)
  
  // This feature is about the stdDeviation in a circle around the point
  train_composite = train_composite.addBands([2, 7].map(
    function (r) {
      return train_composite.select(['gbri', 'white_shade']).reduceNeighborhood({
        reducer: ee.Reducer.stdDev(),
        kernel: ee.Kernel.circle(r)
      })// .rename('circle_r_' + r)
    }))
  print(train_composite.bandNames())
  // try a difference of gaussians (dog) edge detection kernel on the white ortho image
  // this is the kernel
  var dog = ee.Kernel.gaussian(4, 3, "pixels", false).add(ee.Kernel.gaussian(3, 0.5, "pixels", false, -1))
  // apply
  var dog_1 = train_composite.select('white_shade_stdDev').convolve(dog).rename('dog1')
  // iterate the dog
  var dog_2 = dog_1.convolve(dog).rename('dog2')

  return train_composite.addBands([dog_1, dog_2])
}

var train_composite = calculate_predictors()

//train_composite = train_composite.select(['b1', 'b2', 'b3', 'white'])

Map.addLayer(train_composite, {}, 'predictor layers')

/*
Map.addLayer(train_composite, {min: 0.32, max: 25783, bands: [
  'white_shade_stdDev_1',
  'white_shade_stdDev_1',
  'gbri']},'stddev and gbri', false)
*/

/*
There is lots of room for exploration here ie: potential for lots of (potentially really good) predictors.
The predictors might also be really specific to the nest and local enviroment.
So a supervised learning method that is able to do feature selection should be important. 
*/


// 3. Train classifier and fit model

// boilerplate from now on
var nests_class = ee.Image(1).toUint16().reduceToVectors({
  geometry: nests_training,
  geometryType: 'centroid',
  scale: REDUCER_SCALE
})

// TODO check whether we need to make these points fall outside some distance from nests...
// ANSWER: definitely yes...
var non_nests = ee.Image(0).toUint16().reduceToVectors({
  geometry: ee.FeatureCollection.randomPoints(colony, NON_NEST_POINTS, SEED),
  geometryType: 'centroid',
  scale: REDUCER_SCALE
})


var train_points = ee.FeatureCollection([nests_class, non_nests]).flatten()

var my_bands = train_composite.bandNames()
if(!EXPORT) print('train_image', my_bands)

var train_vectors = train_composite.reduceRegions({
  reducer: 'first',
  collection: train_points,
  scale: 0.1 // 4 cm around a point is probably enough (given nest marking point is variable)
})

var classifier = ee.Classifier.randomForest({
  numberOfTrees: NUM_TREES,
  minLeafPopulation: 10,
  seed: SEED
}).train({
  features: train_vectors,
  classProperty: 'label',
  inputProperties: my_bands
})

var bird_nests = train_composite.classify(classifier)


// 4. Export

var export_img = function () {
  Export.image.toAsset({
    image: bird_nests,
    description: 'barmah',
    assetId: OUTPUT_FILE,
    scale: EXPORT_SCALE,
    region: colony,
    maxPixels: 1e10
  })
}

if (EXPORT) export_img() 
else {
  Map.addLayer(bird_nests.mask(bird_nests.eq(1)), { min: 0, max: 1 }, 'new')
}
