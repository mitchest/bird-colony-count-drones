# Monitoring large and complex wildlife aggregations with drones

- Data and code for a forthcoming paper on monitoring and semi-automated mapping of really large and complex bird colonies. 
- Forthcoming paper in *Methods in Ecology and Evolution* (May 2019):  
  Lyons, M., K. Brandis, J. Wilshire, N. Murray, J. McCann, R. Kingsford, and C. Callaghan (2019). Monitoring large and complex wildlife aggregations with drones. *Methods in Ecology and Evolution*.  
  https://doi.org/10.1111/2041-210X.13194    
- Contact for more details or [check out the pre-print here](https://doi.org/10.32942/osf.io/w247h)
- See [this Google Earth Engine app](https://mitchest.users.earthengine.app/view/ibis-drone-count) to explore some of the data

The basic concept is as such:
- Most automated classification algorithms focus on consistent targets with high contrast, but many biological phenomenon are not like this
- For example, many large breeding bird colonies are not like this (e.g. birds of different ages, nests of different ages that may be green/brown/white, nests that are on their own or in clumps of 5/10/50, empty or occupied nests, nests with eggs, birds by themselves, birds flying around, etc. etc.)
- This approach focuses on using a remote sensing approach (via drone imagery) to map all of the targets of interest, and then subsequently using an algorithmic or modeling approach to estimate the parameter of interest (e.g. number of nests in our case)
- We demonstrate for 4 different colonies, ranging in size from ~20-30,000 to ~200,000 birds
- The mapping is implemented in the Google Earth Engine, and modelling in R

A small extract from one colony is below, to give a flavour of the study sites - this is a nice simple example with pretty consistent targets.

### Orthomosaic:
- ~3 cm RGB drone imagery, processed via Pix4D  
![](https://github.com/mitchest/bird-colony-count-drones/blob/master/readme/ortho.JPG)

### Manual counting
- All visible nests were counted, a small proportion of which is used for training image classifiers  
![](https://github.com/mitchest/bird-colony-count-drones/blob/master/readme/nest_dots.JPG)

### Nest/bird classification
- Nest material and birds classified  
![](https://github.com/mitchest/bird-colony-count-drones/blob/master/readme/nest_class.JPG)

### Nest count estimation
- The mapped areas are then used - again using a small proportion of the manually counted nest data as training - to estimate the parameter of interest: the number of nests
- We are trying to maximise the accuracy of the counts, and minimise the amount of manual effort
- Note these results are not finalised yet  
![](https://github.com/mitchest/bird-colony-count-drones/blob/master/readme/nest_est.png)
