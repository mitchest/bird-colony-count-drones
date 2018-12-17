library(raster)
library(SpaDES)
library(jpeg)

# image <- raster("data/testing_images/70/b1.JPG", band = 1)
# 
# split_image <- splitRaster(r = image, nx = 16, ny = 16, path = "data/testing_images/70/b1_split/")
# names(split_image) <- 1:length(split_image)
# 
# 
# write_raster <- function(idx, rasters, path) {
#   raster_idx <- rasters[[idx]]
#   raster_array <- array()
#   
#   jpeg(filename = paste0(path,idx,".jpg"), width = raster, height = nrow(raster_idx))
#   plot(raster_idx)
#   dev.off()
# }
# 
# lapply(1:length(split_image), write_raster, split_image, "data/testing_images/70/b1_split/")

### functionalise later from here ###

image <- readJPEG("data/testing_images/70/b1.JPG")


mat_split <- function(M, r, c){
  nr <- ceiling(nrow(M)/r)
  nc <- ceiling(ncol(M)/c)
  newM <- matrix(NA, nr*r, nc*c)
  newM[1:nrow(M), 1:ncol(M)] <- M
  
  div_k <- kronecker(matrix(seq_len(nr*nc), nr, byrow = TRUE), matrix(1, r, c))
  matlist <- split(newM, div_k)
  N <- length(matlist)
  mats <- unlist(matlist)
  dim(mats)<-c(r, c, N)
  return(mats)
}

b1_splits <- mat_split(image[,,1], 62, 62)
b2_splits <- mat_split(image[,,2], 62, 62)
b3_splits <- mat_split(image[,,3], 62, 62)

stack_array_tiles <- function(idx, b1, b2, b3) {
  b1x <- b1[,,idx]
  b2x <- b2[,,idx]
  b3x <- b3[,,idx]
  array(c(b1x, b2x, b3x),
        dim = c(dim(b1)[1:2],3))
}

image_tiles <- lapply(1:dim(b1_splits)[3], stack_array_tiles, b1_splits, b2_splits, b3_splits)


write_jpeg_tiles <- function(idx, tiles, path) {
  writeJPEG(tiles[[idx]], target = paste0(path,idx,".jpg"), quality = 1)
}


lapply(1:length(image_tiles), write_jpeg_tiles, image_tiles, "data/booligal_tiles/b1/")



"data/testing_images/70/b1_split/"
