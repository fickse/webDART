################################
# Dart Web Shiny App Utilities #
################################

# Steve Fick
# 12/31/2019



getDart <- function( x, y, buffer = 60, searchRadius = 3000, nControl = 100, targetRadius = 30) {

    # Generate dart pixels
    
    # X = x coordinate in EPSG 5070
    # Y = y coordinate in EPSG 5070
    # buffer = distance to buffer target in m
    # searchRadius = distance to search for matches (m)
    # nControl = number of matches to look for
    # targetDim = edge length in pixels of target area (1x1, 2x2, 3x3)
    
    ## Data checks
    
       # x and y within bounding area
       
       # buffer >= 0
       
       # searchRadius >= 0 & > buffer
       
       # nControl > 0
       
       # targetDim > 0 & less than MAX
    
    ## Generate target area polygon
       target <- getTarget(x,y, targetRadius)
    
    ## Load and crop Dart covariates
       rasterData <- getRasterData(target, searchRadius)
       
    ## Generate Masked Candidate Pixels
       candidates <- maskCandidates(target, buffer, searchRadius, rasterData)
    
    ## Generate Masked Target Pixels
       targetRaster <- maskTarget(target, rasterData)
    
    ## Get edaphic Subset
        edaphicRaster <- edaphicSubset( targetRaster, candidates)
        
    ## Get topos Subset
        chosenPixels <- topoSubset( targetRaster, edaphicRaster, nControl) 
    
    ## Return
      list(
        # raster of candidates
        candidates = raster( candidates['refrast'] ),
        
        # raster of target(s)
        targetRast = targetRaster, 
        
        # polygon of target
        targetPoly = target,
        
        # raster of topo-edaphic subset
        edaphicRaster = raster( edaphicRaster['refrast'] ),
        
        # raster of topN pixels w/ value = gower distance
        chosenPixels = chosenPixels
      )
}

e5070 <- function(){
  # helper for EPSG5070 proj string
  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=1,1,-1,0,0,0,0 +units=m +no_defs"
}

toE5070 <- function(x,y){
    
    sp <- SpatialPoints( coordinates(cbind(x,y)), CRS(projection(raster())))
    spTransform(sp, e5070())
}

project <- function(sp){

  if(projection(sp) != projection(raster()))
  spTransform(sp, projection(raster()))

}

getTarget <- function(x,y, targetRadius){
  
  # convert to EPSG5070 and draw polygon
  target <- toE5070(x,y)
  target <- buffer(target, targetRadius)
  target

}

getRasterData <- function(target, searchRadius){

   cat(' ** loading raster data\n')
      
   get_fun <- function(x) { 
       
        s <- lapply( names(x), function(y) {  r <- brick(x[[y]]) ; names(r) <- y; r})
        stack(s)
   
   }
   
   toponames <- gsub(".tif","", names(dpar$topoVars))
   masks <- get_fun(dpar$maskVars)
   topovars <- get_fun(dpar$topoVars)
   filtervars <- get_fun(dpar$filterVars)
   b <- stack(masks, topovars, filtervars)
   rast.proj <- projection(masks[['refrast']])

   ## Prepare neighborhood
   padpolybuf <- buffer(target, searchRadius)

   ## crop
   cat('\tcropping predictors\n')
   zone <- crop(b, padpolybuf)

   return( zone )
}




maskCandidates <- function(target, buffer, searchRadius, rasterData){

   cat(' ** masking candidates\n')
   
   #Generate raster brick of masked candidate pixels
   padpolybuf <- buffer(target, width = searchRadius)
   padpolybuf$rastval <- 1
   padbufrast <- rasterize(padpolybuf, rasterData[[1]], field=padpolybuf$rastval, datatype='INT1U')

  ## Screen out unwanted disturbances in buffer zone
  f_mask <- function(a,b) a*b

  masks <- names(dpar$maskVars)

  for( msk in masks){
  
   if( msk == 'refrast') next()
   if( msk == 'nlcd' ) next()
   
    padbufrast <- overlay(padbufrast, rasterData[[msk]], fun = f_mask)
    
  }

  # mask everything
  cat('\tmasking buffer\n')
  padbufrast[which(padbufrast[] == 0)] <- NA
  padbufstk <- overlay(padbufrast, rasterData, fun = f_mask)

  # make donut
  cat('\tmaking donut\n')
  padpolyb <- buffer(target, width = buffer)
  padbufstk <- mask(padbufstk, padpolyb, inverse = TRUE)

  # propagate names
  names(padbufstk) <-  names(rasterData)

  # checks
  cat('\tchecking number of candidates in buffer\n')
  if(all(is.na(padbufrast[]))) stop( 'ERROR1: no non-na candidate pixels in buffer (every pixel masked)')

  cat('\tconverting buffer to Spatial Pixels\n')
  padbufpixels <- as(padbufstk, "SpatialPixelsDataFrame")

  return( padbufpixels )

}

maskTarget <- function(target, rasterData){

   # return raster of target pixels
   cat(' ** getting treated pixels\n')

  ## Create rasters
  target$rastval <- 1
  padrast <- rasterize(target, crop(rasterData[[1]], target),field=target$rastval, datatype='INT1U')

  # make donut hole
  padstk <- crop(rasterData, target)
  f_mask <- function(a,b) a*b

  # mask out unwanted  pixels in treated area
  padMask <- padstk[[1]]
  for( variable in dpar$interiorMaskVars){
        if( variable == 'nlcd' ) next()
   
       padMask  <- overlay( padstk[[variable]], padMask, fun = f_mask)
  }

  padMask[padMask[] == 0] <- NA
  padstk <- overlay(padMask, padstk, fun = f_mask)
  names(padstk) <-  names(rasterData)

  # check that there are enough padpixels
  cat('\tchecking for enough non-na pixels in treatment area\n')
  if(all(is.na(padrast[]))) stop( 'ERROR2: not enough pixels in treatment polygon;')

  cat('\tconverting treatment raster to Spatial Pixels DFs\n')

  padpixels <- as(padstk, "SpatialPixelsDataFrame")

  return( padpixels )

}

edaphicSubset <- function( targetRaster, candidates){
    
   # Function to find edaphic matches
   w <-   which( 
                candidates@data$soilps %in% targetRaster@data$soilps &
                candidates@data$soilec > min( targetRaster@data$soilec) * .95 &
                candidates@data$soilec < max( targetRaster@data$soilec) * 1.05
                )
    
   candidates[w,]

}

topoSubset <- function( targetRaster, edaphicRaster, nControl){
      require(gower)
      toponames <- names(dpar$topoVars)

      # select comparison vars in target
      dat1 <- targetRaster@data[, c(toponames)]

      # select buffered candidate references
      dat2 <- edaphicRaster@data[,c(toponames)]

      # get similarity matrix for top 100 ref pixels for each target pixel
      cat('getting similarity index \n')
      toposim <- gower_topn(x=dat1, y=dat2, n=nControl, nthread = 1)

      # pick top N most frequently selected indices
      tabs <- rev( sort( table( c( toposim$index ) ) ) )
      ids <- as.numeric( names(tabs[1: pmin( nControl, length(tabs) ) ] ) )
      freqs <- tabs[ 1:pmin( nControl, length(tabs) ) ] 
      dists <- aggregate( list( d = c(toposim$distance)), by = list(id = c(toposim$index)), FUN= mean)
      dists <- round(dists[ match(ids, dists$id), 'd'], 4)

      # subset and add average gower distance attribute
      out <- edaphicRaster[ids,]
      out$distance <- dists
      out$freq <- freqs
      
      out
}
