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


extractDart <- function( dartOutput, variable ){

    # extract response data from dart output
    # bfile = path to brick of things to extract
    # f = filename
    # rename = replace original file with 'renamed' file
    
    cat('loading response variables\n');flush.console()
    
    BB <- brick(dpar$respVars[[variable]])
    
    bx <- rbind(dartOutput$targetRast[,1], dartOutput$chosenPixels[,1])
    
    m <- crop(BB, bx, progress = 'text')
    
    cat('extracting padvals\n') ; flush.console()
    padvals <- list( extract(m, dartOutput$targetRast, progress = 'text'))
    names(padvals) <- variable
    
    cat('extracting reference vals\n') ; flush.console()
    allvals <- list(extract(m, dartOutput$chosenPixels, progress = 'text'))
    names(allvals) <- variable
    
    return( list( target = padvals, reference = allvals) )

}

synthCtrl <- function( extraction , firstYear , variable){

  
    #CI wants matrix with first column = treated
    # No NAs!
    
    trt <- colMeans( extraction$target[[variable]])
    ctr <- extraction$reference[[variable]]


    #rearrange
    X <- cbind(trt, t(ctr))
    
    # pre and post
    pre <- c(1, grep( (firstYear - 1) , 1984:2020))
    post <- c( grep( firstYear , 1984:2020), nrow(X) )
    
     colnames(X) <- NULL
     rownames(X) <- as.character(c(1984:2020)[1:nrow(X)])

    y <- CausalImpact::CausalImpact(X, pre.period = pre, post.period  =post)

    return(y)
    
  }



getQuantiles <- function(extraction, variable){

  tg <- data.frame(extraction$target[[variable]])
  ref <- data.frame(extraction$reference[[variable]])
  
  # namer -- change this!
  names(tg) <- names(ref) <- paste0('x', 1:ncol(tg))
  
  tg$id <- paste0('tg', 1:nrow(tg))
  tg <- melt(tg, id = 'id')

  ref$id <- paste0('rf', 1:nrow(ref))
  ref <- melt(ref, id = 'id')

  r <- rbind(tg, ref)
  r$group <- ifelse( grepl('rf', r$id), 'Reference', 'Treated')
  
  r$xval <- as.numeric( gsub('[^0-9]', '', r$variable))
  r$xval <- c(1984:2020)[r$xval]
  
  r <- data.table(na.omit(r))
  setorder(r, id)
  
  ## plot Quantile trajectory
  v <- r[, ecdf(value[which(group != 'Treated')])( value[which(group == 'Treated')] ), by = list(xval)]
  v[,id:= 1:.N, by = list(xval)]
  V <- dcast(v, xval  ~ id, value.var = 'V1')  
  names(V) <- paste0('pixel_', names(V),'_', variable, '_quantile')
  names(V)[grep('xval', names(V))] <- 'year'
  
  V[[paste0('avg_', variable, '_quantile')]] <- rowMeans(V[, -c('year'), with = F])
  V <- V[, c(1, ncol(V), 2:(ncol(V)-1)), with = F]
  
  as.data.frame(V)

}





ts_plot <- function( extraction, variable){


  require(ggplot2)
  require(data.table)
  require(gridExtra)
  library(ggridges)
  
  # str(extraction)
  # str(variable)
  tg <- data.frame(extraction$target[[variable]])
  ref <- data.frame(extraction$reference[[variable]])
  
  # namer -- change this!
  names(tg) <- names(ref) <- paste0('x', 1:ncol(tg))
  
  tg$id <- paste0('tg', 1:nrow(tg))
  tg <- melt(tg, id = 'id')

  ref$id <- paste0('rf', 1:nrow(ref))
  ref <- melt(ref, id = 'id')

  r <- rbind(tg, ref)
  r$group <- ifelse( grepl('rf', r$id), 'Reference', 'Treated')
  
  r$xval <- as.numeric( gsub('[^0-9]', '', r$variable))
  r$xval <- c(1984:2020)[r$xval]
  
  r <- data.table(na.omit(r))
  setorder(r, id)
  
  ## plot Quantile trajectory
  v <- r[, ecdf(value[which(group != 'Treated')])(value[which(group == 'Treated')] ), by = list(xval)]
  v[,id:= 1:.N, by = list(xval)]
  p1 <- ggplot(v, aes(x = xval, y = V1, group = id) ) + geom_line(col = 'red', size= .6, alpha = .25) + ylim(0,1) + theme_bw() + ylab('quantile') + xlab('time') + ggtitle( paste0( variable, 'quantile'))
  
  v <- r[, list( 
                treated = mean(value[ which(group == 'Treated')]), 
                refUp = max(value[ which(group != 'Treated')]),
                refDn = min(value[ which(group != 'Treated')]), 
                iqrUp = quantile(value[ which(group != 'Treated')], .75, na.rm = TRUE),
                iqrDn = quantile(value[ which(group != 'Treated')], .25, na.rm = TRUE)), by = xval]

  v <- r[, list( 
                treated = mean(value), 
                refUp = mean(value) + sd(value),#/sqrt(.N),
                refDn = mean(value) - sd(value))#/sqrt(.N)) 
                , by = list(group,xval)]

  p2 <- ggplot(v, aes(x = xval, y = treated, color = group, fill = group) ) + geom_ribbon(aes(ymin=refDn, ymax=refUp, x=xval), alpha = 0.3,colour = NA) + scale_color_manual(values=c("#999999", "red", "#56B4E9")) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  geom_line( size  =2) + theme_bw() + xlab('Time') + ylab(variable) + theme(legend.position="top", legend.title = element_blank())
 
   
   trted <- data.table(r)[group == 'Treated', list(val = mean(as.numeric(value)), ysmol = unique(as.numeric(xval))), by = list(xval = as.factor(xval))]
  trted$ybig <- trted$ysmol + .9
  trted <- na.omit(trted)
  qs <- quantile(r$value, c(0.001, 0.999))
 
 # Fig1 <- ggplot( r, aes( x = value, y = as.factor(xval), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
     # stat_density_ridges(geom = 'density_ridges_gradient', calc_ecdf = TRUE, scale = .9) 
  # ingredients <- ggplot_build(Fig1) %>% purrr::pluck("data", 1)

  
  p3 <-   ggplot( r, aes( x = value, y = as.factor(xval), fill = stat(x))) + 
      geom_density_ridges_gradient(scale = .9) + 
     # stat_density_ridges(geom = 'density_ridges_gradient', calc_ecdf = TRUE, scale = .9) + 
      scale_fill_viridis_c(option = 'C')+ 
      #coord_flip() +
      coord_cartesian(xlim = qs) +
      geom_segment(data = trted, aes(x = val, y = ysmol, xend = val, yend = ybig)) + 
      theme_bw() + theme(legend.position="none") + ylab('') + xlab(variable)
  

  grid.arrange(p2,p1,p3, layout_matrix = matrix(c(1,2,1,2,3,3),2, 3))

 
  
}


viewImportance <- function(d){
  require(rpart)
  
  dat <- d$chosenPixels@data
 # cat('=====\n',names(key)[which(!names(key) %in% names(dat))], '\n=====\n', file = stderr())
  key <- c(
  'EASTNESS' = "Eastness",#	index from 1 to -1  of how east (1) or west (-1) a site faces 
  'SOUTHNESS' = 'Southness',#	index from 1 to -1  of how south (1) or north (-1) a site faces 
  'ELEVm' = 'Elev', #	elevation in meters
  'TCURV' = "Cross Slope Curvature",#	curvature perpendicular to the slope direction 
  'PCURV' = "Down Slope Curvature",	#curvature parallel to the slope direction
  "SLOPE" = "Slope",	#slope gradient in degrees 
  "MRRTF" = "Ridgetop Flatness",#	multiple resolution ridgetop flatness index 
  "MRVBF" = "Valley Flatness", #	multiple resolution valley bottom flatness index
  "RELHT1" = "Rel  Height 1m", #Height of cell above the local minimum elevation in 1-pixel radius
  "RELHT32" = "Rel Height 32m", # Height of cell above the local minimum elevation in 32-pixel radius
  "RELHT128" = "Rel  Height 128m", #	Height of cell above the local minimum elevation in 128-pixel radius
  "RELMNHT1" = "Rel  Mean Height 1m",#	Height of cell above the local mean elevation in 1-pixel radius
  "RELMNHT32"= "Rel  Mean Height 32m",#	Height of cell above the local mean elevation in 32-pixel radius
  "RELMNHT128"= "Rel  Mean Height 128m",#	Height of cell above the local mean elevation in 128-pixel radius
  "TWI_TOPMODEL" = "Topo  Wetness Index",#	Topographic wetness index from topmodel in SAGA GIS.
  "CAlog_10" = "Upslope contrib  area", #	Upslope contributing area in log10 units
  "LFELEMS" = "Landform Class", #	Landform classification system using DEM: landform elements
  "distance" = "topoDistance"
  )

  dat <- dat[, names(key)]
 # cat('=====\n',names(key)[which(!names(key) %in% names(dat))], '\n=====\n', file = stderr())
  dat$LFELEMS <- as.factor(dat$LFELEMS)
  names(dat) <- key
  # cat('=====\n',names(dat), '\n=====\n', file = stderr())
  # cat('=====\n',dim(dat), '\n=====\n', file = stderr())
  # cat('=====\n',str(dat), '\n=====\n', file = stderr())
  dis <- rpart(topoDistance ~ . , dat)
  # dis <- rpart(distance ~ . , dat)
  return( visNetwork::visTree(dis, digits = 2, legend = FALSE, height= '100%',submain = "Factors related to topographic distance"))
}


generateRmdReport <- function(file, params){
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        td <- tempdir()
        tempReport <- file.path( td, "report.Rmd")
        
        
        # generate static images for report
        mapview::mapshot(params$bigMap, file = file.path(td,'overviewimage.png'))
        mapview::mapshot(params$smallMap, file = file.path(td,'smallmap.png'))
        mapview::mapshot(params$dartMap, file = file.path(td,'dartmap.png'))
        
        
        file.copy("dartReport.rmd", tempReport, overwrite = TRUE)
        


        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )

}
