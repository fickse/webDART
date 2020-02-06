 # run Art

######################################################
# Parameters template

dpar <- list(

    
    #vars to filter by
    filterVars = list(
      soilec = "/data/rasters/soil/ec_0to60cm_100xInt_ucrb.tif",
      soilps = "/data/rasters/soil/UCRB_mPSC_RFE_10plus.tif"
    ),

    # masking variables: 1 = mask, 0 = ok
    maskVars = list(
      refrast =  "/data/rasters/masks/refrast.tif",
      allMasks = "/data/rasters/masks/allMasks.tif"
    ),

    # variables for distance matrix
    topoVars = list(
        ELEVm = "/data/rasters/topo/ELEVm.tif",
        PCURV = "/data/rasters/topo/PCURV.tif",
        TCURV = "/data/rasters/topo/TCURV.tif",
        RELHT1 ="/data/rasters/topo/RELHT1.tif",
        RELHT32 ="/data/rasters/topo/RELHT32.tif",
        RELHT128 ="/data/rasters/topo/RELHT128.tif",
        RELMNHT1 ="/data/rasters/topo/RELMNHT1.tif",
        RELMNHT32 ="/data/rasters/topo/RELMNHT32.tif",
        RELMNHT128 ="/data/rasters/topo/RELMNHT128.tif",
        MRRTF ="/data/rasters/topo/MRRTF.tif",
        MRVBF ="/data/rasters/topo/MRVBF.tif",
        SLOPE ="/data/rasters/topo/SLOPE.tif",
        SOUTHNESS ="/data/rasters/topo/SOUTHNESS.tif",
        EASTNESS ="/data/rasters/topo/EASTNESS.tif",
        TWI_TOPMODEL="/data/rasters/topo/TWI_TOPMODEL.tif",
        CAlog_10="/data/rasters/topo/CAlog_10.tif",
        LFELEMS="/data/rasters/topo/LFELEMS.tif"
    ),


    ## response variables
    respVars = list(
     satvi = "/data/rasters/response/SATVI/satvi_median_1984_2018_Mar_Nov.vrt"
    )
    

)

