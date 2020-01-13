 # run Art

######################################################
# Parameters template

dpar <- list(

    
    #vars to filter by
    filterVars = list(
      soilec = "/home/steve/data/DART2/rasters_input/ec_0to60cm_100xInt_ucrb.tif",
      soilps = "/home/steve/data/DART2/rasters_input/UCRB_mPSC_RFE_10plus.tif"#,
#      vegPotential = "/home/steve/data/GIS_ARCHIVE/Landfire/intermediate.grd"
    ),

    # masking variables: 1 = mask, 0 = ok
    maskVars = list(
      refrast =  "/home/steve/data/DART2/rasters_input/refrast.tif",
      roadrast =  "/home/steve/data/GIS_ARCHIVE/ROADS/TIGER2018/TIGER_2018_UCRB_MASK.tif",
      otherpads =  "/home/steve/data/DART2/rasters_input/WYCOriv_Nopads.tif",
      oilgas =      "/home/steve/data/GIS_ARCHIVE/OILGAS/pads4corners2016/oil_gas_buf_ucrb_mask.tif",
      othersites =  "/home/steve/data/GIS_ARCHIVE/MANAGEMENT/mask.grd",
      exclosures = "/home/steve/data/GIS_ARCHIVE/MANAGEMENT/MFO_EXCLOSURES/mfo_exclosures_mask.grd",
      fires= "/home/steve/data/GIS_ARCHIVE/MTBS/mtbs_mask.tif",
      utblmfires= '/home/steve/data/GIS_ARCHIVE/UTFIRES/utfire_mask.tif',
      irrigated =  "/home/steve/data/DART2/rasters_input/WYCOrivnotIrrigated.tif",
      coal = "/home/steve/data/DART2/rasters_input/NoCoalCOriv.tif",
      wind = "/home/steve/data/DART2/rasters_input/WY_COriv_nowind.tif",
      nlcd = "/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/NLCDcl.tif"
    ),

    # variables for distance matrix
    topoVars = list(
        ELEVm = "/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/ELEVm.tif",
        PCURV = "/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/PCURV.tif",
        TCURV = "/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/TCURV.tif",
        RELHT1 ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/RELHT1.tif",
        RELHT32 ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/RELHT32.tif",
        RELHT128 ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/RELHT128.tif",
        RELMNHT1 ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/RELMNHT1.tif",
        RELMNHT32 ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/RELMNHT32.tif",
        RELMNHT128 ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/RELMNHT128.tif",
        MRRTF ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/MRRTF.tif",
        MRVBF ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/MRVBF.tif",
        SLOPE ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/SLOPE.tif",
        SOUTHNESS ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/SOUTHNESS.tif",
        EASTNESS ="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/EASTNESS.tif",
        TWI_TOPMODEL="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/TWI_TOPMODEL.tif",
        CAlog_10="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/CAlog_10.tif",
        LFELEMS="/home/steve/data/GIS_ARCHIVE/UCRB_Covariates/LFELEMS.tif"
    ),


    ## response variables
    respVars = list(
     satvi = "/home/steve/data/GIS_ARCHIVE/EE/SATVI/satvi_median_1984_2018_Mar_Nov.vrt"
    )
    


)

