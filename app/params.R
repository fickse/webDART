 # run Art

######################################################
# Parameters template

dpar <- list(

    
    #vars to filter by
    filterVars = list(
      soilec = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/ec_0to60cm_100xInt_ucrb.tif",
      soilps = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/UCRB_mPSC_RFE_10plus.tif"
    ),

    # masking variables: 1 = ok, 0 = mask
    maskVars = list(
      refrast =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/refrast.tif",
      roadrast =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/TIGER_2018_ucrb_mask.tif",
      otherpads =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/WYCOriv_Nopads.tif",
      oilgas =      "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/oil_gas_buf_ucrb_mask.tif",
      oilgas4corners= "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/fourCorners_oilgas_mask.tif",
      othersites =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/treatment_mask.tif",
      disturbance = '/lustre/projects/ecosystems/sbsc/ucrb/GIS/LANDFIRE/disturbance/allDist2.tif',
      exclosures = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/mfo_exclosures_mask.tif",
      fires= "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/mtbs_mask.tif",
      utblmfires= '/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/utfire_mask.tif',
     irrigated =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/WYCOrivnotIrrigated.tif",
     coal = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/NoCoalCOriv.tif",
     wind = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/WY_COriv_nowind.tif",
#      nlcd = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/NLCDcl.tif",
      nlcdBuf = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/nlcdMask.tif",
#      hasPJ = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/espHasPJ.tif" 
   ),

    # variables for distance matrix
    topoVars = list(
        ELEVm = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/ELEVm.tif",
        PCURV = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/PCURV.tif",
        TCURV = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/TCURV.tif",
        RELHT1 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT1.tif",
        RELHT32 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT32.tif",
        RELHT128 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT128.tif",
        RELMNHT1 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT1.tif",
        RELMNHT32 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT32.tif",
        RELMNHT128 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT128.tif",
        MRRTF ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/MRRTF.tif",
        MRVBF ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/MRVBF.tif",
        SLOPE ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/SLOPE.tif",
        SOUTHNESS ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/SOUTHNESS.tif",
        EASTNESS ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/EASTNESS.tif",
        TWI_TOPMODEL="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/TWI_TOPMODEL.tif",
        CAlog_10="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/CAlog_10.tif",
        LFELEMS="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/LFELEMS.tif"
    ),


    ## response variables
    respVars = list(
     iqr = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/EE/satvi_iqr_MarNov/iqr.vrt",
     med = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/EE/satvi-median-MarNov/median.vrt",
     bare = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/rap/bare_vrt/rapBare.vrt"
    )

    )

)
