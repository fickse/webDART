## Disturbance Automated Reference Toolset Web Version

  Quick Start:
  
    library(shiny)
    runApp('app')

  Basic Usage:
  
    User specifies coordinates of target area by clicking
    User specifies key Dart Parameters ( menu )
        Target Area Radius (m) : Size of target area  
        Buffer Around Target Area (m) : How much to mask outside edge of target
        Search Area Radius (m) : How far out to look for candidate reference areas
        N control points : How many Dart reference pixels to return
    
    Results:
      Visualization of target pixels, search radius, Gower distances
      Export data frame of DART Pixels and target Pixels
      (tbd) Timeseries of target pixels vs. controls
      (tbd) Stats about similarity


