######################
# Dart Web Shiny App #
######################

# Steve Fick
# 12/31/2019

"
  Basic Usage:
  
    User shown a map
    User specifies coordinates (click)
    User specifies key Dart Parameters
        Area to average over
        Buffer size
        Search radius
        N control points
    
    Results:
      Visualization of target pixels, search radius, Gower distances
      Timeseries of target pixels vs. controls
      Stats about similarity
"


library(shiny)
library(leaflet)
library(raster)

# Load parameters object ("dpar") with path names and defaults from file
#source('params.R') 

# get functions
source('functions.R')

#d <- getDart( -110.22, 38.478)
load('exampleDartRun.RData')
load('ucrb_outline.RData')
cat('loaded\n')
ss <- spTransform(ss, projection(raster()))
print(ss)

x = -110.22
y = 38.478
buffer = 60
searchRadius = 3000
nControl = 100
targetRadius = 30
debugMessage = TRUE

ui <- bootstrapPage(
    
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    

    ################
    # Map          #
    ################
    leafletOutput("mymap", width = "100%", height = "100%"),
    
    ################
    # Menu         #
    ################
    absolutePanel( 
      style = "opacity: 0.92", draggable= TRUE, top = 10, left = 50,  width = 500,
      HTML('<button data-toggle="collapse" data-target="#demox">Menu</button>'),
      tags$div(id='demox', class="collapse", 
          #verbatimTextOutput('mymenu'),
          wellPanel(
          actionButton("runDart", "Run Dart"), downloadButton("export", "Export"),
          sliderInput("targetRadius", "Target Area Radius (m)", min = 0, max = 300, step = 30, value = 30), 
          sliderInput("searchRadius", "Search Area Radius (m)", min = 500, max = 6000, step = 500, value = 3000),
          sliderInput("buffer", "Buffer Around Target Area (m)", min = 0, max = 300, step = 30, value = 60),
          sliderInput("nControl", "Number of Reference Pixels to Return", min = 1, max = 300, step = 1, value = 100)
          )
       )
      ),
        
    ################
    # Debug window #
    ################
    if(debugMessage){  
      # Messages
      absolutePanel( 
        style = "opacity: 0.92", draggable= TRUE, bottom = 10, right = 50,  width = 150,
        HTML('<button data-toggle="collapse" data-target="#demoz">Message</button>'),
        tags$div(id='demoz', class="collapse" ,
              verbatimTextOutput('mymessage')
        )
      )
    }
    
)



cols <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

server <- function(input, output, session) {
  
  ##########################################################################
  ##########################################################################
  ###  Dynamic Objects          
  ##########################################################################
  ##########################################################################
  v <- reactiveValues( msg = getwd(), msg2 = 'place holder', lat = 38.478, lng = -110.22, 
                       dartResult = d
                       )
  
  
  ##########################################################################
  ##########################################################################
  ###  Reactive Events              
  ##########################################################################
  ##########################################################################
  
  ################################
  ## What to do when map clicked #
  ################################
    # log coordinates of click
    observeEvent(input$mymap_click,{ v$msg <- input$mymap_click
                                   v$lng <- input$mymap_click$lng
                                   v$lat <- input$mymap_click$lat
                                   } )
  
    # re-draw the markers plus search radius
    observe({
      
      searchPoly <- project( getTarget( v$lng, v$lat, 
                             input$searchRadius + input$targetRadius))
      
      dartPoly <- project( getTarget ( v$lng, v$lat, input$targetRadius) )
      
      bufferPoly <- project( getTarget ( v$lng, v$lat, input$buffer + input$targetRadius))
      
      leafletProxy('mymap') %>%
          clearMarkers() %>%
          addMarkers(lng = v$lng, lat = v$lat) %>%
          clearShapes() %>%
          addPolygons(data = dartPoly, fillColor='transparent', group = "Polygons")%>%
          addPolygons(data = searchPoly, color = 'red', fillColor='transparent', group = "Polygons")%>%
          addPolygons(data = bufferPoly, color = 'red', dashArray = '10',fillColor='transparent', group = "Polygons")%>%
          addPolygons(data = ss, fillColor='transparent', group = "Polygons", dashArray = '10', col = 'white')
   })

   #####################################
   ## What to do when Run Dart clicked #
   #####################################
   
   # pull dart results
   observeEvent(input$runDart,
      {
        
       # dartResult <- getDart( x = v$lng, y = v$lat, buffer = input$buffer,
                              # searchRadius = input$searchRadius,
                              # targetRadius = input$targetRadius,
                              # nControl = input$nControl)
                              
      dartResult <- v$dartResult
      chosen <- raster(dartResult$chosenPixels['distance'])
      targetRast <- raster(dartResult$targetRast['refrast'])
      cz <- colorNumeric(cols, values(chosen), na.color = "transparent")
      
      # plot results
      leafletProxy('mymap') %>%
          clearImages() %>%
          addRasterImage(dartResult$candidates, col = 'blue', opacity = 0.15, group = 'Candidates')%>%
          addRasterImage(dartResult$edaphicRaster, col = 'green', opacity = 0.25, group = 'Edaphic Subset')%>%
          addRasterImage(targetRast, col = 'purple', opacity = 1, group = 'Target Pixels')%>%
          addRasterImage(chosen, colors = cz, opacity = 1, group = 'Topo-edaphic Matches')%>%
          clearControls() %>%
          addLayersControl(overlayGroups = c("Candidates", "Edaphic Subset", "Target Pixels", "Topo-edaphic Matches")) %>%
          addLegend( values = values(chosen), pal = cz ,title = "Topo-distance", position = 'bottomright')
      
      }
   )
   
  #####################################
  ## What to do when 'Export' clicked #
  #####################################
  
  # Downloadable csv of selected dataset ----
   output$export <- downloadHandler(
    filename = function() {
      paste('DART_', round(v$lng,3), '_', round(v$lat,3),'_', format(Sys.time(), '%Y-%m-%d-%H-%M'), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # Data prepper
  datasetInput <- reactive({
  
    ref <- as.data.frame(project(v$dartResult$chosenPixels))
    target <- as.data.frame(project(v$dartResult$targetRast))
    ref$type <- 'dartReference'
    target$type <- 'target'
    
    meta <- jsonlite::toJSON( list(  lat = v$lat, lng = v$lng, date = Sys.time(), 
                        targetRadius = input$targetRadius,
                        searchRadius = input$searchRadius,
                        buffer = input$buffer,
                        nControl = input$nControl) , auto_unbox = TRUE)
    
    metadata <- target[1,]
    metadata$type <- 'metadata'
    metadata$x  <- metadata$y <- metadata$freq <- metadata$distance <- metadata$soilps <- NA
    metadata$soilec <- meta
    
    
    out <- data.table::rbindlist(list(metadata, target, ref), fill = TRUE)
    out <- out[ , list( type, x, y, freq, distance, soilps, soilec)]
    
    setnames(out, 'freq', 'freqSelected')
    setnames(out, 'distance', 'avgTopoDist')
    out
    
  })

   
  ##########################################################################
  ##########################################################################
  ###  Webpage Elements              
  ##########################################################################
  ##########################################################################
  
  
  # Background Map
  output$mymap <- renderLeaflet({
      leaflet() %>%
      addTiles(urlTemplate = '//server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      setView(lng = -110.22, lat = 38, zoom = 6) %>%
      clearShapes() %>%
      addPolygons(data = ss, fillColor='transparent', dashArray = '10',col = 'white', group = "Polygons")
      
  })
  
  # Menu
  output$mymenu <- renderPrint({
        print(v$msg2)
  })
  
  # Debug Message 
  output$mymessage <- renderPrint({
        print(v$msg)
  })
  
  
}


shinyApp(ui = ui, server = server)
