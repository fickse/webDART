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

cat('starting\n')
library(shiny)
#library(shinyjs)
library(leaflet)
library(raster)
library(data.table)
library(visNetwork)

# Load parameters object ("dpar") with path names and defaults from file
#source('params.R') 

# get functions
source('functions.R')
source('params.R')

#d <- getDart( -110.22, 38.478)
load('exampleDartRun.RData')
load('ucrb_outline.RData')
cat('loaded\n')
ss <- spTransform(ss, projection(raster()))
print(ss)

mapTilesUrlTemplate = '//server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
x = -110.22
y = 38.478
buffer = 60
searchRadius = 3000
nControl = 100
targetRadius = 30
debugMessage = TRUE


ui <- tagList(

  # adjust menu based on screen size
  tags$head(tags$style(HTML("

      @media screen and (max-width: 500px) {
        #demox {
          width: 60%;
        }

      }
      "))),
  
  navbarPage('webDART', id = 'nav', inverse = TRUE, collapsible = FALSE,position = 'fixed-bottom',

             
  tabPanel("Interactive Map",
   div(class="outer",  style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0;",value = 'mapPanel',
    #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    
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
      tags$div(id='demox', class="collapse", style = "overflow-y:scroll; max-height: 500px",

          wellPanel(

          actionButton("runDart", "Run Dart"),# actionButton('analyze', 'Analyze'),
          sliderInput("targetRadius", "Target Area Radius (m)", min = 0, max = 300, step = 30, value = 30), 
          sliderInput("searchRadius", "Search Area Radius (m)", min = 500, max = 6000, step = 500, value = 3000),
          sliderInput("buffer", "Buffer Around Target Area (m)", min = 0, max = 300, step = 30, value = 60),
          sliderInput("nControl", "Number of Reference Pixels to Return", min = 1, max = 300, step = 1, value = 100),
          sliderInput("dDate", "Year of First Disturbance (For Synthetic Control)", sep = '', min = 1991, max = 2018, step = 1, value = 1991)
          
          )
       )
      ),

 
     ###############
     # Plot Panel  #
     ###############
     # conditionalPanel(style = "opacity: 0.92 bottom:0, right:'auto' width:'90%' height:300", 
                    # condition = "input.analyze!==0",
                    # icon('hand-point-down', class = "fa-9x", lib = "font-awesome"),
                    # tags$h3("Plots"),
                    # plotOutput("myplot", click = 'plot_click', height = 400, width = 900)
                    # ),
    
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
#   )
  ),
    

    ###############
    # Plot Panel  #
    ###############
    
  tabPanel("Analysis", value = 'analysisPanel',
    #div(class="outer",  style = "position: relative; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0;",
     tabsetPanel(
      tabPanel("Quantiles",plotOutput("myplot", height = 'auto')), 
      tabPanel("Synthetic Control", plotOutput("scPlot")),
      tabPanel("Variable Importance", visNetworkOutput('cartPlot', height = 800))
      #downloadButton("exportPDF", "Export Figures")
     )
  ),


    #################
    # Export Panel  #
    #################

  tabPanel("Export", value = 'exportPanel',
    div(class="outer",  style = "position:inherit; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0;",
      sidebarPanel(
        #selectInput('whatToExport', "What to Export:", choices = c('Export Dart Pixels (xlsx)', 'Export Report (pdf)')),
        downloadButton("exportPDF", "Export Report (pdf)"),
        downloadButton("exportXLS", "Export Dart Pixels (xlsx)")#,
      )
    )
  )
    
)

)

cols <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

server <- function(input, output, session) {
  
  ##########################################################################
  ##########################################################################
  ###  Dynamic Objects          
  ##########################################################################
  ##########################################################################
  v <- reactiveValues( msg = getwd(), msg2 = 'place holder', lat = 38.478, lng = -110.22, 
                       dartResult = d, dartParams = list(), analysisDirections = "Select 'Run Dart' then 'Analyze' on the Interactive Map Menu to see plots"
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
            showModal(modalDialog("Running Dart", footer=NULL))
            attempt <- try({ getDart( x = v$lng, y = v$lat, buffer = input$buffer,
                               searchRadius = input$searchRadius,
                               targetRadius = input$targetRadius,
                               nControl = input$nControl) })
            
            removeModal()
            if(class(attempt) == 'try-error'){

               showNotification(
                             attempt,duration = NULL, type = 'error'
                             )
               dartResult <- v$dartResult
            } else {

                dartResult <- attempt

            }
                              
      #dartResult <- v$dartResult
      v$dartResult <- dartResult
      
      #cashe params
      v$dartParams <- list( lng = v$lng, lat = v$lat, buffer = input$buffer, 
                            searchRadius = input$searchRadius, targetRadius = input$targetRadius, 
                            ncontrol = input$nControl, synthControlStartYear = input$dDate)

      
      # extract timeseries for targets + references
      v$extraction <- extractDart( v$dartResult, 'SATVI' )
      v$synthControl <- synthCtrl( v$extraction, input$dDate, 'SATVI' )
      
      ## Timeseries plots
      output$myplot <- renderPlot( ts_plot(v$extraction, 'SATVI'),
                                        height = function() {
                                            #https://github.com/rstudio/shiny/issues/650
                                            pmin(session$clientData$output_myplot_width *.6, 800)
                                        }
                                  )
      
      ## Synthetic Control Plots
      output$scPlot <- renderPlot( plot(v$synthControl), height = function() {
                                            #https://github.com/rstudio/shiny/issues/650
                                            pmin(session$clientData$output_scPlot_width *.6, 800)
                                        } )
      
      ## Variable importance Plots
      output$cartPlot <- renderVisNetwork( viewImportance(v$dartResult))
      
      
      showModal(modalDialog("Analysis Completed", footer= tagList(modalButton("OK"))))
      
      chosen <- raster(dartResult$chosenPixels['distance'])
      targetRast <- raster(dartResult$targetRast['refrast'])
      cz <- colorNumeric(cols, values(chosen), na.color = "transparent")
      
      # plot results on map
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
  ## Toggle Menu                      #
  #####################################

   observeEvent(input$Menu, {
       shinyjs::toggle(id = "Sidebar")
   })
    
    
  ######################################
  ## What to do when "Analyze" clicked #
  ######################################
  
  # observeEvent(input$analyze,
  # {
     # strt <- format(Sys.time(), '%H:%M:%S')
     # msg <- paste0("Extracting response variables -- this may take a while -- 
                   # started at ",strt)
     # showModal(modalDialog(msg, footer=NULL))
       
     # extract timeseries for targets + references
     # v$extraction <- extractDart( v$dartResult, 'SATVI' )
     # v$synthControl <- synthCtrl( v$extraction, input$dDate, 'SATVI' )
     
     # removeModal()

     # generate a timeseries plot
     
        # output$myplot <- renderPlot( ts_plot(v$extraction, 'SATVI') )
        # output$scPlot <- renderPlot( plot(v$synthControl) )
     
     # showModal(modalDialog("Analysis Finished", footer= tagList( actionButton('goto', "Go To"), modalButton("Dismiss"))))

     # go to plot
     # v$analysisDirections = ''
   # })

  ##################################################
  # When analysis finished and user clicks 'go to' #
  ##################################################

    observeEvent(input$goto, {
     updateTabsetPanel(session, "nav", selected = "analysisPanel")
    })
     
   
  #####################################
  ## What to do when 'Export' clicked #
  #####################################
  
  # Downloadable xlsx of selected dataset ----
  
  output$exportPDF <- downloadHandler(

    filename = function() {
    
      f <- paste('DART_', round(v$dartParams$lng,3), '_', round(v$dartParams$lat,3),'_', format(Sys.time(), '%Y-%m-%d-%H-%M'), ".pdf", sep = "")
     
    },
    content = function(file) {
      
         searchPoly <- project( getTarget( v$dartParams$lng, v$dartParams$lat, 
                             v$dartParams$searchRadius + v$dartParams$targetRadius))
      
         dartPoly <- project( getTarget ( v$dartParams$lng, v$dartParams$lat, v$dartParams$targetRadius) )
      
         bufferPoly <- project( getTarget ( v$dartParams$lng, v$dartParams$lat, v$dartParams$buffer + v$dartParams$targetRadius))
         chosen <- raster(v$dartResult$chosenPixels['distance'])
          targetRast <- raster(v$dartResult$targetRast['refrast'])
          cz <- colorNumeric(cols, values(chosen), na.color = "transparent")
           
         
         
         params = list(
            dartParams = v$dartParams,
            bigMap = leaflet() %>%
                     addTiles(urlTemplate = mapTilesUrlTemplate) %>%
                     setView(lng = -110.22, lat = 38, zoom = 6) %>%
                     addMarkers(lng = v$dartParams$lng, lat = v$dartParams$lat) %>%
                     addPolygons(data = ss, fillColor='transparent', dashArray = '10',col = 'white', group = "Polygons"),
            smallMap = leaflet() %>%
                     addTiles(urlTemplate = mapTilesUrlTemplate) %>%
                     addPolygons(data = dartPoly, fillColor='transparent', group = "Polygons")%>%
                     addPolygons(data = searchPoly, color = 'red', fillColor='transparent', group = "Polygons")%>%
                     addPolygons(data = bufferPoly, color = 'red', dashArray = '10',fillColor='transparent', group = "Polygons") %>%
                     setView(lng = v$dartParams$lng, lat = v$dartParams$lat, zoom = 14),
            dartMap = leaflet() %>%
                     addTiles(urlTemplate = mapTilesUrlTemplate) %>%
                     addRasterImage(v$dartResult$edaphicRaster, col = 'green', opacity = 0.25, group = 'Edaphic Subset')%>%
                     addRasterImage(targetRast, col = 'purple', opacity = 1, group = 'Target Pixels')%>%
                      addRasterImage(chosen, colors = cz, opacity = 1, group = 'Topo-edaphic Matches')%>%
                     setView(lng = v$dartParams$lng, lat = v$dartParams$lat, zoom = 14),
         
            extraction = v$extraction,
            dartOutput = v$dartResult,
            sc = v$synthControl
         )
         
         generateRmdReport(file, params)
      }
    
  )
  


output$exportXLS <- downloadHandler(

    filename = function() {
    
      f <- paste('DART_', round(v$dartParams$lng,3), '_', round(v$dartParams$lat,3),'_', format(Sys.time(), '%Y-%m-%d-%H-%M'), ".xlsx", sep = "")
     
    },
    
    content = function(file) {  
  
  
         writexl::write_xlsx(datasetInput(), path = file)
      
    }
  )
  
  # Data prepper
  datasetInput <- reactive({

    # Dart pixels
    ref <- as.data.frame(project(v$dartResult$chosenPixels))
    target <- as.data.frame(project(v$dartResult$targetRast))
    ref$type <- 'dartReference'
    target$type <- 'target'

    out <- data.table::rbindlist(list(target, ref), fill = TRUE)
    out <- out[ , c( 'type', 'x', 'y', 'freq', 'distance', 'soilps', 'soilec', names(dpar$topoVars)), with =F ]

    setnames(out, 'freq', 'freqSelected')
    setnames(out, 'distance', 'avgTopoDist')
    setnames(out, 'soilps', 'soilParticleSizeClass')
    
    # Metadata
    dp <- v$dartParams
    kv <-   c(dartVersion = '2.0', lat = dp$lat, lng = dp$lng, date = format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 
                        targetRadius = dp$targetRadius,
                        searchRadius = dp$searchRadius,
                        buffer = dp$buffer,
                        nControl = dp$nControl)
    meta <- data.frame(key = names(kv), value = kv)
    
    # Quantiles
    quants <- getQuantiles(v$extraction, 'SATVI')
    
    list( metadata = meta, DartPixels = as.data.frame(out), quantiles = quants)
    
  })

   
  ##########################################################################
  ##########################################################################
  ###  Webpage Elements              
  ##########################################################################
  ##########################################################################
  
  
  # Background Map
  output$mymap <- renderLeaflet({
      leaflet() %>%
      addTiles(urlTemplate = mapTilesUrlTemplate) %>%
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

  output$analysisDirections <- renderPrint({
        print(v$analysisDirections)
  })
    
}


shinyApp(ui = ui, server = server)
