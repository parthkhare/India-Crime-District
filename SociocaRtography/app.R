# Prilimnaries
# ====================================================================================
rm(list=ls())
gc(verbose=T)
gcinfo(FALSE)
#setwd("/Users/parthkhare/Work/Data Mining/CrimeMapsv2.0/Sessions/SociocaRtography")
#load('IPC_1.RData')
load('IPC_2.RData')   # capped scaled data with addnl treatment for 2013
#load('IPC_3.RData')   # non capped/scaled data only for 2001:2012
# ====================================================================================

# 
# library(shiny)
# runApp(paste0(sys,"CrimeMapsv2.0/Codes/app.R"))

# Libraries
# ====================================================================================
library(maps)
library(mapproj)
library(sp)
library(maptools)
library(raster)
library(leaflet)
library(shiny)
library(ggplot2)
library(shinyBS)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(colorRamps)
library(colorspace)
# ====================================================================================

# Data Mods
# ====================================================================================
locs <- locx
dcd <- decades[1:12]
y <- x[[1:12]]
class(y)
nlayers(y)
decades <- dcd
x <- y

# ====================================================================================
ui <- bootstrapPage(
  h1("India Crime Map by Districts [NCRB]"),
  p(code("Spatial Distribution of Total IPC Crimes from 2001:2012")),
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("Map", width="100%", height="100%"),
  absolutePanel(top=10, right=10,
                sliderInput("dec", "Years", min=min(decades), max=max(decades), 
                            value=decades[1], step=1, sep="", post="'s"), # NEW LINE
                checkboxInput("show_communities", "Show State Centers", FALSE),
                checkboxInput("legend", "Show legend", TRUE), # NEW LINE
                conditionalPanel("input.show_communities == true",
                                 selectInput("location", "State", c("", locs$loc), selected=""),
                                 conditionalPanel("input.location !== null && input.location !== ''",
                                                  actionButton("button_plot_and_table", "View Plot/Table", class="btn-block"))
                )
  ),
  bsModal("Plot_and_table", "Plot and Table", "button_plot_and_table", size = "large",
          plotOutput("TestPlot"),
          dataTableOutput("TestTable")
  )
)
# ====================================================================================



# ====================================================================================
server <- function(input, output, session) {
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=2, color="blue", 
                                                       fillColor="orange", fillOpacity=1, 
                                                       opacity=1, weight=2, stroke=TRUE, 
                                                       layerId="Selected")
  
  ras <- reactive({ subset(x, which(decades==input$dec)) })
  ras_vals <- reactive({ values(ras()) })
#   pal <- reactive({ colorNumeric(rgb.tables(4), ras_vals(),
#                                  na.color="transparent") })
  pal <- reactive({ colorNumeric(brewer.pal(10, "YlOrRd"), ras_vals(),
                                  na.color="transparent") })
   
  # BaseMap II
  output$Map <- renderLeaflet({
    leaflet() %>% setView(lon, lat, 5) %>% addProviderTiles('CartoDB.DarkMatter',group='CartoDB-3') %>% 
      addCircleMarkers(data=locs, radius=2, color="greens", stroke=FALSE, 
                       fillOpacity=0.5, group="locations", layerId = ~loc)
  })
  
  observe({
    proxy <- leafletProxy("Map")
    proxy %>% removeTiles(layerId="rasimg") %>% addRasterImage(ras(), colors=pal(),
                                                               opacity=0.6, 
                                                               layerId="rasimg")
  })
  
  observe({
    proxy <- leafletProxy("Map")
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position="bottomright", pal=pal(), values=ras_vals(), 
                          title=" Reported Crime Incidents")
    }
  })
  
  observe({ # show or hide location markers
    proxy <- leafletProxy("Map")
    if (input$show_communities) {
      proxy %>% showGroup("locations")
    } else {
      updateSelectInput(session, "location", selected="")
      proxy %>% hideGroup("locations") %>% removeMarker(layerId="Selected")
    }
  })
  
  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  observeEvent(input$Map_marker_click, { # update the location selectInput on map clicks
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })
  
  observeEvent(input$location, { # update the map markers and view on location selectInput changes
    p <- input$Map_marker_click
    p2 <- subset(locs, loc==input$location)
    proxy <- leafletProxy("Map")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
    } else if(length(p$id) && input$location!=p$id){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    } else if(!length(p$id)){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    }
  })
  
  # @knitr server03pointdata
  Data <- reactive({ d %>% filter(Location==input$location) })
  output$TestPlot <- renderPlot({ ggplot(Data(), aes(value, Year)) + geom_line() + geom_smooth() })
  output$TestTable <- renderDataTable({
    Data()
  }, options = list(pageLength=5))
  # @knitr server03remainder
}

shinyApp(ui, server)
# ====================================================================================
