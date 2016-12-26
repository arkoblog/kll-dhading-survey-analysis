library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Dhading Survey"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("elev_slider")
      ),
    mainPanel(
      p(),
      leafletOutput("mymap")
      
    )
  )
 
)

server <- function(input, output, session) {
  
  ddng_poly<-readRDS("data/ddng_poly.Rds")
  ddng_sch<-readRDS("data/ddng_sch.Rds")
  
  max_elev<-max(ddng_sch$elev)
  min_elev<-min(ddng_sch$elev)
  
  output$elev_slider<-renderUI({
    sliderInput("number", label = "Please select the elevation", min=0, max=max_elev, value=100)
  })
  
  points<-ddng_sch[which(ddng_sch$no_students_total>100),]
  
  filteredData<-reactive({
    ddng_sch[which(ddng_sch$elev>input$number),] 
  })

    
  output$mymap <- renderLeaflet({
    points %>% leaflet() %>% addTiles() %>% fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })

  observe({
    
    leafletProxy("mymap", data = filteredData()) %>% clearShapes() %>% clearMarkers() %>% clearMarkerClusters()%>% addPolygons(data=ddng_poly)%>% addMarkers(clusterOptions=markerClusterOptions())
      
      })
  
}

shinyApp(ui, server)

