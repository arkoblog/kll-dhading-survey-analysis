library(shiny)
library(leaflet)
library(RColorBrewer)

ui3 <- fluidPage(
  
  titlePanel("Elevation"),
  
  fluidRow(
    
    column(4,
           wellPanel(
             uiOutput("elev_slider"),
             br(),
             h3(textOutput("tot_stu")),
             br(),
             textOutput("pct_of_total")
           )       
    ),
    
    column(8,
           leafletOutput("mymap", width = "100%", height = "100%")
    )
  )
)
server3 <- function(input, output, session) {
  
  ddng_poly<-readRDS("data/ddng_poly.Rds")
  ddng_sch<-readRDS("data/ddng_sch.Rds")
  
  max_elev<-max(ddng_sch$elev)
  min_elev<-min(ddng_sch$elev)
  
  output$elev_slider<-renderUI({
    sliderInput("number", label = "Please select the elevation", min=0, max=max_elev, value=c(100,1000))
  })
  
  points<-ddng_sch[which(ddng_sch$no_students_total>100),]
  sum_students<-sum(ddng_sch$no_students_total,na.rm = T)
  filteredData<-reactive({
    ddng_sch[which(ddng_sch$elev>input$number),] 
  })
  
  
  
  total_students<- reactive({
      sum(filteredData()$no_students_total, na.rm = T)
  })

  pct_students<- reactive({
    round((total_students()/sum_students*100), digits=2)
  })
  
  output$tot_stu<-renderText({
    paste("Total number of students:", total_students())
  })
  
  output$pct_of_total<-renderText({
    paste(pct_students(),"% of total population")
  })
  
  output$mymap <- renderLeaflet({
    points %>% leaflet() %>% addTiles() %>% fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  observe({
    leafletProxy("mymap", data = filteredData()) %>% 
      clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% 
      addMarkers(popup=~school_name,clusterOptions=markerClusterOptions()) %>% 
      addPolygons(fillOpacity = .5,color = "steelblue",weight = 0.5, data=ddng_poly) 
    
  })
  
}


shinyApp(ui3, server3)
