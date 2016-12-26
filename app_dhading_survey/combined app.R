library(shiny)
library(leaflet)
library(RColorBrewer)
library(plyr)
library(sp)

ui <- bootstrapPage(title="KLL-Dhading Schools Survey",
                    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-size:10px} h1,h2,h3 {padding:0;margin-top:0; margin-bottom:5px} h5{padding:0;margin:0;font-weight:bold;font-size:10px}"),
                    leafletOutput("mymap", width = "100%", height = "100%"),
                    absolutePanel(bottom = 20, right = 20, width = 300, draggable = TRUE,
                                  wellPanel(h5("Number of students:"),
                                            h2(textOutput("tot_stu")),    
                                            (textOutput("tot_sch")),
                                            uiOutput("elev_slider"),
                                            uiOutput("dist_to_road")
                                            ,style="opacity:0.72")
                    )
)

server <- function(input, output, session) {
  
  ddng_poly<-readRDS("data/ddng_poly.RDS")
  ddng_sch<-readRDS("data/ddng_sch.RDS")
  
  ddng_sch$approx_dist_to_road<-revalue(ddng_sch$approx_dist_to_road,c("betn_1_3km"="Between 1 Km to 3 Km", "betn_3_5km"="Between 3 Km to 5 Km","betn_500m_1km"="Between 500m to 1 Km", "betn_5_10km"="Between 5 Km to 10 Km", "less_than_500m"="Less than 500m", "more_than_10km"="More than 10 Km"))
  
  dis_buckets<-levels(ddng_sch$approx_dist_to_road)
  max_elev<-max(ddng_sch$elev)
  min_elev<-min(ddng_sch$elev)
  
  
  points<-ddng_sch[which(ddng_sch$no_students_total>100),]
  sum_students<-sum(ddng_sch$no_students_total,na.rm = T)
  filteredData<-reactive({
    len<-length(input$selDistBucket)
    ddng_sch[which(ddng_sch$elev>input$number[1] & ddng_sch$elev<=input$number[2] & ddng_sch$approx_dist_to_road%in%input$selDistBucket),] 
  })
  
  total_students<- reactive({
    sum(filteredData()$no_students_total, na.rm = T)
  })
  
  total_schools<- reactive({
    length(filteredData())
  })
  
  output$elev_slider<-renderUI({
    sliderInput("number", label = "Select elevation range", min=0, max=max_elev, value=c(0,max_elev))
  })
  
  output$dist_to_road<-renderUI({
    checkboxGroupInput("selDistBucket", selected=dis_buckets,label="Select approximate distance to nearest road", choices = c("Less than 500m","Between 500m to 1 Km","Between 1 Km to 3 Km", "Between 3 Km to 5 Km","Between 5 Km to 10 Km", "More than 10 Km"))
  })
  
  
  pct_students<- reactive({
    round((total_students()/sum_students*100), digits=2)
  })
  
  output$tot_sch<-renderText({
    paste(total_schools(),"schools selected")
  })
  
  
  output$tot_stu<-renderText({
    paste0(total_students()," (",pct_students(),"%)")
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

shinyApp(ui,server)
