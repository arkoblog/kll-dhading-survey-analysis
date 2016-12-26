library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(plyr)
library(stringr)
library(sp)


server <- function(input, output, session) {
  
  ddng_poly<-readRDS("data/ddng_poly.Rds")
  ddng_sch<-readRDS("data/ddng_sch.Rds")

  dis_buckets<-levels(ddng_sch$approx_dist_to_road)
  max_elev<-max(ddng_sch$elev)
  min_elev<-min(ddng_sch$elev)
  
  output$elev_slider<-renderUI({
    sliderInput("number", label = "Select elevation range (in meters)", min=0, max=max_elev, value=c(0,max_elev))
  })
  
  output$dist_to_road<-renderUI({
    checkboxGroupInput("selDistBucket", selected=dis_buckets,label="Select approximate distance to nearest road", choices = c("Less than 500m","Between 500m to 1 Km","Between 1 Km to 3 Km", "Between 3 Km to 5 Km","Between 5 Km to 10 Km", "More than 10 Km"))
  })
  
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
  
  pct_students<- reactive({
    round((total_students()/sum_students*100), digits=2)
  })
  
  schools_dist<-reactive({
    aggregate(school_name ~ max_ed, filteredData(), length)
  })
  
  output$tot_sch<-renderText({
    paste(total_schools(),"schools selected")
  })

  output$tot_stu<-renderText({
    paste0(total_students()," (",pct_students(),"%)")
  })
  
  output$plot1<-renderPlot({
    y<-schools_dist()
    
    y$max_ed<-str_wrap(y$max_ed, width=10)
    ggplot(y, aes(max_ed, school_name)) +
      xlab("") + ylab("Number of Schools") + xlab("Maximum level of education \n available")+ geom_bar(stat = "identity", fill="steelblue") +geom_text(aes(label=str_wrap(school_name, width=10)), vjust=-0.5) + ylim(0, 400)
  })

  output$mymap <- renderLeaflet({
    pal <- colorBin("Blues", ddng_poly$tot_sch,5)
    points %>% 
      leaflet() %>% addTiles() %>% 
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addLegend("topleft",pal = pal,values = ddng_poly$tot_sch,title = "Number of schools <br/> in each VDC  <br/> <h4>Note: The number of schools  <br/> (and color of map) are shown <br/> at an overall  level for each <br/>VDC, and do not change <br/>based on filter selection.</h4>",opacity = 1)

  })
  
  
  
  observe({
    pal <- colorBin("Blues", NULL, 5)
    
    leafletProxy("mymap", data = filteredData()) %>% 
      clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% 
      addMarkers(popup=~paste("<b>",school_name,"</b>","<br/>",vdc_label,"VDC <br/>","Elevation: ",elev,"meters <br/> Number of Students:", no_students_total ),clusterOptions=markerClusterOptions()) %>% 
      addPolygons(weight = 1, fillOpacity = 0.8, data=ddng_poly, fillColor = ~pal(tot_sch))  
    
  })
  
}

