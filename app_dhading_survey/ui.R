library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(plyr)
library(stringr)
library(sp)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%; font-size:10px} h1,h2,h3 {padding:0;margin-top:0; margin-bottom:5px}; h5{padding:0;margin:0;font-weight:bold;font-size:10px} h4{padding:0;margin:0;font-size:10px}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 20, right = 20, width = 300, draggable = TRUE,
                wellPanel(h5("Number of students:"),
                          h2(textOutput("tot_stu")),    
                          (textOutput("tot_sch")),
                          uiOutput("elev_slider"),
                          uiOutput("dist_to_road"),
                          plotOutput("plot1",height = "210px")
                          ,style="opacity:0.9")
  )
)