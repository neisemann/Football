library(R.utils)
library(dplyr)
library(XML)
library(maps)
library(RColorBrewer)
library(shiny)
source("world_bubble_plot3.R")
source("Describe.R")

shinyServer(function(input,output){
  output$map_it1 <- renderPlot({
  map_it(fieldID = input$select_box_CIA, User_Define_Color = input$color_CIA)
          })
  output$Describe1 <- renderText({
    Describe(input$fieldID)  
  })
})
  