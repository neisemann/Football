library(R.utils)
library(dplyr)
library(XML)
library(maps)
library(RColorBrewer)
library(shiny)
library(mosaic)
source("TwoTeamSimulation.R")

shinyServer(function(input,output){
  output$TwoTeamSimulation_1 <- renderPlot({
  TwoTeamSimulation(team.idA = input$select_box_NFL_A, team.idB = input$select_box_NFL_B, num.games = input$num_sims, punt.yard = input$punt_yard_line,
                    fourth.d.yard = input$fourth_yard_line, FG.min = input$FG_min_yard_line, FG.max = input$FG_max_yard_line)
          })
})
  