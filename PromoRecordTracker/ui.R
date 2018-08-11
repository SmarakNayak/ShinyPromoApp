#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title="Promo Tracker",
  fluid = TRUE,
  tabPanel(title="Rugby League",
    fluidRow(
      column(width=6, 
             rHandsontableOutput("hot"),
             textOutput("Text")
             ),
      column(width=6, dataTableOutput("DT"))
    )
  ),
  tabPanel(title="Premier League",
           h3("Come back later.."))
))
