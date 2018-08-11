#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr","data.table","rhandsontable","DT")
if (!require("googlesheets4")) devtools::install_github("tidyverse/googlesheets4")
library(dplyr)
library(data.table)
library(rhandsontable)
library(DT)
library(googlesheets4)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title="Promo Record Keeper",
  fluid = TRUE,
  tabPanel(title="Rugby League",
    fluidRow(
      column(width=6,
             h3("Calculator"),
             rHandsontableOutput("hot"),
             actionButton("btn","Update Stakes"),
             textOutput("Text"),
             br(),
             h3("Record Keeper"),
             rHandsontableOutput("record"),
             actionButton("recUpd","Update Returns"),
             actionButton("log","Log Bets"),
             tags$a(href = "https://docs.google.com/spreadsheets/d/1b6EMQRD55O9755rw2LhV1Vif4kqWv-GUXgQSJyQb4uI/",
                    "Click here to find the spreadsheet")
      )
    )
  ),
tabPanel(
  title = "Premier League",
  fluidRow(
    column(
      width = 6,
      h3("Calculator"),
      rHandsontableOutput("plCalc"),
      actionButton("plBtn", "Update Stakes"),
      textOutput("plText"),
      br(),
      h3("Record Keeper"),
      rHandsontableOutput("plRecord"),
      actionButton("plRecUpd", "Update Returns"),
      actionButton("plLog", "Log Bets"),
      tags$a(
        href = "https://docs.google.com/spreadsheets/d/1b6EMQRD55O9755rw2LhV1Vif4kqWv-GUXgQSJyQb4uI/",
        "Click here to find the spreadsheet"
      )
    )
  )
)
  )
)
