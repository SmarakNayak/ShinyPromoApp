#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr","data.table","rhandsontable","DT","devtools")
if (!require("googlesheets4")) devtools::install_github("tidyverse/googlesheets4")
library(dplyr)
library(data.table)
library(rhandsontable)
library(DT)
library(shinyTable)
library(googlesheets4)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  df=data.table(Team=c("Team A","Team B"),Odds=c(2.0,2.0),Stake = c(0,0),Return=c(0,0),stringsAsFactors = FALSE)
  df2=data.table(Team=c("Team A","Team A","Team B"),Odds=c(2.0,2.0,2.0),Stake = c(0,0,0),Return=c(0,0,0),
                 Person=c("Danny","Miltu","Vikash"),Outcome=c("","",""),stringsAsFactors = FALSE)
  ###init
  output$hot <- renderRHandsontable(
    rhandsontable(df) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
      hot_col("Stake", readOnly = TRUE) %>%
      hot_col("Return", readOnly = TRUE)
  )
  output$record = renderRHandsontable(
    rhandsontable(df2) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
      hot_col("Stake", readOnly = TRUE) %>%
      hot_col("Return", readOnly = TRUE)
  )
  output$Text=renderText("Total Loss is $0. Potential Gain is $0.")
  
  ##on button press
  observeEvent(input$btn,{
    output$hot <- renderRHandsontable(
      rhandsontable(DFcalculator(hot_to_r(input$hot))) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
        hot_col("Stake", readOnly = TRUE) %>%
        hot_col("Return", readOnly = TRUE)
    )
    
    output$record = renderRHandsontable(
      rhandsontable(BetMaker(hot_to_r(input$hot))) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
        hot_col("Stake", readOnly = TRUE) %>%
        hot_col("Return", readOnly = TRUE)
    )
    
    data=hot_to_r(input$hot)
    Loss=-round(data$Return[1]-sum(data$Stake),2)
    Gain=round(sum(data$Return)-sum(data$Stake),2)
    
    output$Text = renderText(paste0(
      "Total Loss is $",
      Loss,
      '. Potential Gain is $',
      Gain,
      '.'))
  })
})

DFcalculator = function(df) {
  df$Stake=stakeCalculator(df$Odds[1],df$Odds[2])
  df[,Return:=Stake*Odds]
}

stakeCalculator = function(oddsA,oddsB) {
  ratio=oddsA/oddsB
  if (ratio<=0.5) {
    return(c(1000,1000*ratio))
  } else if (ratio<=1) {
    return(c(500/ratio,500))
  } else if (ratio<=2) {
    return(c(500,500*ratio))
  } else if (ratio>2) {
    return(c(1000/ratio,1000))
  } else {
    stop("check odds")
  }
}

BetMaker = function(df) {
  df = df[order(-Stake)]
  SplitRow=df[1,c("Stake","Return"):=list(Stake/2,Return/2)][1]
  df = rbind(df,SplitRow)
  df=df[,Person:=c("Danny","Miltu","Vikash")][order(Team)]
  df[,Outcome:=""]
}
