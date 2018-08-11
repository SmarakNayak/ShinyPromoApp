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
pacman::p_load("dplyr","data.table","rhandsontable","DT","devtools","googlesheets")
library(dplyr)
library(data.table)
library(rhandsontable)
library(DT)
library(shinyTable)
library(googlesheets)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  sheet=gs_key("1b6EMQRD55O9755rw2LhV1Vif4kqWv-GUXgQSJyQb4uI")
  
  #####
  ##Rugby league
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
        hot_col("Return", readOnly = TRUE)
    )
  })
  observeEvent(input$btn,{
    data=reactive(hot_to_r(input$hot))
    Loss=reactive(-round(data()$Return[1]-sum(data()$Stake),2))
    Gain=reactive(round(sum(data()$Return)-sum(data()$Stake),2))
    
    output$Text = renderText(paste0(
      "Total Loss is $",
      Loss(),
      '. Potential Gain is $',
      Gain(),
      '.'))
  })
  
  ##return updater
  observeEvent(input$recUpd,{
    df=hot_to_r(input$record)
    df[,Return:=Odds*Stake]
    output$record = renderRHandsontable(
      rhandsontable(df) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
        hot_col("Return", readOnly = TRUE)
    )
  })
  
  ##button logger
  observeEvent(input$log,{
    gs_add_row(sheet,2,hot_to_r(input$record))
    showModal(modalDialog(
      title = "Success!",
      "Rows successfully appended."
    ))
    showNotification("Rows successfully appended.")
  })
  
  ##### 
  ##Premier League
  df3=data.table(Team=c("Team A","Team B","Draw"),Odds=c(3.0,3.0,3.0),Stake = c(0,0,0),Return=c(0,0,0),stringsAsFactors = FALSE)
  df4=data.table(Team=c("Team A","Team A","Team B","Draw"),Odds=c(3.0,3.0,3.0,3.0),Stake = c(0,0,0,0),Return=c(0,0,0,0),
                 Person=c("Danny","Miltu","Vikash","Miltu"),Outcome=c("","","",""),stringsAsFactors = FALSE)
  ###init
  output$plCalc <- renderRHandsontable(
    rhandsontable(df3) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
      hot_col("Stake", readOnly = TRUE) %>%
      hot_col("Return", readOnly = TRUE)
  )
  output$plRecord = renderRHandsontable(
    rhandsontable(df4) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
      hot_col("Return", readOnly = TRUE)
  )
  output$plText=renderText("Total Loss is $0. Potential Gain is $0.")
  
  ###button presses
  observeEvent(input$plBtn,{
    output$plCalc <- renderRHandsontable(
      rhandsontable(DFcalculator2(hot_to_r(input$plCalc))) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
        hot_col("Stake", readOnly = TRUE) %>%
        hot_col("Return", readOnly = TRUE)
    )
    
    output$plRecord = renderRHandsontable(
      rhandsontable(BetMaker(hot_to_r(input$plCalc))) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
        hot_col("Return", readOnly = TRUE)
    )
    
    data=reactive(hot_to_r(input$plCalc))
    Loss=reactive(-round(data()$Return[1]-sum(data()$Stake),2))
    Gain=reactive(round(sum(data()$Return)-sum(data()$Stake),2))
    
    output$plText = renderText(paste0(
      "Total Loss is $",
      Loss(),
      '. Potential Gain is $',
      Gain(),
      '.'))
  })
  
  observeEvent(input$plRecUpd,{
    df=hot_to_r(input$plRecord)
    df[,Return:=Odds*Stake]
    output$plRecord = renderRHandsontable(
      rhandsontable(df) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
        hot_col("Return", readOnly = TRUE)
    )
  })
  
  ##button logger
  observeEvent(input$plLog,{
    gs_add_row(sheet,3,hot_to_r(input$plRecord))
    showModal(modalDialog(
      title = "Success!",
      "Rows successfully appended."
    ))
    showNotification("Rows successfully appended.")
  })
})

DFcalculator = function(df) {
  df$Stake=stakeCalculator(df$Odds)
  df[,Return:=Stake*Odds]
}

DFcalculator2 = function(df) {
  df$Stake=stakeCalculator2(df$Odds)
  df[,Return:=Stake*Odds]
}

stakeCalculator = function(odds) {
  oddsA=odds[1]
  oddsB=odds[2]
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

stakeCalculator2 = function(odds) {
  minOdds=min(odds[1:2])
  maxStake=max(stakeCalculator(odds[1:2]))
  drawStake=maxStake*minOdds/odds[3]
  return(c(stakeCalculator(odds[1:2]),drawStake))
}

BetMaker = function(df) {
  drawRow=df[tolower(Team)=="draw",]
  df = df[tolower(Team)!="draw", ][order(-Stake)]
  SplitRow=df[1,c("Stake","Return"):=list(Stake/2,Return/2)][1]
  df = rbind(df,SplitRow)
  df=df[order(Team)]
  df=rbind(df,drawRow)
  df=df[,Person:=c("Danny","Miltu","Vikash","Miltu")]
  df[,Outcome:=""]
}
