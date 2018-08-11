#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  df=data.table(Team=c("Team A","Team B"),Odds=c(1.0,1.0),Stake = c(0,0),Return=c(0,0),stringsAsFactors = FALSE)

  output$hot <- renderRHandsontable(
    rhandsontable(df) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
      hot_col("Stake", readOnly = TRUE) %>%
      hot_col("Return", readOnly = TRUE)
  )
   
  observeEvent(input$hot,{
    output$hot <- renderRHandsontable(
      rhandsontable(DFcalculator(hot_to_r(input$hot))) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_validate_numeric("Odds",allowInvalid = FALSE) %>%
        hot_col("Stake", readOnly = TRUE) %>%
        hot_col("Return", readOnly = TRUE)
    )
  })
  
  output$Text = reactive({
    data=hot_to_r(input$hot)
    Loss=-round(data$Return[1]-sum(data$Stake),2)
    Gain=round(sum(data$Return)-sum(data$Stake),2)
    paste0(
      "Total Loss is $",
      Loss,
      '. Potential gain is $',
      Gain,
      '.')
    
  })
  
  output$DT=renderDataTable(datatable(DFcalculator(hot_to_r(input$hot)),options = list(dom="t")))
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
