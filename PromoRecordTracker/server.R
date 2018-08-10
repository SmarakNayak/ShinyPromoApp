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
   DF=data.frame(Team=c("Team A","Team B"),Odds=c(1.0,1.0),Stake = c(0,0),Return=c(0,0))
  output$hot <- renderRHandsontable({
    rhandsontable(DF,width = 500,height = 500)
  })
  
})


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