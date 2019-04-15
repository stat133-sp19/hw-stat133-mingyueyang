#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Savings-Investment Simulation"),
  
  # Widgets  
  fluidRow(
    column(4,
      sliderInput("initial",
                  label = "Initial Amount",
                  min = 0,
                  max = 100000,step = 500,pre = "$",
                  value = 1000)),
    column(4,  
           sliderInput("return",
                       label = "Return Rate(in %)",
                       min = 0,
                       max = 20,
                       step = 1,
                       value = 5)),
    
    column(4,
           sliderInput("years",
                       label = "Years",
                       min = 0,
                       max = 50,step = 1,
                       value = 20))),
  
  fluidRow(
    column(4,  
           sliderInput("contribution",
                       label = "Annual Contribution",pre = "$",
                       min = 0,
                       max = 50000,step = 500,
                       value = 2000)),
    
    
    column(4,
           sliderInput("growth",
                       label = "Growth Rate(in %)",
                       min = 0,
                       max = 20,step = 1,
                       value = 2)), 
    
    column(4,
           selectInput("facet",
                       label = "Facet?",
                       choices = c("YES","NO"),selected = "NO"))
    ),
  
  hr(),
  
  fluidRow(h4("Timelines"),
    plotOutput("linePlot")),
  
  br(),
  
  fluidRow(h4("Balances"),
           verbatimTextOutput("balances"))
  
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$linePlot <- renderPlot({
     
     future_value <- function(amount=1000,rate=0.05,years=1){
       fv <- amount*(1+rate)^(years)
       return(fv)}
     
     annuity <- function(contrib=200,rate=0.05,years=1){
       fva <- contrib*(((1+rate)^(years)-1)/rate)
       return(fva)}
     
     growing_annuity <- function(contrib=200,rate=0.05,growth=0.03,years=1){
       fvga <- contrib*(((1+rate)^(years)-(1+growth)^(years))/(rate-growth))
       return(fvga)}
     
     ini <- input$initial
     contri <- input$contribution
     r <- (input$return)/100
     g <- (input$growth)/100
     yr <- input$years
     
     mode1 <- rep(0,yr+1)
     mode2 <- rep(0,yr+1)
     mode3 <- rep(0,yr+1)
     
     for(i in 1:yr){
       mode1[i+1] <- future_value(amount = ini,rate = r, years = i)
       mode2[i+1] <- future_value(amount = ini,rate = r, years = i)+annuity(contrib = contri,rate=r,years=i)
       mode3[i+1] <- future_value(amount = ini,rate=r,years=i)+growing_annuity(contrib = contri,rate = r,growth = g,years = i)
       mode1[1] <- ini
       mode2[1] <- ini
       mode3[1] <- ini
     }
     
     year <- c(0:yr)
     modalities <- data.frame(year=year,no_contrib=mode1,fixed_contrib=mode2,growing_contrib=mode3)
     
     invest_mode <- c(rep("no_contrib",yr+1),rep("fixed_contrib",yr+1),rep("growing_contrib",yr+1))
     invest_mode <- as.factor(invest_mode)
     facet1 <- data.frame(year=year,bal=mode1)
     facet2 <- data.frame(year=year,bal=mode2)
     facet3 <- data.frame(year=year,bal=mode3)
     facett <- rbind.data.frame(facet1,facet2,facet3)
     facettotal <- cbind.data.frame(invest_mode,facett)
     
     
     facet <- input$facet
     
     if(facet == "NO"){
     ggplot(data=modalities,aes(x=year))+
       geom_line(aes(y=no_contrib,color='no_contrib'),size=0.8)+
       geom_line(aes(y=fixed_contrib,color='fixed_contrib'),size=0.8)+
       geom_line(aes(y=growing_contrib,color='growing_contrib'),size=0.8)+
       labs(title="Annual Balances under Three Investment Modes",x="Year",y="Annual Balance(in dollars)",color="Investment Modes")+
       theme_minimal()+
       scale_colour_manual("", breaks = c("no_contrib", "fixed_contrib", "growing_contrib"),values = c("#7A3225", "#7ADFCC","#2174F0"))}
     
     else{
       ggplot(data=facettotal,aes(x=year))+
         geom_line(aes(y=bal,col=invest_mode),size=0.8)+geom_area(aes(y=bal,fill=invest_mode),alpha=0.4)+
         labs(title="Annual Balances under Three Investment Modes",x="Year",y="Annual Balance(in dollars)")+facet_grid(~ invest_mode)
     }
     
   })
   
   output$balances <- renderPrint({
     future_value <- function(amount=1000,rate=0.05,years=1){
       fv <- amount*(1+rate)^(years)
       return(fv)}
     
     annuity <- function(contrib=200,rate=0.05,years=1){
       fva <- contrib*(((1+rate)^(years)-1)/rate)
       return(fva)}
     
     growing_annuity <- function(contrib=200,rate=0.05,growth=0.03,years=1){
       fvga <- contrib*(((1+rate)^(years)-(1+growth)^(years))/(rate-growth))
       return(fvga)}
     
     ini <- input$initial
     contri <- input$contribution
     r <- (input$return)/100
     g <- (input$growth)/100
     yr <- input$years
     
     mode1 <- rep(0,yr+1)
     mode2 <- rep(0,yr+1)
     mode3 <- rep(0,yr+1)
     
     for(i in 1:yr){
       mode1[i+1] <- future_value(amount = ini,rate = r, years = i)
       mode2[i+1] <- future_value(amount = ini,rate = r, years = i)+annuity(contrib = contri,rate=r,years=i)
       mode3[i+1] <- future_value(amount = ini,rate=r,years=i)+growing_annuity(contrib = contri,rate = r,growth = g,years = i)
       mode1[1] <- ini
       mode2[1] <- ini
       mode3[1] <- ini
     }
     
     year <- c(0:yr)
     modalities <- data.frame(year=year,no_contrib=mode1,fixed_contrib=mode2,growing_contrib=mode3)
     modalities
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

