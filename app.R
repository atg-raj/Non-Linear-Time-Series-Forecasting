
#Load libraries
library(shiny)
library(quantmod)
library(ggplot2)
library(shinyTime)
library(tseries)
library(dsa)
library(forecast)
library(ggfortify)
library(smooth)
library(Mcomp)
library(rugarch)
library(tidyRSS)
library(tidyquant)
library(shinythemes)
source("armagarch_functions.R")

stock_list <- c("AAPL","MSFT","AMZN","FB", "AAPL", "GOOG","JNJ","PG","V","NVDA"
                ,"HD","MA","JPM","UNH","VZ","ADBE","CRM","PYPL","NFLX","DIS","INTC","MRK","T")

#### USER INTERFACE Define UI ----
ui = shinyUI(fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Non-Linear Forecasting"),
  helpText("Select desired stock ,start date and end date"),

  sidebarPanel(
    width = 3,
    selectizeInput('stock_name', 'Stock', c('Select stock', stock_list), selected = 'Select stock'),
    htmlOutput('selected_stock'),
    dateInput('start_time', 'Start Date', value = Sys.Date() - 365),
    dateInput('end_time', 'End Date')
    
  ),
  
  # Parameters for ARIMA-GARCH method ####

      absolutePanel(
        width = 250,
        top = 500, left = 50,
        sliderInput('days_forecast_arima_garch', 'Days to forecast', min = 0, max =  365, value = 15),
        selectizeInput('pred_interval_arima_garch', 'Prediction Interval', c('99', '95', '90', '80', '70','60'), selected = '95'),
        
        selectInput("AR_para", "AR (p)", 
                    choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4), selected = 2), 
        selectInput("MA_para", "MA (q)", 
                    choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4), selected = 2),  
        selectInput("lag_variance_para", "G (p)", 
                    choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4), selected = 2), 
        selectInput("lag_res_para", label = "ARCH (q)", 
                    choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4), selected = 2),
        radioButtons("transform", label = "Transformation Selection",
                     choices = list("Close Price" = "Close", 
                                    "Absolute Returns" = "Returns", 
                                    "Log Returns" = "Log Returns"), 
                     selected = "Log Returns")
        
      
  ),


  mainPanel(
    plotOutput("my_plot"),
    
    tableOutput("view")
    ,
    
    tableOutput("view2")
  )
))

server = shinyServer(function(input, output){
  output$my_plot = renderPlot({

    
   
    # ARIMA-GARCH ####
      if(input$stock_name != 'Select stock'){
      my_ts = na.omit(getSymbols(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time))
    
      # Arima-Garch Table Info
      if(input$transform == "Close"){
        my_ts_for_garch <- my_ts[,4]
      } else if(input$transform =="Returns"){
        my_ts_for_garch <- na.omit(diff(my_ts[,4]))
      } else if(input$transform =="Log Returns"){
        my_ts_for_garch <- na.omit(diff(log(my_ts[,4])))
      }
      mat <- garma_model(my_ts_for_garch, input$AR_para, input$MA_para,
                         input$lag_variance_para, input$lag_res_para,
                         input$pred_interval_arima_garch, input$transform, Cl(my_ts[length(my_ts_for_garch)]),
                         input$days_forecast_arima_garch)
      ic <- model_quality(my_ts_for_garch,input$AR_para, input$MA_para,
                          input$lag_variance_para, input$lag_res_para)
      
      # Table Info
      serie_variants <- c("Close","Returns","Log Returns")
      table_complete <- cbind(serie_variants, stationarity_test(my_ts),rep("||",3),
                              c("AIC","BIC","H-Q"), ic)
      colnames(table_complete) <- c("Transformation of the series", "p-value of the ADF test","||",
                                    "Information Criteria", " ")
    
      output$view <- renderTable({
        table_complete
      })
      
      
      # Arima-Garch forecast Table Creation
      output$view2 <- renderTable({
        na.omit(mat)
      })
      
      priceandempty <- c(as.numeric(Cl(my_ts)), rep(NA, input$days_forecast_arima_garch))
      
      plot(priceandempty, type="l", main = "Stock Prices v/s Days Selected", xlab = "Days",ylab = "Forecasted Stock Prices")
      lines(mat[,2], col ="purple", lty = 2)
      lines(mat[,1], col ="red", lty = 2)
      lines(mat[,3], col ="red", lty = 2)
    
    }
  })
  output$selected_stock = renderText({
    input$stock_name
  })
  output$initial_date = renderText({
    index(getSymbols(input$stock_name, auto.assign = F))[1]
  })
})

# Run app ####
shiny::shinyApp(ui,server)

