library(shiny)
library(ggplot2)
library(dplyr)

setwd("C:/Users/userPC/Documents/GeneralInsurance_Class/Data")
dt_KPI <- read.csv("lesson2_KPI.csv")
dt_KPI_new <- dt_KPI %>% filter_all(all_vars(!is.na(.)))


function(input, output) {
  
  output$Plot <- renderPlot({
    
    
    ggplot(data = dt_KPI_new,
           mapping = aes_string(x = "Premium", y = "Expenses", colour = input$volba)
    ) +
      geom_point() +
      geom_smooth() 
    
  })
}


