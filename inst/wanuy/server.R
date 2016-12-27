library(shiny)
library(shinydashboard)
library(gsheet)
library(dplyr)
library(tidyr)
library(DT)

shinyServer(function(input, output) {
  
ft <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1XBezmc0S1T0EQj7zuZjTAOlPXosdDgTTotJK2jPAQAI/edit#gid=1020115876")  

fert <- ft %>% 
  select(fertilizante, elemento, porcentage) %>% 
  spread(key = elemento, value = porcentage)

output$frt <- DT::renderDataTable(fert, server = F, filter = 'top', option = list(pageLength = 50, autoWidth = F))  
    
# select row & colums: https://rstudio.github.io/DT/shiny.html

output$sfrt = renderPrint({
  s = input$frt_rows_selected
  if (length(s)) {
    cat(fert$fertilizante[s], sep = '\n')
  }
})

  
})
