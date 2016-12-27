library(shiny)
library(shinydashboard)
library(ggplot2)
library(gsheet)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(zoo)
library(lubridate)



shinyServer(function(input, output) {


# manual update -----------------------------------------------------------

metdata <-  eventReactive(input$reload, {

  file <-   getData(url = input$wtdt, type = "manually")

  file <- file %>%
    dplyr::mutate(ETo = PenMon(data = file, day = "date", Tmin = "Tmin", Tmax = "Tmax",
                          RHmin = "RHmin", RHmax = "RHmax",
                          n = "sun", v = "wind",
                          lat = "latitude", alt = "altitude", z = "z")) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    as.data.frame()

    }, ignoreNULL = FALSE)


# automatic update -----------------------------------------------------------

# metdata <-  reactive({
#
#   file <-   getData(url = input$wtdt, type = "automatic")
#
#   file <- file %>%
#     dplyr::mutate(ETo = PenMon(data = file, day = "date", Tmin = "Tmin", Tmax = "Tmax",
#                                RHmin = "RHmin", RHmax = "RHmax",
#                                n = "sun", v = "wind",
#                                lat = "latitude", alt = "altitude", z = "z")) %>%
#     dplyr::mutate(date = as.Date(date)) %>%
#     as.data.frame()
#
# })


# Plot metdata ------------------------------------------------------------


  output$wtplot <- renderPlotly({

    file <- metdata()

    file <- file %>%
      dplyr::select(-latitude, -altitude, -z) %>%
      dplyr::mutate(date = as.Date(date))%>%
      tidyr::gather(key = variable , value = value,  -date)


    ax <- list(
      title = "",
      type = "date",
      tickformat = "%d-%b",
      tickmode = "auto"
     )

    ay <- list(
      title = "",
      type = "linear",
      tickmode = "auto"
    )

    lgd <- list(

      orientation = "h",
      xanchor = "auto"

      )


    plot_ly(file, x = ~date, y = ~value,
            color = ~variable, symbol = ~variable) %>%
      add_lines() %>%
      layout(xaxis = ax, yaxis = ay, legend =  lgd)

  })


# Data frame in the screen ------------------------------------------------


  output$gss <- renderUI({

    gss <- tags$iframe(src = input$wtdt ,
                       style="height:450px; width:100%; scrolling=no")

    print(gss)

  })




# Crop dataframe ----------------------------------------------------------

cropdt <- reactive({


  stg1 <- tibble::data_frame( crop = input$crop,
                              date = (input$pdate:(input$pdate+ input$st1)),
                              stage = "initial",
                              DAP = 0:input$st1,
                              Kc = input$kc1,
                              CD = input$cd1,
                              ID = input$id1
  )

  stg2 <- tibble::data_frame( crop = input$crop,
                              date = ((input$pdate+input$st1+1):(input$pdate+input$st1+input$st2)),
                              stage = "develop",
                              DAP = (input$st1+1):(input$st1+input$st2),
                              Kc = input$kc2,
                              CD = input$cd2,
                              ID = input$id2
  )


  stg3 <- tibble::data_frame( crop = input$crop,
                              date = ((input$pdate+input$st1+input$st2+1):(input$pdate+input$st1+input$st2+input$st3)),
                              stage = "mid",
                              DAP = (input$st1+input$st2+1):(input$st1+input$st2+input$st3),
                              Kc = input$kc3,
                              CD = input$cd3,
                              ID = input$id3
  )


  stg4 <- tibble::data_frame( crop = input$crop,
                              date = ((input$pdate+input$st1+input$st2+input$st3+1):(input$pdate+input$st1+input$st2+input$st3+input$st4)),
                              stage = "end",
                              DAP = (input$st1+input$st2+input$st3+1):(input$st1+input$st2+input$st3+input$st4),
                              Kc = input$kc4,
                              CD = input$cd4,
                              ID = input$id4
  )


  cdt <- rbind(stg1, stg2, stg3, stg4) %>%
    dplyr::mutate(date = as.Date(date), Da = input$sden, FC = input$fc, WP = input$wp) %>%
    as.data.frame()


  })



  # merge table --------------------------------------------------------------


cpt <- reactive({

  file1 <- cropdt()
  file2 <- metdata()

  crop <- dplyr::left_join(file1, file2) %>%
    dplyr::mutate(ETc = Kc*ETo) %>%
    dplyr::mutate(IR = (((FC-WP)/100)*Da*(1-CD/100)*ID*input$area*input$irs)/input$ire) %>%
    dplyr::select(crop, date, DAP, stage, Kc, ETo, ETc, IR) %>%
    dplyr::mutate(
      ETo = round(ETo, 2),
      ETc =  round(ETc, 2),
      IR = round(IR, 2)
      )



})


# Table formated ----------------------------------------------------------    file1 <- metdata()


  output$crop <- renderDataTable({


    file <- cpt()


    DT::datatable(file,
                  # filter = list(position = 'top', clear = FALSE),
                  extensions = 'Scroller',
                  rownames=FALSE,
                  options = list(
                    autoWidth = TRUE,
                    columnDefs = list(list(className = 'dt-center', targets ="_all")),
                    searching = FALSE,
                    deferRender=TRUE,
                    scrollY = 500,
                    scroller = TRUE,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                    ))



  })




})


