# fieldbook -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(sapiens)
library(agricolae)
library(dplyr)
library(plotly)
library(tibble)
library(DT)
library(ggplot2)


shinyServer(function(input, output) {


# import data -----------------------------------------------------------

fb <-  eventReactive(input$reload, {

  file <- sapiens::getData(dir = input$wtdt)

    }, ignoreNULL = FALSE)


# fieldbook ------------------------------------------------


output$fbook <- DT::renderDataTable({


  file <- fb()


  DT::datatable(file,
    #filter = list(position = 'top', clear = FALSE),
    extensions = 'Scroller',
    rownames=FALSE,
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets ="_all")),
      searching = FALSE,
      deferRender=TRUE,
      scrollY = 450,
      scroller = TRUE,
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))



})


# output$gss <- renderUI({
#
#   gss <- tags$iframe(src = input$wtdt ,
#     style="height:450px; width:100%; scrolling=no")
#
#   print(gss)
#
# })


# boxplot -----------------------------------------------------------------

output$bpx <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "xbp",
    label = "Axis X",
    choices = c("choose" = "", fbn)
  )

})

output$bpy <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "ybp",
    label = "Axis Y",
    choices = c("choose" = "", fbn)
  )

})


output$bpz <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "zbp",
    label = "Grouped",
    choices = c("choose" = "", fbn)
  )

})


output$boxplot <- renderPlot({

  file <- fb()

  boxp <- sapiens::bplot(

    data = file,
    x = input$xbp,
    y = input$ybp,
    z = input$zbp,
    xlab = input$bplx,
    ylab = input$bply,
    lgl =  input$bplz,
    lgd = "top"

  )

  boxp


})


# multivariate ------------------------------------------------------------

output$crpt <- renderPlot({

  file <- fb()
  file <- file %>% select_if(is.numeric) %>% as.data.frame()

  crd <- agricolae::correlation(file, method = "pearson")

  col3 <- colorRampPalette(c("red", "white", "blue"))

  crp <- corrplot::corrplot(
    corr = crd$correlation,
    method="color",
    col=col3(20))


})


output$pca <- renderPlot({

  file <- fb()
  file <- file %>% select_if(is.numeric) %>% as.data.frame()

  FactoMineR::PCA(file)

})


# statistics --------------------------------------------------------------

# Select factors


output$resp <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "rsp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})


output$stv1 <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "fac1",
    label = "Factor 1",
    choices = c("choose" = "", fbn)
  )

})

output$stv2 <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "fac2",
    label = "Factor 2",
    choices = c("choose" = "", fbn)
  )

})


output$block <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "blk",
    label = "Block",
    choices = c("choose" = "", fbn)
  )

})

# ANOVA

av <- reactive({

  file <- fb()

  if (is.null(file)){return(NULL)}

  else if(input$fac1 == '' && input$fac2 == '' && input$blk == '' && input$rsp == '')

  {return(NULL)}

  else if( !(input$fac1 == '') && !(input$fac2 == '') && !(input$blk == '') && !(input$rsp == ''))

  {
    formula <- as.formula( paste(input$rsp, paste(input$blk, paste(input$fac1, input$fac2, sep = "*"), sep = " + ") , sep = " ~ ") )
    modelo <- aov(formula, data = file)
  }


  else if( !(input$fac1 == '') && !(input$fac2 == '') && !(input$rsp == ''))

  {
    formula <- as.formula( paste(input$rsp, paste(input$fac1, input$fac2, sep = "*") , sep = " ~ ") )
    modelo <- aov(formula, data = file)
  }

  else if( !(input$fac1 == '') && !(input$blk == '') && !(input$rsp == ''))

  {
    formula <- as.formula( paste(input$rsp, paste(input$blk, input$fac1, sep = " + ") , sep = " ~ ") )
    modelo <- aov(formula, data = file)
  }


  else if( !(input$fac1 == '') && !(input$rsp == ''))

  {
    formula <- as.formula(paste(input$rsp, input$fac1, sep = " ~ "))
    modelo <- aov(formula, data = file)
  }


})

# ANOVA table

output$tbav = renderPrint({

  file <- av()

  if (is.null(file)){ cat("select your variables") }

  else {

    summary(file)

  }
})



# Mean comparion test


mcomp <- reactive({

  file <- av()

  if (is.null(file)) return(NULL)

  else if( !(input$fac1 == '') && !(input$fac2 == '') && !(input$rsp == ''))

  {
    snk <- agricolae::SNK.test( y = file, trt = c(input$fac1, input$fac2 ))
    mc <- sapiens::dtsm(snk)
    mc
  }


  else if( !(input$fac1 == '') && !(input$rsp == ''))

  {
    snk <- agricolae::SNK.test( y = file, trt = c(input$fac1))
    mc <- sapiens::dtsm(snk)
    mc
  }


})

# Mean comparison table

output$mnc = DT::renderDataTable({

  file <- mcomp()

  DT::datatable(file,
    # filter = list(position = 'top', clear = FALSE),
    extensions = 'Scroller',
    rownames=FALSE,
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets ="_all")),
      searching = FALSE,
      deferRender=TRUE,
      scrollY = 420,
      scroller = TRUE,
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))

})




# graphics ----------------------------------------------------------------


output$grplot <- renderPlot({


df <- mcomp()


if (is.null(df)) return(NULL)

else if( !(input$fac1 == '') && !(input$fac2 == '') && !(input$rsp == ''))

{

sapiens::fplot(data = df, type = "bar",
  x = input$fac1,
  y = "mean",
  z = input$fac2,
  ylab = input$gply,
  xlab = input$gplx,
  lgl = input$gplz,
  lgd = "top",
  erb = T,
  sig = "sg"
  )


}


else if( !(input$fac1 == '') && !(input$rsp == ''))

{

  sapiens::fplot(data = df, type = "bar",
    x = input$fac1,
    y = "mean",
    z = input$fac1,
    ylab = input$gply,
    xlab = input$gplx,
    lgl = input$gplz,
    lgd = "top",
    erb = T,
    sig = "sg"
    )

  }




})




})




