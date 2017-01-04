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

  file <- sapiens::getData(dir = input$fbdt)

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


output$fbook <- renderUI({

  gss <- tags$iframe(src = input$fbdt,
    style="height:450px; width:100%; scrolling=no")

  print(gss)

})


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
    label = "Response",
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

  boxp <- sapiens::plot_box(

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

  sapiens::plot_correlation(file)

})


output$pca <- renderPlot({

  file <- fb()

  sapiens::plot_PCA(data = file)


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

  fac1 <- input$fac1
  fac2 <- input$fac2
  blk <-  input$blk
  rsp <-  input$rsp


  if(fac1 != ""){

    file[, fac1 ] <- file[,fac1] %>% as.factor()

  }

  if(fac2 != ""){

    file[, fac2] <- file[, fac2] %>% as.factor()

  }

  if(blk != ""){

    file[,blk] <- file[,blk] %>% as.factor()

  }



  if (is.null(file)){return(NULL)}

  else if(fac1 == '' && fac2 == '' && blk == '' && rsp == '')

  {return(NULL)}

  else if( !(fac1 == '') && !(fac2 == '') && !(blk == '') && !(rsp == ''))

  {
    formula <- as.formula( paste(rsp, paste(blk, paste(fac1, fac2, sep = "*"), sep = " + ") , sep = " ~ ") )
    modelo <- aov(formula, data = file)
  }


  else if( !(fac1 == '') && !(fac2 == '') && !(rsp == ''))

  {
    formula <- as.formula( paste(rsp, paste(fac1, fac2, sep = "*") , sep = " ~ ") )
    modelo <- aov(formula, data = file)
  }

  else if( !(fac1 == '') && !(blk == '') && !(rsp == ''))

  {
    formula <- as.formula( paste(rsp, paste(blk, fac1, sep = " + ") , sep = " ~ ") )
    modelo <- aov(formula, data = file)
  }


  else if( !(fac1 == '') && !(rsp == ''))

  {
    formula <- as.formula(paste(rsp, fac1, sep = " ~ "))
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


# comparison test


comp <- reactive({

  file <- av()
  test <- input$stmc
  sig <- input$stsig

  if (is.null(file)) return(NULL)

  else if( !(input$fac1 == '') && !(input$fac2 == '') && !(input$rsp == ''))

  {

  rs <- sapiens::test_comparison(
      aov = file,
      comp = c(input$fac1, input$fac2 ),
      type = test,
      sig = sig)

  }


  else if( !(input$fac1 == '') && !(input$rsp == ''))

  {

  rs <- sapiens::test_comparison(
      aov = file,
      comp = c(input$fac1),
      type = test,
      sig = sig)

  }


})



# Mean comparison table

output$mnc = DT::renderDataTable({

  file <- comp()

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


output$stplot <- renderPlot({


df <- comp()
fac1 <- input$fac1
fac2 <- input$fac2
rsp <- input$rsp
gtype <- input$gtype
gcolor <- input$gcolor
gply <- input$gply
gplx <- input$gplx
gplz <- input$gplz
gerbr <- input$gerbr
gsig <- input$gsig
gfont <- input$gfont
glabel <- input$glabel

limits <- input$glmti * input$glmtf
brakes <- input$gbrakes

# limits & brake ----------------------------------------------------------

if(is.na(limits)) {

  glimits <- NULL

} else {

  glimits <- c(input$glmti, input$glmtf)

}


if(is.na(brakes)) {

  gbrakes <- NULL

} else {

  gbrakes <- brakes

  }


# Error & significance ----------------------------------------------------

if(gerbr == "yes"){

  gerbr <- TRUE

}

if (gerbr == "no"){

  gerbr <-  FALSE

  }


if(gsig == "yes"){

  gsig <- "sg"

}

if (gsig == "no"){

  gsig <-  NULL

  }


# body graph --------------------------------------------------------------


if (is.null(df)) return(NULL)

else if( !(fac1 == '') && !(fac2 == '') && !(rsp == ''))

{

pt <- sapiens::plot_brln(data = df, type = gtype,
  x = input$fac1,
  y = "mean",
  z = input$fac2,
  ylab = gply,
  xlab = gplx,
  lgl = gplz,
  lgd = glabel,
  erb = gerbr,
  sig = gsig,
  font = gfont,
  lmt = glimits,
  brk = gbrakes
  )


}


else if( !(fac1 == '') && !(rsp == ''))

{

pt <- sapiens::plot_brln(data = df, type = gtype,
    x = input$fac1,
    y = "mean",
    z = input$fac1,
    ylab = gply,
    xlab = gplx,
    lgl = gplz,
    lgd = glabel,
    erb = gerbr,
    sig = gsig,
    font = gfont,
    lmt = glimits,
    brk = gbrakes
    )

  }


if(gtype == "bar" && gcolor == "color" ){

  pt

} else if (gtype == "bar" && gcolor == "gray"){

  pt + scale_fill_grey(gplz, start = 1, end = 0)

} else if (gtype == "line" && gcolor == "color"){

  pt

} else if (gtype == "line" && gcolor == "gray"){

  pt +
    scale_color_grey(gplz, start = 0, end = 0) +
    scale_shape_discrete(gplz)

}


})




})




