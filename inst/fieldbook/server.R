# fieldbook -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(sapiens)
library(agricolae)
library(dplyr)
library(tibble)
library(DT)
library(ggplot2)



shinyServer(function(input, output) {


# import data -----------------------------------------------------------

fb <-  eventReactive(input$reload, {


  validate(

    need( input$fbdt, message = "Insert gss URL or xlsx file" )

    )


  if ( !is.null(input$impdata) ) {

    xls <- input$impdata

    file.rename(xls$datapath, paste(xls$datapath, ".xlsx", sep = ""))

    sapiens::getData(dir = paste(xls$datapath, ".xlsx", sep = ""), sheet = input$sheetdt)


  } else {

    url <- input$fbdt

    sapiens::getData(dir = url, sheet = input$sheetdt)

  }



    }, ignoreNULL = FALSE)



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

  validate(

    need( input$ybp, "Select your response variable"),
    need( input$xbp, "Select your X axis variable" ),
    need( input$zbp, "Select your grouped variable")

  )



  file <- fb()

  if(is.na(input$bpbrk)){

    brks <- NULL

  } else {

    brks <- input$bpbrk

  }

  boxp <- sapiens::plot_box(

    data = file,
    x = input$xbp,
    y = input$ybp,
    z = input$zbp,
    xlab = input$bplx,
    ylab = input$bply,
    lgl =  input$bplz,
    lgd = "top",
    font = input$bpsize,
    brk = brks

  )

  boxp


})


# multivariate ------------------------------------------------------------

output$crpt <- renderPlot({

  file <- fb()

  sapiens::plot_correlation(
    data = file,
    sig = input$corsig,
    color = input$corcol,
    font = input$cor_font)

})


output$pca <- renderPlot({

  file <- fb()


  if( is.na(input$pcaqs) ){

    qs <- NULL

  } else {

    qs <- input$pcaqs

  }


  if( input$pcalbl == "" ){

    lbl <- NULL

  } else {

    lbl <- input$pcalbl

  }




  sapiens::plot_PCA(
    data = file,
    type = input$pcatype,
    quali.sup = qs,
    lgl = lbl
    )


})


# statistics --------------------------------------------------------------

# Select factors


output$stat_response <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "stat_rsp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})


output$stat_factor <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "stat_fact",
    label = "Factors",
    choices = c("choose" = "", fbn),
    multiple = TRUE
  )

})



output$stat_block <- renderUI({

  file <- fb()
  fbn <- names(file)

  selectInput(
    inputId = "stat_blk",
    label = "Block",
    choices = c("choose" = "", fbn),
    multiple = TRUE
  )

})

# ANOVA


av <- reactive({

  validate(

    need( input$stat_rsp, "Select your response variable" ),
    need( input$stat_fact, "Select your factors")

  )

    file <- fb()

    variable <- input$stat_rsp

    factor <- input$stat_fact %>% paste0() %>%  paste(collapse= " * ")

    block <- input$stat_blk %>% paste0() %>% paste(collapse= " + ")

    file <- file %>% dplyr::mutate_each_(funs(factor(.)), input$stat_fact)


    if ( block == "" ){

      formula <- as.formula(paste( variable , factor, sep = " ~ "))


    } else {

      formula <- as.formula(paste( variable , paste(block, factor, sep = " + "), sep = " ~ "))

    }


    av <- aov(formula, data = file)
    av



})


# ANOVA table

output$tbav = renderPrint({

  file <- av()

  summary(file)


})


# comparison test


comp <- reactive({


  validate(
    need( input$comp , "Select you parameter for statistical analysis")
  )

  file <- av()
  test <- input$stmc
  sig <- input$stsig
  factor <- input$stat_fact
  variable <- input$stat_rsp


  if( length(factor) == 1 && !(variable == '') )

  {

    rs <- sapiens::test_comparison(
      aov = file,
      comp = factor[1],
      type = test,
      sig = sig)


  }


  else if( length(factor) >= 2 && !(variable == '') )

  {

    rs <- sapiens::test_comparison(
      aov = file,
      comp = c( factor[1], factor[2] ),
      type = test,
      sig = sig)


  }


  rs


})



# Mean comparison table

output$mnc = DT::renderDataTable({

  file <- comp()

  file <- file %>% format(digits = 3, nsmall = 3)


  DT::datatable(file,

    filter = 'top',
    extensions = c('Buttons', 'Scroller'),
    rownames = FALSE,

    options = list(

      searchHighlight = TRUE,
      searching = TRUE,

      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),

      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets ="_all")),
      deferRender=TRUE,
      scrollY = 400,
      scrollX = TRUE,
      scroller = TRUE,

      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))

})


# graphics ----------------------------------------------------------------

stat_plot <- reactive({

  validate(
    need( input$stat_plot , "Select you parameter for statistical analysis")
  )


df <- comp()

factor <- input$stat_fact
variable <- input$stat_rsp
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

xbl <- input$gp_xbk
zbl <- input$gp_zbk


# Title axis --------------------------------------------------------------

if ( gply == ""){

  gply <- variable

} else {

  gply <- input$gply

}

if ( gplx == ""){

  gplx <- NULL

}

if ( gplz == ""){

  gplz <- NULL

}


# Color -------------------------------------------------------------------

if ( gcolor == "yes" ){

  gcolor <- TRUE

} else {

  gcolor <- FALSE

}



# Label brake axis --------------------------------------------------------


if ( xbl == ""){

  xbl <- NULL

} else {

  xbl <- input$gp_xbk

}

if ( zbl == ""){

  zbl <- NULL

} else {

  zbl <- input$gp_zbk

}

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


if( length(factor) == 1 && !(variable == '') ){


  pt <- sapiens::plot_brln(data = df, type = gtype,
    x = factor[1],
    y = "mean",
    z = factor[1],
    ylab = gply,
    xlab = gplx,
    lgl = gplz,
    lgd = glabel,
    erb = gerbr,
    sig = gsig,
    font = gfont,
    lmt = glimits,
    brk = gbrakes,
    xbl = xbl,
    zbl = zbl,
    color = gcolor
  )


}


else if( length(factor) >= 2  && !(variable == ''))

{


  pt <- sapiens::plot_brln(data = df, type = gtype,
    x = factor[1],
    y = "mean",
    z = factor[2],
    ylab = gply,
    xlab = gplx,
    lgl = gplz,
    lgd = glabel,
    erb = gerbr,
    sig = gsig,
    font = gfont,
    lmt = glimits,
    brk = gbrakes,
    xbl = xbl,
    zbl = zbl,
    color = gcolor
  )


}


pt


})



# plot output -------------------------------------------------------------

output$stplot <- renderPlot({

  plot <-  stat_plot()
  plot

})

# download plot -----------------------------------------------------------

output$download_plot <- downloadHandler(
  file = function(){ paste( "plot_", input$stat_rsp, '.tiff', sep = '')},
  content = function(file){
    ggplot2::ggsave(file, plot = stat_plot(), device = "tiff", dpi = 300, width = input$plot_W, height = input$plot_H, units = "mm" )

  }
)


# fieldbook design --------------------------------------------------------



fdbk <- reactive({

  validate(
    need( input$tool_f1, "Insert levels for you experiment")
  )

  trt1 <- input$tool_f1
  trt2 <- input$tool_f2
  dsg <-  input$tool_dsg
  lbl1 <- input$tool_lb1
  lbl2 <- input$tool_lb2
  r <- input$tool_rep
  int <- input$tool_eva


  if( trt2 == "" ){

    trt2 <- NULL

  } else {

    trt2 <- input$tool_f2

  }

  if( trt1 == "" ){

    trt1 <- NULL

  } else {

    trt1 <- input$tool_f1

  }


  if( input$tool_rep == "" ){

    r <- NULL

  } else {

    r <- input$tool_rep

  }


  if( input$tool_var == "" ){

    vars <- NULL

  } else {

    vars <- input$tool_var

  }


  sapiens::design_fieldbook(
    treat1 = trt1,
    treat2 = trt2,
    rep = r,
    design = dsg,
    lbl_treat1 = lbl1,
    lbl_treat2 = lbl2,
    variables = vars,
    intime = int
    )


})



# Fieldbook table ---------------------------------------------------------

output$fbdsg = DT::renderDataTable({


file <- fdbk()

DT::datatable(file,

  filter = 'top',
  extensions = c('Buttons', 'Scroller'),
  rownames = FALSE,

  options = list(

    searchHighlight = TRUE,
    searching = TRUE,

    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),

    autoWidth = TRUE,
    columnDefs = list(list(className = 'dt-center', targets ="_all")),
    deferRender=TRUE,
    scrollY = 400,
    scrollX = TRUE,
    scroller = TRUE,

    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ))


})





})
