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


shinyUI(dashboardPage(skin = "green",


    dashboardHeader(title = "FIELDBOOK"),

# Sider -------------------------------------------------------------------

    dashboardSidebar(

      sidebarMenu(
        menuItem("Presentacion", tabName = "intro", icon = icon("home")),
        menuItem("Fieldbook", tabName = "fieldbook", icon = icon("cog")),
        menuItem("Outliers", tabName = "outlier", icon = icon("leaf")),
        menuItem("Multivariate", tabName = "multv", icon = icon("edit")),
        menuItem("Statistics", tabName = "stat", icon = icon("cloud")),
        menuItem("Graphics", tabName = "graph", icon = icon("tint")),
        menuItem("About", tabName = "info", icon = icon("book"))
      )


    ),


# Iconos :: http://getbootstrap.com/components/#glyphicons-glyphs


    dashboardBody(


      tabItems(


# presentacion ------------------------------------------------------------


        tabItem(tabName = "intro",


                box(
                  title = "Presentacion",
                  width = 6,
                  status = "primary",
                  solidHeader = T,

                p( strong(em("FieldBook")),"is a interactive application for exploratory data analisys and graphics for experimnetal designs"),

                  img(src = "agrinka.jpg",  width = "100%")

                ),


                box(
                  title = "Charateristics",
                  width = 6,
                  status = "danger",
                  solidHeader = T,

                    p("- Import data from excel files and google spreadsheet documents"),

                    p("- Detection of outliers"),

                    p("- Statistical analisys for experimental designs"),

                    p("- Colour and black graphics for publication"),

                    p("- Multivariate analisys: PCA and correlation")


                ),

                box(
                  title = "Contributors",
                  width = 6,
                  status = "success",
                  solidHeader = T,

                  p(
                    strong("Flavio Lozano Isla "),
                    br(),
                    a("< flavjack@gmail.com >"),
                    br(),
                    code("Universidad Nacional Agraria la Molina, Lima, Perú")
                    ),


                  hr(),

                  p(strong("If you have any question, commment or sugestion you can write a email for us, enjoy FIELDBOOK!!"))



                )



        ),



# fieldbook -------------------------------------------------------------


        tabItem(tabName = "fieldbook",


        box(

          status = "info",
          width = 12,
          background = "black",


          column(width = 6,

           h4(icon("book"), "Google SpreadSheet (URL)", width = "100%"),

           textInput("fbdt",
             label = NULL ,
             width = "100%",
             value = "https://docs.google.com/spreadsheets/d/14sO81N50Zx1al5O3Iu3IPaz1_5CVncvtsx-_JRqJ_qE/edit#gid=172957346")


          ),


          column(width = 4,

            h4(icon("book"), "Excel file (.xlsx)", width = "100%"),

            fileInput('impdata',
              label = NULL,
              accept = c(".xlsx"))

          ),

          column(width = 1,

            h4("Sheet", width = "100%"),

            numericInput("sheetdt", label = NULL, value = 1, step = 1, min = 1)

          ),

          column(width = 1,

            h4( "Update", width = "100%"),

            actionButton(inputId = "reload", label = "", icon("refresh"), width = "100%")

          )


        ),


        box(

          status = "danger",
          solidHeader = T,
          width = 12,

        # DT::dataTableOutput('fbook')
        htmlOutput("fbook")


        )


        ),


# outliers ----------------------------------------------------------------

        tabItem(tabName = "outlier",

          box(

            width = 12, background = "black",

                    column(width = 4,

                      uiOutput("bpy")

                    ),


                    column(width = 4,

                      uiOutput("bpx")

                    ),


                    column(width = 4,

                      uiOutput("bpz")


                    ),



                    column(width = 4,

                      textInput(inputId ="bply", label = "Y label", value = "")


                    ),


                    column(width = 4,


                      textInput(inputId ="bplx", label = "X label", value = "")


                    ),


                    column(width = 4,

                      textInput(inputId ="bplz", label = "Legend label", value = "")


                    )

          ),


          box(width = 12,

          plotOutput("boxplot")


          )



        ),


# multivariate ------------------------------------------------------------

        tabItem(tabName = "multv",

         box(width = 6,

           column(width = 3,

             h5(icon("book"), "Correlation", width = "100%")

           ),


           column(width = 3,

            numericInput("corsig",
               label = "Significance",
              value = 0.05,
              min = 0,
              max = 5,
              step = 0.01)

           ),

            column(width = 3,

              selectInput("corcol",
                label = "Color",
                choices = c("palet 01", "palet 02", "palet 03"),
                selected = "palet 03")

            )


          ),


          box(width = 6,

            column(width = 2,

              h5(icon("book"), "PCA", width = "100%")

            ),


            column(width = 3,

              selectInput("pcatype",
                label = "Type",
                choices = c("ind", "var", "biplot"),
                selected = "biplot")

            ),

            column(width = 2,

              numericInput("pcaqs",
                label = "Variable",
                value = NA,
                min = 1,
                step = 1
              )

            ),

            column(width = 5,

              textInput("pcalbl",
                label = "Label",
                value = NA
              )

            )



          ),


          box(width = 6,

            plotOutput("crpt", width = "520px", height = "520px")

          ),

          box(width = 6,


            plotOutput("pca", width = "520px", height = "520px")


          )

        ),


# statistics -------------------------------------------------------------

        tabItem(tabName = "stat",


          box(width = 12, background = "black",

            column(width = 2,

              uiOutput("resp")

            ),

            column(width = 2,

              uiOutput("stv1")

            ),


            column(width = 2,

              uiOutput("stv2")

            ),


            column(width = 2,

              uiOutput("block")


            ),


            column(width = 2,

              numericInput("stsig",
                label = "Significance",
                value = 0.05,
                min = 0,
                max = 5,
                step = 0.01)


            ),

            column(width = 2,

              selectInput("stmc",
                label = "Type",
                choices = c("tukey", "duncan", "snk"),
                selected = "snk")


            )


          ),



          box(width = 5,


          verbatimTextOutput("tbav")


          ),


          box(width = 7,


            DT::dataTableOutput("mnc")


          )


        ),

# graphics ----------------------------------------------------------------


        tabItem(tabName = "graph",


        box(width = 12,


            box(width = 8, title = NULL, background = "blue",


                      column(width = 4,

                        textInput(inputId ="gply", label = "Y label", value = "")


                      ),


                      column(width = 4,

                        textInput(inputId ="gplx", label = "X label", value = "")


                      ),


                      column(width = 2,

                        textInput(inputId ="gplz", label = "Legend", value = "")


                      ),

                      column(width = 2,


                        numericInput(
                          inputId ="gfont",
                          label = "Size",
                          value = 2,
                          min = 1,
                          step = 0.1
                        )

                      )


              ),



            box(width = 4, title = NULL, background = "red",


                    column(width = 4,


                      numericInput(
                        inputId ="gbrakes",
                        label = "Brakes axis",
                        value = NA
                      )

                    ),


                    column(width = 4,


                        numericInput(
                          inputId ="glmti",
                          label = "Initial limit",
                          value = NA
                        )

                      ),


                    column(width = 4,


                      numericInput(
                        inputId ="glmtf",
                        label = "Final limit",
                        value = NA
                      )

                      )




            ),



          column(width = 2,


          radioButtons(
            inputId ="gtype",
            label = "Type",
            choices = c("bar", "line"),
            selected = "bar",
            inline = TRUE)
          ),


          column(width = 2,


            radioButtons(
              inputId ="gcolor",
              label = "Color",
              choices = c("color", "gray"),
              selected = "color",
              inline = TRUE)
          ),

          column(width = 2,


            radioButtons(
              inputId ="gerbr",
              label = "Error",
              choices = c("yes", "no"),
              selected = "yes",
              inline = TRUE)
          ),


          column(width = 2,


            radioButtons(
              inputId ="gsig",
              label = "Significance",
              choices = c("yes", "no"),
              selected = "yes",
              inline = TRUE)
          ),


          column(width = 4,


            radioButtons(
              inputId ="glabel",
              label = "Label",
              choices = c("none", "left", "right", "bottom", "top"),
              selected = "top",
              inline = TRUE)
          )



        ),


        box(width = 12,


            plotOutput("stplot")



        )


        ),



# information -------------------------------------------------------------


        tabItem(tabName = "info",

                h4(strong("FieldBook")),

                p("User Manual"),



                br(),
                br()

        )




      )



    )

  )
)



