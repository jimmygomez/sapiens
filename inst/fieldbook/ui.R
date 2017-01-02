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
          background = "blue",

          column(width = 3,


                 h4("Google spreadsheet (URL)", icon("book"), width = "100%")


          ),

          column(width = 7,

                 textInput("wtdt", label = NULL, width = "100%",
                           value = "")
            # C:\Users\Flavio\Documents\sapiens\exdata\germination.xlsx

          ),

          column(width = 2,

                 actionButton(inputId = "reload", label = "update", icon("refresh"), width = "100%")

          )


        ),


        box(

          status = "danger",
          solidHeader = T,
          width = 12,

        DT::dataTableOutput('fbook')
        # htmlOutput("gss")


        )


        ),


# outliers ----------------------------------------------------------------

        tabItem(tabName = "outlier",

          box(

            width = 12,

                    column(width = 4,

                      uiOutput("bpx")

                    ),


                    column(width = 4,

                      uiOutput("bpy")

                    ),


                    column(width = 4,

                      uiOutput("bpz")


                    ),



                    column(width = 4,

                      textInput(inputId ="bplx", label = "X label", value = "")

                    ),


                    column(width = 4,

                      textInput(inputId ="bply", label = "Y label", value = "")

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

          column(width = 6,

            plotOutput("crpt")

          ),

          column(width = 6,


            plotOutput("pca")


          )

        ),


# statistics -------------------------------------------------------------

        tabItem(tabName = "stat",


          box(width = 12,

            column(width = 3,

              uiOutput("resp")

            ),

            column(width = 3,

              uiOutput("stv1")

            ),


            column(width = 3,

              uiOutput("stv2")

            ),


            column(width = 3,

              uiOutput("block")


            ),



          box(width = 5,


          verbatimTextOutput("tbav")


          ),


          box(width = 7,


            DT::dataTableOutput("mnc")


          )


          )


        ),

# graphics ----------------------------------------------------------------


        tabItem(tabName = "graph",


        box(width = 12,


          column(width = 4,

            textInput(inputId ="gplx", label = "X label", value = "")

          ),


          column(width = 4,

            textInput(inputId ="gply", label = "Y label", value = "")

          ),


          column(width = 4,

            textInput(inputId ="gplz", label = "Legend label", value = "")


          )

        ),


        box(width = 12,

            plotOutput("grplot")

        )


        ),



# information -------------------------------------------------------------


        tabItem(tabName = "info",

                h4(strong("ELISIOS")),

                p("Elisios, is a free application for calculete que irrigation requirments accodirng the weather conditions, It allows to
                  connect at remote arduino divices"),



                br(),
                br()

        )




      )



    )

  )
)



