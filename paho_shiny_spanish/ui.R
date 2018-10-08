fluidPage(
  ## additional custom style info
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(20%);;
             left: calc(20%);;
             right: calc(20%);;
      }

            img {
              max-width: 100%
            }"
      )
    )),

  useShinyjs(),

fluidRow(column(width = 2, br(), downloadButton("download","Descargar artículo")),
    column(width = 7, uiOutput("titleinfo")),
           column(width = 1, br(), actionButton("help", label = "Ayuda", icon = icon("question",lib = "font-awesome"))),
           column(width = 2, br(), actionButton("reset_map", "Reiniciar", icon = icon("refresh", lib = "glyphicon")))),
  br(),
  fluidRow(
    column(width = 5,
           div(id="global_map",leafletOutput("global_leaf")),
           div(id = "country_map", leafletOutput("country_leaf"))
    ),
    column(width = 7,dygraphOutput(
      "dygraph"
    ))
  ),
fluidRow(uiOutput("recommendation")),
br(),
  fluidRow(column(width = 12,  uiOutput("tsblock"))),
br(),
  hr(),
  div(class="footer", img(src = "logos.png"),align="center")
)
