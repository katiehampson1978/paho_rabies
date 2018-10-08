### utility constants for the paho shiny app

# help page text
HELP = function(x){
  switch(x,
         main_help = showModal(modalDialog(
           title = "Ayuda",
           p("Al pasar el cursor sobre un país, se muestra información relevante para ese país, incluido
casos detectados en el gráfico de la derecha. Al hacer clic en el país lo llevará a la página del país
 y lo muestrará su progreso hace la eliminación. Zoom in en el gráfico arrastrando una región. Haga doble
 clic para restablecer el nivel de zoom. Los datos solo están disponibles para los países
con un contorno más grueso (actualmente solo México y Brasil).")
         )),
         country_help = showModal(modalDialog(
           title = "Ayuda",
           p("Al pasar el cursor sobre un estado, se muestra la clasificación actual para ese estado y su
casos detectados en comparación con los casos detectados en todo el país (gráfico de la derecha). Haga clic en
un estado para ir a la página de estado y a la guía de gestión. Zoom en el gráfico por
arrastrando una región. Haga doble clic para restablecer el nivel de zoom.")
         )),

         state_help = showModal(modalDialog(
           title = "Ayuda",
           p("Al pasar el cursor sobre un estado, se muestra la clasificación actual para ese estado.
El gráfico de la derecha muestra casos detectados para ese estado y está coloreado por el progreso en el tiempo.
Al hacer clic en un punto de la serie temporal, se muestra la clasificación de ese momento para todos los estados
de acuerdo con el esquema de color (ver clave). Amplíe el gráfico arrastrando una región.
Haga doble clic para restablecer el nivel de zoom.")
         ))
  )
}


# function to generate dynamic recommendations
## format:
## classification = text
RECOMMENDER = function(class){
  switch(class,
         "Endemic" = p(h5("El estado se clasifica actualmente como", strong("ENDEMICO: ")),
"Monitorear la implementación y cobertura de la campaña de vacunación para identificar áreas que necesitan de mejora.
Asegurar una cobertura > 70% alcanzada anualmente en todas las comunidades con vacunas de alta calidad.
            Los programas actuales no están funcionando de manera efectiva para controlar la rabia."),

         "Declining" = p(h5("El estado se clasifica actualmente como", strong("EN DECLIVE: ")),
"Continuar con los programas actuales, que estan a funcionar!
Desarrollar planes para", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("mejorar la vigilancia")),
"y para", tags$a(href = "http://www.sciencedirect.com/science/article/pii/S0147957112001221", strong("respuesta de emergencia y certificar libre."))),

         "Intermittent" = p(h5("El estado se clasifica actualmente como", strong("INTERMITENTE:")),
           "Mejorar la vigilancia para comprender si: ",
                            tags$ul( style = "list-style-type:none",
                              tags$li("1) la transmisión es endémica pero la vigilancia es deficiente;"),
                              tags$li("2) la transmisión se interrumpe pero las incursiones son frecuentes;"),
                              tags$li("3) otras variantes están causando casos en perros.")
                            ), "Los casos deben ser secuenciados y las ubicaciones registradas para diferenciar estos escenarios.
For 1) mejorar la vacunación para garantizar una cobertura alta en todas las comunidades con vacunas de alta calidad;
For 2) control directo hacia poblaciones de origen y poblaciones en riesgo;
For 3) mantener el esfuerzo actual de vacunación de perros."
         ),

         "Absent-Vulnerable" = p(h5("El estado se clasifica actualmente como", strong("AUSENTE-VULNERABLE:")),
"Mantenga el esfuerzo de control mientras persisten los riesgos de incursión.
Implementar o mantener una vigilancia mejorada para la detección temprana de cualquier caso introducido.
Prepare una estrategia de respuesta de emergencia y una revisión después de cualquier incursión.
Secuencia todos los casos para identificar su variante y fuente.
Compilar evidencia para", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("certificar libre")),
" de la rabia canina!"),

         "Absent" = p(h5("El estado se clasifica actualmente como", strong("AUSENCIA:")),
"Mantener la vigilancia. Secuenciar casos para identificar su variante y fuente.
Presentar evidencia para", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("certificar libre")),
" de la rabia canina!")
  )
}

## main page bottom text
GLOBAL_ABOUT = div(br(), p(h4(strong("Acerca de esta herramienta:")),
"Los programas regionales de control de la rabia se han coordinado en toda América
desde la década de 1980 coordinado por",
tags$a(href = "http://www.paho.org/hq/index.php?option=com_topics&view=article&id=46&Itemid=40766&lang=en", strong("PAHO/Panaftosa,")),
" y con el objetivo de eliminar la rabia canina hasta",
tags$a(href = "https://www.frontiersin.org/articles/10.3389/fvets.2017.00004/full", strong("2022.")),
"Esta página de internet muestra el desempeño reciente del programa regional, el progreso a nivel nacional y
niveles subnacionales hacia la eliminación y orientación de gestión adaptada a la région. Usa el botón de ayuda
(arriba a la derecha) para obtener información sobre cómo explorar esta página. Desplácese sobre un país en el mapa para mostrar
información sobre el país y los casos detectados desde 2005 (gráfico de la derecha) en relación con los casos
de la región. Seleccione un país para explorar su situación con más detalle (datos actualmente solo
mostrado para México y Brasil). Para obtener más información sobre esta herramienta, descargue nuestro estudio (arriba a la izquierda),
y para obtener más información sobre el programa de control regional visite las páginas",
tags$a(href = "http://www.panaftosa.org/redipra16/index.php?lang=en", strong("REDIPRA.")),
"",
p(h5(tags$a(href = "https://boydorr.shinyapps.io/paho_rabies/", "English,          "),
  tags$a(href = "https://boydorr.shinyapps.io/paho_rabies_spanish/", "Spanish,          "),
  tags$a(href = "https://boydorr.shinyapps.io/paho_rabies_portuguese/", "Portuguese         "),
  align = "center"))
))

## country page bottonm left text
COUNTRY_ABOUT = p("Usando datos de la base de datos regional de rabia",
tags$a(href = "http://sirvera.panaftosa.org.br", strong("SIRVERA,")),
" os estados se clasificaron en 5 situaciones (ver la clave de la derecha). El mapa da la corriente
clasificación para cada estado en el país seleccionado. Seleccione un estado para ver su progreso hacia
eliminación de la rabia canina y recomendaciones para el manejo de la rabia en el estado.")


### titles
# main page
GLOBAL_TITLE = "Progreso hacia la eliminación de la rabia canina en América Latina"
# country page
COUNTRY_TITLE = "Progreso a nivel de país hacia la eliminación de la rabia canina: "
# state page
STATE_TITLE = "Progreso a nivel estatal hacia la eliminación de la rabia canina: "
