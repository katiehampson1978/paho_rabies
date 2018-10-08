### utility constants for the paho shiny app

# help page text
HELP = function(x){
  switch(x,
         main_help = showModal(modalDialog(
           title = "Help",
           p("Hovering over a country shows relevant information for that country including
detected cases on the right hand graph. Clicking on the country will take you to the country
page and show you progress towards elimination within that country. Zoom in on the graph by
dragging a region. Double-click to reset the zoom level. Data are only available for countries
with a bold outline (currently just Mexico and Brazil).")

         )),
         country_help = showModal(modalDialog(
           title = "Help",
           p("Hovering over a state shows the current classification for that state and its
detected cases compared to cases detected across the country (right hand graph). Click on
a state to go to the state page and management guidance. Zoom in on the graph by
dragging a region. Double-click to reset the zoom level.")
         )),

         state_help = showModal(modalDialog(
           title = "Help",
           p("Hovering over a state shows the current classification for that state. The
right hand graph shows cases detected for that state and is coloured by progress over time.
Clicking a point on the time series shows the classification for that time for all states
according to the color scheme (see key). Zoom in on the graph by dragging a region.
Double-click to reset the zoom level.")
         ))
  )
}


# function to generate dynamic recommendations
## format:
## classification = text
RECOMMENDER = function(class){
  switch(class,
         "Endemic" = p(h5("The state is currently classified as",strong("ENDEMIC:")),
"Monitor vaccination campaign implementation and coverage to identify areas for improvement.
Ensure >70% coverage achieved annually in all communities with high quality vaccines.
            Current programmes are not working effectively enough to control rabies."
           ),

         "Declining" = p(h5("The state is currently classified as", strong("DECLINING:")),
"Continue current programmes, which are working!
Develop plans for ", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("enhancing surveillance")),
"and for", tags$a(href = "http://www.sciencedirect.com/science/article/pii/S0147957112001221", strong("emergency response and maintaining freedom."))),

         "Intermittent" = p(h5("The state is currently classified as",strong("INTERMITTENT:")),
           "Enhance surveillance to understand whether: ",
                            tags$ul( style = "list-style-type:none",
                              tags$li("1) transmission is endemic but surveillance is poor;"),
                              tags$li("2) transmission is interrupted but incursions are frequent;"),
                              tags$li("3) other variants are causing cases in dogs.")
                            ), "Cases should be sequenced and locations recorded to differentiate these scenarios.
For 1) improve vaccination to ensure high coverage in all communities with high quality vaccines;
For 2) direct control towards source populations and at-risk populations; For 3) maintain current dog vaccination effort."
         ),

         "Absent-Vulnerable" = p(h5("The state is currently classified as", strong("ABSENT-VULNERABLE:")),
"Maintain control effort while incursion risks remain.
Implement or maintain enhanced surveillance for early detection of any introduced cases.
Prepare an emergency response strategy and review after any incursions.
Sequence all cases to identify their variant and source.
Compile evidence to", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("verify freedom")),
" from dog-mediated rabies!"),

         "Absent" = p(h5("The state is currently classified as",strong("ABSENT:")),
"Maintain enhanced surveillance.
Sequence any cases to identify their variant and source.
Submit evidence to", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("verify freedom")),
" from dog-mediated rabies!")
  )
}

## main page bottom text
GLOBAL_ABOUT = div(br(), p(h4(strong("About this tool:")),
"Regional rabies control programmes have been coordinated across the Americas
since the 1980s coordinated by",
tags$a(href = "http://www.paho.org/hq/index.php?option=com_topics&view=article&id=46&Itemid=40766&lang=en", strong("PAHO/Panaftosa,")),
" and aim to eliminate dog-mediated rabies by",
tags$a(href = "https://www.frontiersin.org/articles/10.3389/fvets.2017.00004/full", strong("2022.")),
"This site shows the recent performance of the regional programme, progress at national and
subnational levels towards elimination, and locally tailored management guidance. Use the help button
(top right) for information on how to explore the site. Hover over a country on the map to show
information about the country and cases detected since 2005 (right hand graph) in relation to cases
across the region. Select a country to explore their situation in more detail (data currently only
shown for Mexico and Brazil). For more information about this tool, download our study (top left),
and for more information about the regional control programme visit the",
tags$a(href = "http://www.panaftosa.org/redipra16/index.php?lang=en", strong("REDIPRA webpages.")),
"",
p(h5(tags$a(href = "https://boydorr.shinyapps.io/paho_rabies/", "English,          "),
  tags$a(href = "https://boydorr.shinyapps.io/paho_rabies_spanish/", "Spanish,          "),
  tags$a(href = "https://boydorr.shinyapps.io/paho_rabies_portuguese/", "Portuguese         "),
  align = "center"))
))

## country page bottonm left text
COUNTRY_ABOUT = p("Using data from the regional rabies database",
tags$a(href = "http://sirvera.panaftosa.org.br", strong("SIRVERA,")),
" states were classified into 5 situations (see the right hand key). The map gives the current
classification for each state in the selected country. Select a state to see its progress towards
elimination of dog-mediated rabies and recommendations for rabies management in the state.")


### titles
# main page
GLOBAL_TITLE = "Progress towards the Elimination of Dog-Mediated Rabies across Latin America"
# country page
COUNTRY_TITLE = "Country-level progress towards eliminating dog-mediated rabies: "
# state page
STATE_TITLE = "State-level progress towards eliminating dog-mediated rabies: "
