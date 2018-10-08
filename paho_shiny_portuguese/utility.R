### utility constants for the paho shiny app

# help page text
HELP = function(x){
  switch(x,
         main_help = showModal(modalDialog(
           title = "Ajuda",
           p("Ao passar o rato por cima de cada país, informaçāo sobre esse país e sobre
incursões identificadas será mostrada no gráfico do lado direito. Ao clicar num país, a
página desse país irá aparecer e será apresentado o seu progresso em direçāo à eliminaçāo.
Se aumentar o zoom, um clique duplo restabelecerá o nível de zoom. Os dados só estāo
disponíveis para os países com margens a preto (neste momento somente México e Brazil.")
        )),
         country_help = showModal(modalDialog(
           title = "Ajuda",
           p("Passar o rato pelo estado mostra a classificaçāo atual para esse estado e as
incursões identificadas para esse estado comparadas com as incursões identificadas no país
inteiro (lado direito do gráfico).  Clique num estado para visitar a página desse estado e
o seu guia de gestāo. Um clique duplo restabelecerá o nível de zoom.")
         )),

         state_help = showModal(modalDialog(
           title = "Ajuda",
           p("Passar o rato pelo estado mostra a classificaçāo atual para esse estado. O gráfico
do lado direito mostra as incursões detetadas para esse estado e as diferentes cores representam
o progresso ao longo do tempo.  Clicando num ponto da série temporal mostrará a classificaçāo
para esse período de tempo para todos os estados de acordo com o esquema de cores (consultar
chave). Um clique duplo restabelecerá o nível de zoom.")
         ))
  )
}


# function to generate dynamic recommendations
## format:
## classification = text
RECOMMENDER = function(class){
  switch(class,
         "Endemic" = p(h5("Atualmente, o estado é classificado como", strong("ENDÊMICO:")),
"Monitorar a implementaçāo de campanhas de vacinaçāo e de cobertura para identificar áreas
que necessitam melhoramento. Assegurar", ">70%","de cobertura anualmente em todas as comunidades
com acesso a vacinas de alta qualidade. Os programas atuais nāo sāo suficientemente efetivos
para controlar a raiva."
           ),

         "Declining" = p(h5("Atualmente, o estado é classificado como", strong("DECLINANTE:")),
"Continuar com os programas atuais, que estāo a funcionar!
Desenvolver planos para ", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("melhorar a vigilância")),
"e para", tags$a(href = "http://www.sciencedirect.com/science/article/pii/S0147957112001221", strong("preparar estratégia de resposta de emergência e manter status livre."))),

         "Intermittent" = p(h5("Atualmente, o estado é classificado como ", strong("INTERMITENTE:")),
           "Melhorar vigilância para determinar se: ",
                            tags$ul( style = "list-style-type:none",
                              tags$li("1) a transmissão é endêmica mas a vigilância é pobre;"),
                              tags$li("2) a transmissāo é interrompida mas as incursões sāo frequentes;"),
                              tags$li("3) outras variantes sāo responsáveis por casos em cāes.")
                            ), "Os casos devem ser sequenciados e as localizações devem ser gravadas para diferenciar estes cenários.
Para 1) melhorar a vacinaçāo de modo a assegurar uma grande cobertura em todas as comunidades com vacinas de alta qualidade;
Para 2) um controlo direto de populaçōes problemáticas e de alto-risco; Para 3) manter os atuais esforços de vacinaçāo de cāes."
         ),

         "Absent-Vulnerable" = p(h5("Atualmente, o estado é classificado como ", strong("AUSÊNCIA-VULNERÁVEL:")),
"Manter os esforços de controle enquanto o risco de incursāo existir.
Implementar ou manter vigilância para uma deteçāo rápida de novos casos.
Preparar estratégia de resposta de emergência e revisar esta estratégia depois de novas incursões.
Sequenciar todos os casos para identificar as suas variantes e origem.

Recolher provas para ", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("verificar ausência")),
" de raiva canina!"),

         "Absent" = p(h5("Atualmente, o estado é classificado como",strong("AUSÊNCIA:")),
"Manter os esforços de vigilância.
Sequenciar todos os casos para identificar as suas variantes e origem.
Submeter provas para", tags$a(href = "http://www.biorxiv.org/content/early/2016/12/31/096883", strong("verificar ausência")),
" de raiva canina!")
  )
}

## main page bottom text
GLOBAL_ABOUT = div(br(), p(h4(strong("Sobre esta ferramenta:")),
"Programas regionais de controlo de raiva têm sido coordinados nas Américas desde os anos 80 pela ",
tags$a(href = "http://www.paho.org/hq/index.php?option=com_topics&view=article&id=46&Itemid=40766&lang=en", strong("PAHO/Panaftosa,")),
" com o objetivo de eliminar a raiva canina até ",
tags$a(href = "https://www.frontiersin.org/articles/10.3389/fvets.2017.00004/full", strong("2022.")),
"Esta página de internet mostra o desempenho de programas regionais, o progresso nacional e sub-nacional
para alcançar eliminaçāo, e guias de gestāo para cada regiāo. Use o botāo de ajuda (à direita em cima) para
mais informaçāo sobre como explorar esta página. Ao passar o rato por cima de cada país no mapa à esquerda,
serà mostrada informaçāo sobre esse país e sobre os casos detectados desde 2005 (gráfico à direita), em
relaçāo a todos os casos encontrados na regiāo. Selecione um país para explorar (os detalhes da sua situaçāo
atualmente somente dados para o Brazil e México serāo mostrados).
Para mais informaçāo sobre esta ferramenta, baixe o nosso estudo (acima à esquerda), e para mais informações sobre o programa
de controle regional visite as ",
tags$a(href = "http://www.panaftosa.org/redipra16/index.php?lang=en", strong("páginas REDIPRA.")),
"",
p(h5(tags$a(href = "https://boydorr.shinyapps.io/paho_rabies/", "English,          "),
  tags$a(href = "https://boydorr.shinyapps.io/paho_rabies_spanish/", "Spanish,          "),
  tags$a(href = "https://boydorr.shinyapps.io/paho_rabies_portuguese/", "Portuguese         "),
  align = "center"))
))

## country page bottonm left text
COUNTRY_ABOUT = p("Usando os dados de raiva da base de dados regional",
tags$a(href = "http://sirvera.panaftosa.org.br", strong("SIRVERA,")),
" os estados foram classificados em uma das 5 situações (ver a chave do lado direito).
O mapa mostra a classificaçāo atual para cada estado no país selecionado. Selecione um estado para ver o
seu progresso relativamente à eliminaçāo de raiva canina e as recomendações para a gestāo de raiva nesse estado.")


### titles
# main page
GLOBAL_TITLE = "Progresso para a eliminaçāo da raiva canina na América Latina"
# country page
COUNTRY_TITLE = "Progresso para a eliminaçāo da raiva canina ao nível do país: "
# state page
STATE_TITLE = "Progresso para a eliminaçāo da raiva canina ao nível do estado: "
