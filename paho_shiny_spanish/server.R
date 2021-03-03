function(input,output,session){
  # hideElement("button")
  output$global_leaf = renderLeaflet(leaf1)
  output$country_leaf = renderLeaflet(NULL)

  ### observation for the help button
  observeEvent(input$help, {
    if(rvs$map_select == "global"){
      HELP("main_help")
    }else {
      if(is.na(rvs$region_select)){ # selected country, but not region
        HELP("country_help")
      }else{
        HELP("state_help")
      }
    }
  })

  ### observation for title(
  observe({
    if(rvs$map_select == "global"){
      output$titleinfo = renderUI({
        div(h3(GLOBAL_TITLE), align = "center")
      })
    }else if(is.na(rvs$region_select)){
      output$titleinfo = renderUI({
        div(align = "center",h3(HTML(paste0(COUNTRY_TITLE, em(rvs$map_select)))))
      })
    }else{
      output$titleinfo = renderUI({
        div(align = "center", h3(HTML(paste0(STATE_TITLE, em(rvs$region_select)))))
      })
    }
  })

  ## default time series view
  output$dygraph = renderDygraph(default_dygraph())

  rvs = reactiveValues(
    map_select = "global",
    region_select = NA,
    region_dy_clicked = NA
    )

  ## event clicking on the global map
  observeEvent(input$global_leaf_shape_click,{
    ## country selection updates the viewable map
    if(!is.null(input$global_leaf_shape_click$id) && input$global_leaf_shape_click$id %in% country_names){
      rvs$map_select = input$global_leaf_shape_click$id
      hideElement("global_map")
      showElement("country_map")

      # showElement("button")
      output$dygraph = renderDygraph(country_default_dygraph(input$global_leaf_shape_click$id))
    }
  })

  ## event for clicking on the country map
  observeEvent(input$country_leaf_shape_click,{
      rvs$region_select = input$country_leaf_shape_click$id
  })

  ## event for reset map button
  observeEvent(input$reset_map,{
    session$reload()
  })

  ### observations for map controls and rerendering of time series
  observe({
    output$country_leaf = renderLeaflet(leaflist[[rvs$map_select]])
    if(rvs$map_select == "global"){
      # hideElement("button")
      hideElement("country_map")
      showElement("global_map")
      output$tsblock =  renderUI({
        GLOBAL_ABOUT
      })
    }
    else{
      hideElement("global_map")
      showElement("country_map")
      if(is.na(rvs$region_select)){
        output$tsblock = renderUI({
          div(
            fluidRow(column(width = 5,br(), COUNTRY_ABOUT),
                     column(width = 7, br(), div(align = "center",
                                           img(src = "ClassificationKey_spanish.png")
                                           #HTML(CLASS_TABLE)
                     )))#,
            # fluidRow(column(width = 10, offset = 1, br(),
            #                   h4("Using ", strong("SIRVERA"), " data, states were classified using an ", strong("epidemiological analysis"), " as:"), br(),
            #                   div(align = "center",
            #                       img(src = "ClassificationKey2.png")
            #                       #HTML(CLASS_TABLE)
            #                       ),br()
              )

        })
      }else{
        output$tsblock = renderUI({
          current_country = rvs$region_select
          class = (tsdatalist[[rvs$map_select]] %>% filter(State == rvs$region_select) %>% filter( min_rank(desc(Date)) == 1))$Classification
          text = RECOMMENDER(class)
          div(fluidRow(column(width = 5,br(), text),
                   column(width = 7, br(), div(align = "center",
                                               img(src = "ClassificationKey_spanish.png")
                                               #HTML(CLASS_TABLE)
                   )))
          )

          # div(fluidRow(column(width = 10, offset = 1, br(),
          #                     h4("Using ", strong("SIRVERA"), " data, states were classified using an ", strong("epidemiological analysis"), " as:"), br(),
          #                     div(align = "center",
          #                         img(src = "ClassificationKey2.png")
          #                         #HTML(CLASS_TABLE)
          #                         ),br()
          #     ))
          # )
        })

        leafletProxy("country_leaf") %>% removeShape("highlighted") %>%
        addPolylines(data = poly_list[[rvs$map_select]][poly_list[[rvs$map_select]]$ADM1_NAME == rvs$region_select,],
                     color = "red", layerId = "highlighted")

          output$dygraph = renderDygraph(tsplot(tsdatalist[[rvs$map_select]],rvs$region_select))
      }
    }
  })

  observe({lapply(rvs,print)})

  ######## observation for adding a popup with info
  observe({
    region_hover = input$country_leaf_shape_mouseover$id
    # if(rvs$map_select == "global") return()
    dat = tsdatalist[[rvs$map_select]]
    cclass = dat$Classification[dat$State == region_hover & dat$Date == max(dat$Date)]
    leafletProxy("country_leaf") %>%
      removeControl("info") %>%
      addControl(position = "bottomleft",
                 paste0("<ul><li> Nombre del país: ", rvs$map_select,
                        "</li><li> Nombre del Estado: ", region_hover,
                        "</li><li> Clasificación actual: ", cclass,
                        "</li>"), layerId = "info")

    ## update the time series plot
    if(!is.null(region_hover) &&  region_hover != "highlighted"){ ## this check is needed, annoyingly the polylines highlighting has the same
      # shiny binding as the filled polygons. Without this check an error raises when the mouse hovers over
      # the red highlighted line.
      if(is.na(rvs$region_select)){ # otherwise the hover anim will take over the selection
        output$dygraph = renderDygraph({
          df = get_region_monthly(rvs$map_select, region_hover)
          id = which(as.Date(rownames(df)) >= "2005-01-01")

          #edge case where state mexico in mexico causes a problem
          if(rvs$map_select == region_hover) region_hover = paste0(region_hover, " ")

          dygraph(df[id,,drop = FALSE], main = paste0("Casos mensuales de rabia en ", region_hover)) %>%
            dySeries(rvs$map_select, color = "grey", strokeWidth = 1) %>%
            dySeries(region_hover, color = "black", strokeWidth = 2) %>%
            dyOptions(drawGrid = FALSE)%>%
           DYRANGE
        })
      }
    }

  })

  ######## observation for hovering on main map
  observe({
    country_hover = input$global_leaf_shape_mouseover$id

    if(!is.null(country_hover) ){

        leafletProxy("global_leaf") %>%
          removeControl("info") %>%
          addControl(position = "bottomleft",
                     country_info(country_hover), layerId = "info")
      if(country_hover %in% country_names){
        output$dygraph = renderDygraph({
          df = get_country_yearly(country_hover)
          id = which(as.Date(rownames(df)) >= "2005-01-01")
          dygraph(df[id,,drop = FALSE], main = paste0("Casos anuales de rabia en ",country_hover)) %>%
            DYRANGE %>%
            dySeries("Latin America", color = "grey", strokeWidth = 1) %>%
            dySeries(country_hover, color = "black", strokeWidth = 2) %>%
            dyOptions(drawGrid = FALSE)
        })
      }else{
        output$dygraph = renderDygraph(default_dygraph())
      }

    }else{
      output$dygraph = renderDygraph(default_dygraph())
    }
  })

  ### observation for selecting time series point
  observeEvent(input$dygraph_click$x_closest_point,{
    clicked_date = input$dygraph_click$x_closest_point
    # disable time series click map shading when not on a selected region
    if(rvs$map_select == "global" || is.na(rvs$region_select)){
      return()
    }
    if(!is.null(clicked_date)){
      rvs$region_dy_clicked = 1 # using this to control whether date window or click has control over the shading on map
      date = as.Date(paste0(substr(clicked_date,1,7),"-01"))
      classifications = filter(tsdatalist[[rvs$map_select]],Date == date) %>% select(ADM1_NAME = State,Classification)
      pdat = dat[dat$CNTRY_NAME == rvs$map_select,]
      # pdat = pdat
      pdat@data = data.frame(pdat@data,
                             Classification = classifications$Classification[match(pdat@data[,"ADM1_NAME"],
                                                            classifications$ADM1_NAME)]
                             ) #%<>% left_join(classifications,by = "ADM1_NAME")
      leafletProxy("country_leaf") %>%
        clearShapes() %>%
        addPolygons(weight = 2, fillOpacity = 0.6, color = "black",
          fillColor = ~colour_pal(Classification),label = ~ADM1_NAME, layerId = ~ADM1_NAME,
                    data = pdat) %>%
        removeControl("datecontrol") %>%
        addControl(position = "topright", paste0("Mostrando classificaciones para <br/>",date),
                   layerId = "datecontrol")  %>%
        addPolylines(data = poly_list[[rvs$map_select]][poly_list[[rvs$map_select]]$ADM1_NAME == rvs$region_select,],
                     color = "red", layerId = "highlighted")
    }
  })


  ### observation for recommendation
  # observe({
  #   # which time series to look at
  #   current_country = rvs$region_select
  #   if(is.na(rvs$region_select)){
  #     output$recommendation = renderUI({NULL})
  #   }else{
  #     # get classification
  #     class = (tsdatalist[[rvs$map_select]] %>% filter(State == rvs$region_select) %>% filter( min_rank(desc(Date)) == 1))$Classification
  #     text = RECOMMENDER(class)
  #     output$recommendation = renderUI({
  #       div(align = "center",
  #         h4("Based on the current classification, management recommendations are that:"),
  #         br(),
  #         text
  #       )
  #
  #     })
  #   }


  # })

  ### debounced observation for end of dygraphs
  debounced = debounce(input$dygraph_date_window[[2]], 150)
  observe({
    dmax = debounced()
    print(dmax)
    if(!is.na(rvs$region_select) ){ # only allow event when we are on the region map
      date = as.Date(paste0(substr(dmax,1,7),"-01"), format = "%Y-%m-%d")

      ## the axis padding forces this to be invalid at the default axis end date
      # so on default force date to be the last date in the data set.
      date = min(date, max(tsdatalist[[rvs$map_select]]$Date))

      classifications = filter(tsdatalist[[rvs$map_select]],Date == date) %>% select(ADM1_NAME = State,Classification)
      pdat = dat[dat$CNTRY_NAME == rvs$map_select,]
      # pdat = pdat
      pdat@data = data.frame(pdat@data,
                             Classification = classifications$Classification[match(pdat@data[,"ADM1_NAME"],
                                                                                   classifications$ADM1_NAME)]
      )
      leafletProxy("country_leaf") %>%
        clearShapes() %>%
        addPolygons(weight = 2, fillOpacity = 0.6, color = "black",
                    fillColor = ~colour_pal(Classification),label = ~ADM1_NAME, layerId = ~ADM1_NAME,
                    data = pdat) %>%
        removeControl("datecontrol") %>%
        addControl(position = "topright", paste0("Mostrando classificaciones para <br/>",date),
                   layerId = "datecontrol") %>%
        addPolylines(data = poly_list[[rvs$map_select]][poly_list[[rvs$map_select]]$ADM1_NAME == rvs$region_select,],
                     color = "red", layerId = "highlighted")
    }
  })

  output$download = downloadHandler(
    filename = function(){
      paste0("LAC_Rabies_Elimination_Classification",Sys.Date(),".pdf")
    }, content = function(file){
      file.copy("www/LAC_Rabies_Elimination_Classification_MS.pdf", file)
    }
    )
}
