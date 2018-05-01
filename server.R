


shinyServer(function(input, output, session){
  
  
  conn = dbConnector(session, dbname = dbname)
  
  classes = (dbGetQuery(conn, statement = paste("SELECT DISTINCT(CLASS) FROM", tablename)))[[1]]
  
  pal = colorFactor(palette = "Spectral", domain = classes)
  
  # ids = c()
  
  observe ({
    updateCheckboxGroupInput(session, "class",
                      choices = classes,
                      selected = 0
    )
  })
  
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"class",choices=classes)
    }
    else
    {
      updateCheckboxGroupInput(session,"class",choices=classes,selected=classes)
    }
  })
  
  # observe ({
  #   if(input$tabs == "map"){
  #     if(length(input$year) == 0) {
  #       yearstart = 1800
  #       yearend = 2020
  #     } else {
  #       yearstart = input$year[1]
  #       yearend = input$year[2]
  #     }
  #     updateSliderInput(session, "year",
  #                                   label = "Year",
  #                                   min = yearstart,
  #                                   max = yearend,
  #                                   value = c(yearstart, yearend),
  #                                   round = TRUE,
  #                                   sep = '',
  #                                   animate = animationOptions(interval = 200,
  #                                                              playButton = HTML("<h4>Play</h4>")))
  #   } else {
  #     updateSliderInput(session, "year",
  #                                   label = "Year",
  #                                   min = yearstart,
  #                                   max = yearend,
  #                                   value = c(yearstart, yearend),
  #                                   round = TRUE,
  #                                   sep = '',
  #                                   animate = FALSE)
  #   }
  # })
  
  # output$slider = renderUI({
  #   if(input$tabs == "map"){
  #       
  #   } else {
  #       
  #   }
  # })
  
  dfm = reactive({
    if(length(input$year) == 0) {
      yearstart = 1800
      yearend = 2020
    } else {
      yearstart = input$year[1]
      yearend = input$year[2]
    }
    dbGetData(conn = conn,
              tblname = tablename,
              yearstart = yearstart,
              yearend = yearend,
              massstart = 1,#input$mass[1],
              massend = 1000000,#input$mass[2],
              class = input$class,
              fall = c('Fell','Found')) %>% 
      group_by(groupname) %>% 
      mutate(groupmass = sum(mass))
  })
  
  dfmgroups = reactive({
    if(length(input$year) == 0) {
      yearstart = 1800
      yearend = 2020
    } else {
      yearstart = input$year[1]
      yearend = input$year[2]
    }
    dbGetData(conn = conn,
              tblname = tablename,
              yearstart = yearstart,
              yearend = yearend,
              massstart = 1,#input$mass[1],
              massend = 1000000,#input$mass[2],
              class = input$class,
              fall = c('Fell','Found')) %>% 
      group_by(groupname) %>% 
      mutate(groupmass = sum(mass)) %>% 
      filter(mass == max(mass))
  })
  
  # show map using leaflet
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>% 
      setView(0,0,zoom=2)
  })
  
  observe({
    if(input$tabs == "map"){
      if(!is.null(input$map_zoom)){
        if(input$map_zoom > 6)
          df = dfm()
        else
          df = dfmgroups()
      } else {
        df = dfmgroups()
      }
      proxy = leafletProxy("map", data = df)
      # labs = lapply(seq(nrow(dfmgroups())), function(i) {
      #   paste(  dfmgroups()[i, "groupname"], '<br>',
      #           dfmgroups()[i, "class"], '<br>',
      #           as.integer(dfmgroups()[i, "groupmass"])/1000,'kg' )
      # })
      # newids = df[,'id']
      # if (is.null(ids)) {
      #   addids = newids
      # } else {
      #   addids = newids[-which(newids %in% ids)]
      # }
      # removeids = ids[-which(ids %in% newids)]
      # ids = newids
      proxy %>%
        clearMarkers() %>% 
        addCircleMarkers(~reclong, ~reclat,
                         radius = ~ 0.1*groupmass^0.333,
                         # layerId = addids,
                         label = ~ paste0(groupname, ', ', class, ', ', as.integer(groupmass)/1000,' kg'),
                         color = ~ pal(class),
                         weight = 1,
                         opacity = 1,
                         fillOpacity = 0.2)
    }
  })
  
  # observeEvent(input$map_zoom,{
  #   proxy = leafletProxy("map")
  #   if(input$zoom > 5)
  # })

  output$hist = renderPlot({
    if(input$tabs == 'hist'){
      dfm() %>% 
        ggplot(aes(x = year)) + 
        geom_histogram(aes(fill = class), position = 'stack', binwidth = 1) +
        xlim(input$year[1],input$year[2])
    }
  })
  
  # show data using DataTable
  output$table = DT::renderDataTable({
    dfm() %>% 
      select(Name = name, ID = id, Class = class, 'Mass (grams)' = mass, Year = year, GeoLocation) %>% 
      datatable(rownames=FALSE) #%>% 
      #formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
})
