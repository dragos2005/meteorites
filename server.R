


shinyServer(function(input, output, session){
  
  
  conn = dbConnector(session, dbname = dbname)
  
  classes = (dbGetQuery(conn, statement = paste("SELECT DISTINCT(CLASS) FROM", tablename)))[[1]]
  
  pal = colorFactor(palette = "Spectral", domain = classes)
  
  # ids = as.character(c())
  
  # initial checkbox values
  observe ({
    updateCheckboxGroupInput(session, "class",
                      choices = classes,
                      selected = 0
    )
  })
  
  # select all button
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
  
  # dataframe not grouped into mass groups
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
              fall = input$fall) %>% 
      group_by(groupname) %>% 
      mutate(groupmass = sum(mass))
  })
  
  # dataframe grouped into mass groups
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
              fall = input$fall) %>% 
      group_by(groupname) %>% 
      mutate(groupmass = sum(mass)) %>% 
      filter(mass == max(mass))
  })
  
  # show map using leaflet
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>% 
      setView(0,0,zoom=2)
  })
  
  # change map when input changes
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
      # df$id = as.character(df$id)
      # df = df %>% 
      #   mutate(id1 = 100000+)
      proxy = leafletProxy("map", data = df)
      # labs = lapply(seq(nrow(dfmgroups())), function(i) {
      #   paste(  dfmgroups()[i, "groupname"], '<br>',
      #           dfmgroups()[i, "class"], '<br>',
      #           as.integer(dfmgroups()[i, "groupmass"])/1000,'kg' )
      # })
      # newids = as.character(df$id)
      # if (length(ids) == 0) {
      #   addids = newids
      # } else {
      #   addids = newids[!(newids %in% ids)]
      # }
      # if (length(newids) == 0) {
      #   removeids = ids
      # } else {
      #   removeids = ids[!(ids %in% newids)]
      # }
      # ids = newids
      # addids = df$id
      # dfadd = df[df$id %in% addids,]
      proxy %>%
        clearMarkers() %>% 
        addCircleMarkers(df$reclong, df$reclat,
                         radius = 0.1*df$groupmass^0.333,
                         # layerId = as.character(dfadd$id),
                         label = paste0(df$groupname, ', ', df$class, ', ', as.integer(df$groupmass)/1000,' kg'),
                         color = pal(df$class),
                         weight = 1,
                         opacity = 1,
                         fillOpacity = 0.2) # %>%
        # removeMarker(layerId = removeids)
    }
  })
  
  # observeEvent(input$map_zoom,{
  #   proxy = leafletProxy("map")
  #   if(input$zoom > 5)
  # })
  
  # render histogram
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
      select('Group Name' = groupname, Name = name, ID = id, Class = class, 'Mass (grams)' = mass, Year = year, GeoLocation) %>% 
      datatable(rownames=FALSE) #%>% 
      #formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
})
