


shinyServer(function(input, output, session){
  
  
  conn = dbConnector(session, dbname = dbname)
  
  classes = (dbGetQuery(conn, statement = paste("SELECT DISTINCT(CLASS) FROM", tablename)))
  
  pal = colorFactor(palette = "Spectral", domain = classes[[1]])
  
  dfm = reactive({
    dbGetData(conn = conn,
                  tblname = tablename,
                  yearstart = input$year[1],
                  yearend = input$year[2],
                  massstart = 1,#input$mass[1],
                  massend = 1000000,#input$mass[2],
                  class = input$class)
  })
  
  # show map using leaflet
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>% 
      setView(0,0,zoom=1.5)
  })
  
  observeEvent(input$year,{
    proxy = leafletProxy("map", data = dfm())
    labs = lapply(seq(nrow(dfm())), function(i) {
      paste(  dfm()[i, "name"], '<br>',
              dfm()[i, "class"], '<br>',
              as.integer(dfm()[i, "mass"])/1000,'kg' )
    })
    proxy %>%
      clearMarkers() %>% 
      addCircleMarkers(~reclong, ~reclat,
                       radius = ~ 0.1*mass^0.333,
                       label = lapply(labs, HTML),
                       color = ~ pal(class),
                       opacity = 1,
                       fillOpacity = 0.5)
  })

  output$hist = renderPlot({
    dfm() %>% 
      #head(50000) %>% 
      ggplot(aes(x = year)) + 
      geom_histogram(aes(fill = class), position = 'stack', binwidth = 1) +
      xlim(input$year[1],input$year[2])
  })
  
  # show data using DataTable
  output$table = DT::renderDataTable({
    dfm() %>% 
      select(Name = name, ID = id, Class = class, 'Mass (grams)' = mass, Year = year, GeoLocation) %>% 
      datatable(rownames=FALSE) #%>% 
      #formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
})
