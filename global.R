library(shiny)
library(dplyr)
library(ggplot2)
library(RSQLite)
library(DT)
library(shinydashboard)
library(leaflet)

dbname = './meteorites-sqlite'
tablename = 'meteorites'

dbConnector <- function(session, dbname) {

  ## setup connection to database
  conn <- dbConnect(drv = SQLite(), 
                    dbname = dbname)
  ## disconnect database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
  ## return connection
  conn
}

dbGetData = function(conn, tblname, yearstart, yearend, massstart, massend, class, fall) {
  query = paste("SELECT * FROM", tblname, "WHERE",
                "year BETWEEN", yearstart, "AND", yearend, "AND",
                "mass BETWEEN", massstart, "AND", massend, "AND",
                "class IN (", paste0("'", class, "'", collapse=","), ")", "AND",
                "fall IN (", paste0("'", fall, "'", collapse=","), ")")
  as.data.frame(dbGetQuery(conn = conn,
                           statement = query))
}

