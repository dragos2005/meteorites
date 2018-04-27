library(dplyr)
library(RSQLite)

## read csv
meteorites = read.csv('meteorite-landings.csv')

## add new column to simplify class values
dfm = meteorites
dfm = dfm %>% 
          mutate(class = 
                   ifelse(
                     substr(recclass, 1, 1) == 'C',
                     'Carbonaceous Chondrite',
                     ifelse(
                       substr(recclass, 1, 1) == 'I',
                       'Iron',
                       ifelse(
                         substr(recclass, 1, 5) == 'Lunar',
                         'Lunar',
                         ifelse(
                           substr(recclass, 1, 7) == 'Martian',
                           'Martian',
                           ifelse(
                             substr(recclass, 1, 2) == 'LL',
                             'LL Chondrite',
                             ifelse(
                               substr(recclass, 1, 1) == 'L',
                               'L Chondrite',
                               ifelse(
                                 substr(recclass, 1, 1) == 'H',
                                 'H Chondrite',
                                 ifelse(
                                   substr(recclass, 1, 9) == 'Pallasite',
                                   'Pallasite',
                                   ifelse(recclass == 'Mesosiderite',
                                          'Mesosiderite',
                                          'Other')
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                   )) %>% 
      filter(year>=1800 & year<=2016) %>% 
      filter(reclong<=180 & reclong>=-180 & (reclat!=0 | reclong!=0)) %>% 
      mutate(groupname = gsub('[0-9]+', '', name))



## connect to database
conn <- dbConnect(drv = SQLite(), 
                  dbname = './meteorites-sqlite')

## write table
dbWriteTable(conn = conn,
             name = 'meteorites',
             value = dfm)

## list tables
dbListTables(conn)

## disconnect
dbDisconnect(conn)
