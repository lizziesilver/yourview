# set up DB connection
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "yourview",
                 host = "localhost", port = 5432,
                 user = "esilver", password = "")

# read Chris' SQL query
fileName <- '~/Documents/2017-02_YourView_data_extraction/comments.sql'
comments.query <- readChar(fileName, file.info(fileName)$size)

# pull comments from DB
comments.df <- dbGetQuery(con, comments.query)

# read new SQL query
fileName <- '~/yourview/SQL/comments_new.sql'
comments.query <- readChar(fileName, file.info(fileName)$size)

# pull comments from DB
comments.df <- dbGetQuery(con, comments.query)
