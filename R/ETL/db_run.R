##############################################

library(yaml)
library(dplyr)
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml
dbname <- conf$db$dbname
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))

####################################

db.update <- function(table, update){
  data <- db.ReadTable(table = table)
  
  data.option <- data[data[,update$variable]==update$option,]
  data.others <- data[data[,update$variable]!=update$option,]
  data.option <- data[,update$variable] <- update$value
  
  data <- rbind(data.option,data.others)
  db.WriteTable(table = table, data = data)
}

############################################
table <- "校长问卷变量"
update <- list()
update$variable <- "topic"
update$option <- ""
update$value <- ""

db.update(table, update)

############################################
