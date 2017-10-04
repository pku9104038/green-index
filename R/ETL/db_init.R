##############################################

library(yaml)
library(readr)
library(readxl)
library(plyr)

db.init <- function(){
  # init global configurations
  conf <- yaml.load_file("yaml/conf.yaml")
  g.dir <- conf$dir
  g.yaml <- conf$yaml
  data.source <- yaml.load_file(paste0(g.dir$yaml,g.yaml$source))$datasource
  data.dir <-  g.dir$data.in
  dbname <- conf$db$dbname
  # source scripts
  source(paste0(g.dir$R,"ETL/db.R"))
  
  #data <- read_csv(file = paste0(getwd(),"/",data.dir,dbname,"/","20170410  校长问卷调查结果.csv") )
  #data[,"xxdm"]  <-  sapply(data[,"xxdm"],as.character)
  #db.WriteTable(table = "test", data = data)
  
  if(length(data.source)>0){
    for(i in 1:length(data.source)){
      table <- data.source[[i]]$table
      file <-  data.source[[i]]$file
      format <- data.source[[i]]$format
      columns <- data.source[[i]]$column
      
      timestamp()
      print(paste("Read",file))
      data <- data.frame()
      if(format ==  "xlsx"){
        data <- read_xlsx(path = paste0(getwd(),"/",data.dir,dbname,"/",file),  sheet = "Sheet1")
      }
      else if(format == "csv"){
        data <- read_csv(file = paste0(getwd(),"/",data.dir,dbname,"/",file) )
      }
      
      timestamp()
      if(length(columns)>0){
        for(j in 1:length(columns)){
          name <- columns[[j]]$name
          newname <- columns[[j]]$newname
          type <-  columns[[j]]$type
          if(is.null(newname)){
            newname  <-  name
          }
          if(is.null(type)){
            data[,newname] <- data[,name]
          }
          else{
            if(type == "text"){
              data[,newname] <- sapply(data[,name],as.character)
            }
            else if(type == "integer"){
              data[,newname] <- sapply(data[,name],as.integer)
            }
            else if(type == "double"){
              data[,newname] <- sapply(data[,name],as.double)
            }
            else{
              data[,newname] <- data[,name]
            }
          }
          if(name != newname){
            data[,name] <- NULL
          }
          
        }
      }
      
      timestamp()
      print(paste("Write",table))
      db.WriteTable(table = table, 
                    data = data)
      timestamp()
    }
  }
  
}

#####################

