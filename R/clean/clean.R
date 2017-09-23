#############################
library(yaml)

conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))

############################
clean.rename <-  function(
  data,
  clean){
  data.out <- data
  columns <-clean$column
  n <- length(columns)
  if(n>0){
    for(i in 1:n){
      old <- columns[[i]]$old
      new <- columns[[i]]$new
      data.out[,new] <- data[,old]
      data.out[,old] <-  NULL
    }
  }
  
  return(data.out)
}
#############################
clean.drop <-  function(
  data,
  clean){
  data.out <- data
  columns <-clean$column
  n <- length(columns)
  if(n>0){
    for(i in 1:n){
      data.out[,columns[[i]]] <-  NULL
    }
  }
  
  return(data.out)
}
#############################
clean.correct <-  function(
  data,
  clean){
  data.out <- data
  corrects <-clean$correct
  n <- length(corrects)
  if(n>0){
    for(i in 1:n){
      columns <- corrects[[i]]$column
      values <- corrects[[i]]$value
      for(j in 1:length(columns)){
        column <- columns[[j]]
        for(k in 1:length(values)){
          value <- values[[k]]
          old <- value$old
          new  <- value$new
          data.out[data.out[,column]==old,column] <- new
        }
      }
    }
  }
  
  return(data.out)
}
#############################
clean.subject <-  function(
  subject,
  algorithms
){
  
  table.in <- subject$clean$table.in
  table.out <- subject$clean$table.out
  timestamp()
  print(paste("Read Table:",table.in))
  data <- db.ReadTable(table.in)

  cleans <- subject$clean$clean
  n_clean <- length(cleans)
  if(n_clean>0 && subject$process){
    
    for(i in 1:n_clean){
      
      clean <- cleans[[i]]
      algorithm <- clean$proc
      
      if(algorithm == algorithms$drop){
        
        data <- clean.drop(data, clean)
      }
      else if(algorithm == algorithms$rename){
        
        data <- clean.rename(data, clean)
      }
      else if(algorithm == algorithms$correct){
        
        data <- clean.correct(data, clean)
      }
      
    }
  }
  
  timestamp()
  print(paste("Write Table:",table.out))
  db.WriteTable(data =  data,  table = table.out)
}