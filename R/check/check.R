#############################
library(yaml)

conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))

##########################
mini.loop <- function(
  loop.limit = FALSE,
  loop
){
  if(loop.limit){
    return(min(1,loop))
  }
  else{
    return(loop)
  }
}

##########################
check.subject <-  function(
  review = FALSE,
  loop.limit = FALSE,
  subject
){
  
  if(review){
    table <- subject$table$clean
  }
  else{
    table <- subject$table$origin
  }
  data <- db.ReadTable(table)
  
  checks <-  subject$check
  
  #n_check<-length(checks)
  n_check <- mini.loop(loop.limit = loop.limit,  loop = length(checks))
  for(i in 1:n_check){
    sets <- checks[[i]]$set
    n_set <- length(sets)
    
    options.data <- data.frame()
    options <- c()
    n <- 0
    max_n  <- 0
    for(l in 1:n_set){
    
      column <- sets[[l]]
      option <- levels(factor(data[,column]))
      n <- length(option)
      max_n <- max(n,max_n)
      options.data[l,"column"] <- column
      for(m in 1:n){
        if(!is.element(option[m], options)){
          options[length(options)+1] <- option[m]
        }
        options.data[l,option[m]] <- TRUE
        
      }
    }
    if(length(options)>max_n){
      print(options.data)  
    }
    else{
      print(options)
    }
    
    
  }
}