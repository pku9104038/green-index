#############################
library(yaml)
library(dplyr)
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))

############################
variable.constant <- function(
  data,
  variable                            
){
  data.out <- data
  var.name <- variable$var.name
  var.value <-  variable$var.value
  
  print(paste("Constant", var.name))
  
  data.out[,var.name] <- var.value
  #print(nrow(data.out))
  return(data.out)
}

############################
variable.polymer <- function(
  data,
  variable                            
){
  data.out <- data
  
  columns <- variable$var.column
  threshold <- variable$var.threshold + 0.5
  var.name<- variable$var.name
  options <- variable$var.option
  
  print(paste("Polymer", var.name, Sys.time()))
  
  col.tmp <- "tmp"
  data.out[,col.tmp] <- 0
  
  if(length(columns) >0){
    for(i in 1:length(columns)){
      
      col.names <- columns[[i]]$col.name
      values <- columns[[i]]$col.value
        
      if(length(col.names) > 0 & length(values)>0){
        for(j in 1:length(col.names)){
          col.name <- col.names[[j]]
          
          for(k in 1:length(values)){
            value <- values[[k]]
            
            data.na<- subset(data.out, is.na(data.out[,col.name]))
            if(nrow(data.na)>0){
              
              data.notna <- subset(data.out, !is.na(data.out[,col.name]))
              data.value <- data.notna[ data.notna[,col.name] == value,]
              data.notvalue <- data.notna[data.notna[,col.name] != value,]
              data.value[, col.tmp] <- data.value[, col.tmp] + 1
              
              data.notna <- bind_rows(data.value, data.notvalue)
              data.out <- bind_rows(data.na,data.notna)
            }
            else{
              data.out[data.out[,col.name] == value,col.tmp] <- 
                data.out[data.out[,col.name] == value,col.tmp] + 1
            }
            
            
          }
        }
          
      }
        
    }
      
    data.out[,var.name] <- options$F
      
    data.out[data.out[,col.tmp] > threshold, var.name] <- options$T
    
    data.out[,col.tmp] <- NULL
      
  }

  #print(nrow(data.out))
  return(data.out)  
}

############################
variable.derivative <- function(
  data,
  variable                            
){
  
  data.out <- data
  
  var.name <- variable$var.name
  columns <-variable$var.column
  options <- variable$var.option
  for(i  in 1:length(columns)){
    data.out[,columns[[i]]] <- ""
  }
  print(paste("Derivative", var.name,Sys.time()))
  
  groups  <- levels(factor(data.out[,var.name]))
  for(i in 1:length(groups)){
    group <- groups[i]
    select.items<-unlist(strsplit(group,"┋"))
    if(length(select.items)>0){
      for(j in 1:length(options)){
        if(is.element(options[[j]],select.items)){
          data.out[data.out[,var.name] == group, columns[[j]] ] <- options[[j]]
        }
      }
    }
  }
  #print(nrow(data.out))
  return(data.out)  
}

############################
variable.classify <- function(
  data,
  variable                            
){
  data.out <- data
  
  columns <- variable$var.column
  var.name<- variable$var.name
  default <- variable$var.default
  
  print(paste("Classify", var.name, Sys.time()))
  
  data.out[,var.name] <- default
  
  if(length(columns) >0){
    for(i in 1:length(columns)){
      
      col.names <- columns[[i]]$col.name
      values <- columns[[i]]$col.value
      class <- columns[[i]]$col.class
      
      if(length(col.names) > 0 & length(values)>0){
        for(j in 1:length(col.names)){
          col.name <- col.names[[j]]
          
          for(k in 1:length(values)){
            value <- values[[k]]
            
            data.na<- subset(data.out, is.na(data.out[,col.name]))
            if(nrow(data.na)>0){
              
              data.notna <- subset(data.out, !is.na(data.out[,col.name]))
              data.value <- data.notna[ data.notna[,col.name] == value,]
              data.notvalue <- data.notna[data.notna[,col.name] != value,]
              data.value[, var.name] <- class
              
              data.notna <- bind_rows(data.value, data.notvalue)
              data.out <- bind_rows(data.na,data.notna)
            }
            else{
              data.out[data.out[,col.name] == value,var.name] <- class
                
            }
            
            
          }
        }
        
      }
      
    }
    
    
  }
  
  return(data.out)  
}

############################
variable.school_statistics <- function(
  data,
  variable                            
){
  data.out <- data
  

  
  return(data.out)  
}

############################
variable.segment <- function(
  data,
  variable                            
){
  data.out <- data
  
  columns <- variable$var.column
  if(length(columns)>0){
    for(i in 1:length(columns)){
      column <- columns[[i]]
      
      col.name <- column$col.name
      print(paste("Segment", col.name,Sys.time()))
      var.name <- column$var.name
      var.default <- column$var.default
      data.out[,var.name] <- var.default
      
      var.values <- column$var.value
      if(length(var.values)>0){
        for(j in 1:length(var.values)){
          var.value <- var.values[[j]]
          
          min <- var.value$min
          max <- var.value$max
          segment <- var.value$segment
          
          data.out[data.out[,col.name] > min & data.out[,col.name] <= max,var.name] <- segment
          
        }
      }
      
    }
      
    
  }
  
  #print(nrow(data.out))
  return(data.out)  
}

#############################
variable.subject <-  function(
  subject,
  algorithms,
  update = FALSE
){
  if(!is.null(subject$variable)){
    table.in <- subject$variable$table.in
    table.out <- subject$variable$table.out
    timestamp()
    print(paste("Read Table:",table.in))
    if(update){
      data <- db.ReadTable(table.out)
    }
    else{
      data <- db.ReadTable(table.in)
    }
    
    
    variables <- subject$variable$var
    n <- length(variables)
    if(n>0 && subject$process){
      for(i in 1:n){
        variable <- variables[[i]]
        algorithm <- variable$var.type
        if(algorithm == algorithms$constant){
          data <- variable.constant(data, variable)
        }
        else if(algorithm == algorithms$polymer){
          data <- variable.polymer(data, variable)
        }
        else if(algorithm == algorithms$derivative){
          data <- variable.derivative(data, variable)
        }
        else if(algorithm == algorithms$classify){
          data <- variable.classify(data, variable)
        }
        else if(algorithm == algorithms$school_statistics){
          data <- variable.school_statistics(data, variable)
        }
        else if(algorithm == algorithms$segment){
          data <- variable.segment(data, variable)
        }
      }
    }
    
    timestamp()
    print(paste("Write Table:",table.out))
    db.WriteTable(data =data,  table = table.out)    
  }

}