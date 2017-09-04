#############################
library(yaml)

conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
#source(paste0(g.dir$R,"check/check.R"))

##############################
weight.individual <- function(
  weight
){
  table.in <- weight$table.in
  table.out <- weight$table.out
  timestamp()
  print(paste("Read Table:",table.in))
  data <- db.ReadTable(table.in)
  column <- weight$col.weight
  
  data[,column] <- 1
  
  timestamp()
  print(paste("Write Table:",table.out))
  db.WriteTable(data =  data,  table = table.out)
}

#############################
weight.group <- function(
  weight
){
  table.in <- weight$table.in
  table.out <- weight$table.out
  timestamp()
  print(paste("Read Table:",table.in))
  data <- db.ReadTable(table.in)
  column <- weight$col.weight
  col.group <- weight$col.group
  
  groups  <- levels(factor(data[,col.group]))
  total <-  nrow(data)
  n_group <- length(groups)
  #weight_group <- total/n_group
  weight_group <- 1
  for(i in 1:n_group){
    n =  nrow(data[data[,col.group] == groups[i],])
    data[data[,col.group] == groups[i], column] <- weight_group/n
  }
  
  timestamp()
  print(paste("Write Table:",table.out))
  db.WriteTable(data =  data,  table = table.out)
}

#############################
weight.sample <- function(
  weight
                          ){
  
  table.in <- weight$table.in
  table.out <- weight$table.out
  timestamp()
  print(paste("Read Table:",table.in))
  data <- db.ReadTable(table.in)
  column <- weight$col.weight
  
  data[,column] <- 1
  
  timestamp()
  print(paste("Write Table:",table.out))
  db.WriteTable(data =  data,  table = table.out)
  
}

#############################

#############################
weight.subject <-  function(
  subject,
  algorithms
  ){
  weights <- subject$weight
  n_weight <- length(weights)
  if(n_weight>0){
    for(i in 1:n_weight){
      weight <- weights[[i]]
      algorithm <- weight$algorithm
      if(algorithm == algorithms$individual){
        weight.individual(weight)
      }
      else if(algorithm == algorithms$group){
        weight.group(weight)
      }
      else if(algorithm == algorithms$sample){
        weight.sample(weight)
      }
    }
  }
}