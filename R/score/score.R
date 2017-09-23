#############################
library(yaml)
library(dplyr)
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.var <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat$var
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"statistics/statistics.R"))


#########################################################
score.bind <- function(
  bind
){
  tables <- bind$table.in
  data.out <- data.frame()
  if(length(tables)>0){
    for(i in 1:length(tables)){
      table <- tables[[i]]
      #timestamp()
      print(paste("Read Table:",table))
      if(i == 1){
        data.out <-  db.ReadTable(table)
      }
      else{
        data.in <- db.ReadTable(table)
        data.out <- bind_rows(data.out,data.in)
      }
    }
  }
  table <- bind$table.out
  timestamp()
  print(paste("Write Table:",table))
  db.WriteTable(data = data.out,  table = table)    
  
}

######################################################
score.statistics.mean <- function(
  data.in,
  data.out,
  stat,
  update =  FALSE
){
  
  print("Score.Mean")
  data.stat  <- data.in
  print(nrow(data.stat))
  set <-  data.frame()
  if(!is.null(stat$set)){
    for(i in 1:length(stat$set)){
      set[1,stat$set[[i]]$col.name] <-  stat$set[[i]]$col.value
    }
  }
  print(set)
  
  equal  <- data.frame()
  if(!is.null(stat$equal)){
    for(i in 1:length(stat$equal)){
      equal[1,stat$equal[[i]]$col.name] <- stat$equal[[i]]$col.value
      data.stat <- data.stat[data.stat[,stat$equal[[i]]$col.name]==stat$equal[[i]]$col.value,]
      print(nrow(data.stat))
    }
  }
  print(equal)
  obs1 <- statistics.obs.merge(set,equal)
  print(obs1)
  print(nrow(data.stat))
  if(nrow(data.stat)>0){
    obs <- data.frame()
    if(!is.null(stat$groupby)){
      by.columns <- unlist(stat$groupby)
      groups <- aggregate(data.stat[,g.var$value],by=data.stat[,by.columns],FUN=mean)
      n  <- nrow(groups)
      if(n>0){
        for(m in 1:n){
          obs2 <- groups[m,]
          obs2[1, g.var$value] <- groups[m,"x"]    
          obs <-statistics.obs.merge(obs1,obs2)
          obs <- obs.dataframe.2list(obs)
          #print(obs)
          data.out  <- statistics.add(data.out,obs,update)
          #index <-  nrow(data.out)+1
          #data.out[index,]  <-  obs
        }
      }
    }
    else{
      obs2[1, g.var$value] <- mean(data.stat[,g.var$value])
      
      obs <-statistics.obs.merge(obs1,obs2)
      obs <- obs.dataframe.2list(obs)
      #print(obs)
      data.out  <- statistics.add(data.out,obs,update)
      
      #index <-  nrow(data.out)+1
      #data.out[index,]  <-  obs
      
    }
  }
  
  
  return(data.out)
}
##############################################
score.statistics <- function(
  statistics,
  algorithms
){
  table.in  <- statistics$table.in
  table.out <- statistics$table.out
  timestamp()
  print(paste("Read Table:",table.in))
  data.in <- db.ReadTable(table = table.in)
  # this function require table.out  = table.in
  data.out <- data.in
  
  stats <- statistics$stat
  if(length(stats)>0){
    for(i in 1:length(stats)){
      stat <- stats[[i]]
      
      if(stat$statistics==algorithms$mean){
        data.out  <- score.statistics.mean(data.in,data.out,stat)
      }
      
    }
    
  }
  
  timestamp()
  print(paste("Write Table:",table.out))
  db.WriteTable(data = data.out,  table = table.out)    
}