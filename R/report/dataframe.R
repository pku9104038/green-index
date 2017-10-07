#############################
library(yaml)
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml
g.var <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat$var
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
##############################
dataframe.table <- function(
  report,
  param
){
  
  data.out <- data.frame()
  filters <- param$data$filter
  print(filters)
  for(i in  1:length(filters)){
    print("data filter")
    filter <- filters[[i]]
    data <- report$data
    for(j in 1:length(filter)){
      fil <- filter[[j]]
      data <- data[data[,fil$variable] == fil$value,]
    }
    data.out <- bind_rows(data.out,data)
  }
  data.out[,g.var$label] <- round(data.out[,g.var$value], digits = param$data$digits)
  return(data.out)
}
######################
dataframe.row <- function(
  table.data,
  selector.list
){
  data <- table.data
  for(i in 1:length(selector.list)){
    selector <- selector.list[[i]]
    data <- data[data[selector$var]==selector$value,]
  }
  return(data)
  
}


######################
dataframe.element <- function(
  table.data,
  selector.list
){
  
  data <- table.data
  for(i in 1:length(selector.list)){
    selector <- selector.list[[i]]
    data <- data[data[selector$var]==selector$value,]
  }
  if(nrow(data)==1){
    return(data[1,g.var$label])
  }
  else{
    return(NULL)
  }
  
}
