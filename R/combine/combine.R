
#########################################################
index.combine <- function(
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