#############################
library(yaml)

conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
#source(paste0(g.dir$R,"check/check.R"))

##############################

merge.subject <-  function(subject){
  merges <- subject$merge
  n_merge <-  length(merges)
  if(n_merge > 0){
    for(i in 1:n_merge){
      merge <- merges[[i]]
      table.left <- merge$table.left
      table.right <- merge$table.right
      table.merge <-  merge$table.merge
      timestamp()
      print(paste("Read Table:",table.left))
      data.left <- db.ReadTable(table.left$tab.name)
      drop.left <- table.left$drop
      if(length(drop.left)>0){
        for(j in 1:length(drop.left)){
          data.left[,drop.left[[j]]] <- NULL 
        }
      }
      rename.left <- table.left$rename
      if(length(rename.left)>0){
        for(j in 1:length(rename.left)){
          data.left[,rename.left[[j]]$new] <- data.left[,rename.left[[j]]$old]
          data.left[,rename.left[[j]]$old] <- NULL
        }
      }
      keep.left <- table.left$keep
      if(length(keep.left)>0){
        data.left <- data.left[,unlist(keep.left)]
      }
      
      data.right <- db.ReadTable(table.right$tab.name)
      drop.right <- table.right$drop
      if(length(drop.right)>0){
        for(j in 1:length(drop.right)){
          data.right[,drop.right[[j]]] <- NULL 
        }
      }
      rename.right <- table.right$rename
      if(length(rename.right)>0){
        for(j in 1:length(rename.right)){
          data.right[,rename.right[[j]]$new] <- data.right[,rename.right[[j]]$old]
          data.right[,rename.right[[j]]$old] <- NULL
        }
      }
      keep.right <- table.right$keep
      if(length(keep.right)>0){
        data.right <- data.right[,unlist(keep.right)]
      }
      
      data.merge <- merge(data.left, data.right, by = merge$merge.by, all.x = TRUE)
      
      drop <- merge$drop
      if(length(drop)>0){
        for(j in 1:length(drop)){
          data.merge[,drop[[j]]] <- NULL 
        }
      }
      
      if(!is.null(table.merge)){
        #db.RemoveTable(table.merge)
        timestamp()
        print(paste("Write Table:",table.merge))
        db.WriteTable(data =  data.merge,  table = table.merge)
      }
      
    }    
  }

}