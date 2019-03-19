## JoinData class for green index data processing and report generation


## flag for source loaded checking
joindata.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexJoinData <- setRefClass(
  "GreenIndexJoinData",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    JoinData = function(){
      
      # get job configuration
      jobs <- config$GetJoinDataJob()
      for (i in 1:length(jobs)){
        job <- jobs[[i]]
        TODO <- job$TODO
        if (TODO || config$IsReworkAll()) {
          
          # read left, right data table
          left.table <- paste0(job$left$table, job$left$suffix)
          left.by <- job$left$by
          left.df <- database$ReadTable(left.table)
          
          right.table <- paste0(job$right$table, job$right$suffix)
          right.by <- job$right$by
          right.select <- job$right$select
          right.df <- database$ReadTable(right.table)
          if (length(right.select) > 0) {
            right.df <- right.df[right.select]
          }
          
          # set the join type
          join.type <- job$join.type
          all <- FALSE
          all.x <- FALSE
          all.y <- FALSE
          if (join.type == "inner"){
            
          } else if (join.type == "left"){
            all <- FALSE
            all.x <- TRUE
            all.y <- FALSE
          } else if (join.type == "right"){
            all <- FALSE
            all.x <- FALSE
            all.y <- TRUE
          } else if (join.type == "full"){
            all <- TRUE
            all.x <- TRUE
            all.y <- TRUE
          }
          # merge data frame
          out.df <- merge(left.df, right.df, by.x = left.by, by.y = right.by,
                          all = all, all.x = all.x, all.y = all.y)
          
          
          # rename some columns of the merged dataframe
          out.df[job$drop] <- NULL
          j <- 1
          while (j <= length(job$rename)){
            name.list <- job$rename[[j]]
            names(out.df) <- sub(paste0("^", name.list$name, "$"), 
                                 name.list$rename, names(out.df))
            j <- j + 1
          }
          
          # keep some columns of the merged dataframe
          if (length(job$keep) > 0) {
            out.df <- out.df[job$keep]
          }
          
          # save merged data frame to database
          out.table <- paste0(job$out$table, job$out$suffix)
          database$WriteTable(out.df, out.table)
          
        }
        
      }
      
    }
  )
)
    