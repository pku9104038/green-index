## DataReady class for green index data processing and report generation


## flag for source loaded checking
dataready.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexDataReady <- setRefClass(
  "GreenIndexDataReady",
  contains = "GreenIndexBase",
  
  fields = list(
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    DataReady = function(){
      LogInfo("Data Ready!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$dataready
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs || reworkall) {
          
          # read left, right data table
          left.table <- paste0(job$left$table, job$left$suffix)
          left.by <- job$left$by
          left.set <- job$left$set
          left.df <- database$ReadTable(left.table)
          left.df <- ColumnProcessDataFrame(left.df, job$left)
          
          right.table <- paste0(job$right$table, job$right$suffix)
          right.by <- job$right$by
          right.select <- job$right$select
          right.set <- job$right$set
          right.df <- database$ReadTable(right.table)
          right.df <- ColumnProcessDataFrame(right.df, job$right)
          
          out.table <- paste0(job$out$table, job$out$suffix)
          join.type <- job$join.type
          
          LogInfo(paste(join.type, "join", left.table, right.table, "into",
                        out.table ))
          
          # set the join type
          
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
          
          
          out.df <- ColumnProcessDataFrame(out.df, job)
          
          LogDebug(paste(left.table, as.character(nrow(left.df)),
                         right.table, as.character(nrow(right.df)),
                         out.table, as.character(nrow(out.df)) ))
          
          # save merged data frame to database
          database$WriteTable(out.df, out.table)
          
        }
        
      }
      
    }
  )
)
