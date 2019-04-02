## LoadData class for green index data processing and report generation


## flag for source loaded checking
loaddata.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexLoadData <- setRefClass(
  "GreenIndexLoadData",
  contains = "GreenIndexBase",
  
  fields = list(
    xlsx = "GreenIndexXlsx",
    database = "GreenIndexDatabase"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj, xlsx.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
      xlsx <<- xlsx.obj
    },
    
    LoadData = function(){
      LogInfo("Loading Data!")
      
      jobs <- config$GetLoadDataJob()
      reworkall <- config$IsReworkAll()
      reworkjobs <- jobs$TODO
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        if (TODO || reworkjobs || reworkall) {
          
          table <- paste0(job$table, job$suffix)
          if (is.element(table, kSubjectSet)) {
            table <- paste0(table, kTableRaw)
          }
          
          file <- job$xlsx
          sheet <- job$sheet
          if (sheet == "csv") {
            LogInfo(paste(file, "into", table))
            df <- xlsx$ReadCsvFile(file)
          } else {
            LogInfo(paste(file, "sheet", sheet, "into", table))
            df <- xlsx$ReadXlsxSheet(file, sheet)
          }
          
          columns <- colnames(df)
          for (i in 1:length(columns)) {
            # set NULL into "" for all character column
            if (typeof(columns[i]) == "character"){
              df[is.na(df[, columns[i]]), columns[i]] <- ""
            }
            
          }
          #df[, kColumnCount] <- TRUE
          
          # rename some columns of the merged dataframe
          #df[job$drop] <- NULL
          
          #j <- 1
          #while (j <= length(job$rename)){
          #  name.list <- job$rename[[j]]
          #  names(df) <- sub(paste0("^", name.list$name, "$"), 
          #                       name.list$rename, names(df))
          #  j <- j + 1
          #}
          
          # keep some columns of the merged dataframe
          #if (length(job$keep) > 0) {
          #  df <- df[, job$keep]
          #}
          
          # add column
          #j <- 1
          #while (j <= length(job$add)){
          #  name <- job$add[[j]]$name
          #  value <- job$add[[j]]$value
          #  df[, name] <- value
          #  j <- j + 1
          #}
          
          #ColumnProcess()
          #data.df <- df
          #job.column <- job
          df <- ColumnProcessDataFrame(df, job)
          
          database$WriteTable(df, table)
        }
        
      }
      
    }
    
  )
)

# colnames(df) <- tolower(colnames(df))
# df <- df[!duplicated(df[,c("学科")]), ]