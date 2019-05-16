## QueryData class for green index data processing and report generation


## flag for source loaded checking
querydata.loaded <- TRUE

## define GreenIndexXlsx
library(methods)

GreenIndexQueryData <- setRefClass(
  "GreenIndexQueryData",
  contains = "GreenIndexBase",
  
  fields = list(
    
    database = "GreenIndexDatabase",
    data.scope = "list",
    
    alias.df = "data.frame",
    dataset.df = "data.frame",

    statistics.table = "character"
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
      
      
      
    },
    
    PrepareDataframe = function(){
      jobs <- config$GetConfigJob()$dataquery
      
      data.scope <<- jobs$pilot$scope
      
      dataframe <- jobs$dataframe
      alias.table <- paste0(dataframe$alias$table, 
                            dataframe$alias$suffix)
      alias.df <<- database$ReadTable(alias.table)
      
      dataset.table <- paste0(dataframe$dataset$table, 
                              dataframe$dataset$suffix)
      dataset.df <<- database$ReadTable(dataset.table)
      
      statistics.table <<- paste0(dataframe$statistics$table, 
                                  dataframe$statistics$suffix)
      
      
    },
    
    SetDataScope = function(scope) {
      data.scope <<- scope
    }, 
    
    
    GetData = function(process) {
      
      LogDebug(paste("Query Data:", process[1, kColumnDataset]))
      if (process[1, kColumnCity] == kColumnCity) {
        process[1, kColumnCity] <- data.scope$city
      }
      if (process[1, kColumnDistrict] == kColumnDistrict) {
        process[1, kColumnDistrict] <- data.scope$district
      }
      if (process[1, kColumnSchool] == kColumnSchool) {
        process[1, kColumnSchool] <- data.scope$school
      }
      
      clause.number <- 0
      SQL <- paste0("SELECT * FROM ", statistics.table, " WHERE ")
      
      if (process[1, kColumnSubject] != kStringAll) {
        SQL <- paste0(SQL, kColumnSubject, " = ", 
                      "'", process[1, kColumnSubject], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnDomain] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnDomain, " = ", 
                      "'", process[1, kColumnDomain], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnDimention] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnDimention, " = ", 
                      "'", process[1, kColumnDimention], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnGroup] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnGroup, " = ", 
                      "'", process[1, kColumnGroup], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnAttribute] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnAttribute, " = ", 
                      "'", process[1, kColumnAttribute], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnTopic] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnTopic, " = ", 
                      "'", process[1, kColumnTopic], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnStatisticsTier] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnStatisticsTier, " = ", 
                      "'", process[1, kColumnStatisticsTier], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnCity] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnCity, " = ", 
                      "'", process[1, kColumnCity], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnDistrict] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnDistrict, " = ", 
                      "'", process[1, kColumnDistrict], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnSchool] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnSchool, " = ", 
                      "'", process[1, kColumnSchool], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnStatisticsSample] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnStatisticsSample, " = ", 
                      "'", process[1, kColumnStatisticsSample], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnStatisticsIndexType] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnStatisticsIndexType, " = ", 
                      "'", process[1, kColumnStatisticsIndexType], "'")
        clause.number <- clause.number + 1
      }
      
      if (process[1, kColumnKey] != kStringAll) {
        if (clause.number > 0) {
          SQL <- paste0(SQL, " AND ")
        }
        SQL <- paste0(SQL, kColumnKey, " = ", 
                      "'", process[1, kColumnKey], "'")
        clause.number <- clause.number + 1
      }
      
      SQL <- paste0(SQL, ";")
      
      LogDebug(SQL)
      
      database$Connect()
      df <- database$GetQuery(SQL)
      database$Disconnect()
      
      if (nrow(df) > 0) {
       
        df[, kColumnName] <- df[, process[1, kColumnName]]

        if (process[1, kColumnDrop] != kStringNone) {
          drop <- unlist(strsplit(process[1, kColumnDrop],kSeparator))
          for (j in 1:length(drop)) {
            df <- df[df[, kColumnName] != drop[j], ]
          }
        }
        
        if (process[1, kColumnKeep] != kStringAll) {
          tmp.df <- data.frame()
          keep <- unlist(strsplit(process[1, kColumnKeep], kSeparator))
          for (j in 1:length(keep)) {
            tmp.df <- rbind(tmp.df, df[df[, kColumnName] == keep[j], ])
          }
          df <- tmp.df
        }
       
        df <- merge(df, alias.df, by = kColumnName, all.x = TRUE)
        if (process[1, kColumnAliasType] == kAliasTypeTotal) {
          df[, kColumnAlias] <- df[, kAliasTypeTotal]
        } else if  (process[1, kColumnAliasType] == kAliasTypeAnonymous) {
          df[, kColumnAlias] <- df[, kAliasTypeAnonymous]
        } else {
          df[, kColumnAlias] <- df[, kColumnName]
        }
        
        df[, c(kAliasTypeTotal, kAliasTypeAnonymous)] <- NULL
        
        
        if (df[1, kColumnValueType] == kValueTypeInteger) {
          df[, kColumnLabel] <- as.character(floor(df[, kColumnValue]))
        } else if (df[1, kColumnValueType] == kValueTypePercent) {
          df[, kColumnLabel] <- as.character(round(df[, kColumnValue], 
                                                   kPercentDigits))
        }
      }
      
      
    
      return(df)
                   
    },
    
    QueryData = function(dataset.code) {
      
      process <- dataset.df[dataset.df[, kColumnDataset] == dataset.code, ]
      
      data.df <- data.frame()
      i <- 1
      while (i <= nrow(process)) {
        df <- GetData(data.frame(process[i, ]))
        data.df <- rbind(data.df, df)
        i <- i + 1
      }
      return(data.df)
      
    },
    
    SubScoSam = function(df, subject, scope, sample, digits) {
      value <- df[df[, kColumnSubject] == subject & 
                    df[, kColumnStatisticsScope] == scope & 
                    df[, kColumnStatisticsSample] == sample, 
                  kColumnValue]
      return(round(value, digits = digits))
    },
    
    SubSco = function(df, subject, scope, digits) {
      value <- df[df[, kColumnSubject] == subject & 
                    df[, kColumnStatisticsScope] == scope, 
                  kColumnValue]
      return(round(value, digits = digits))
    },
    

    SubAttSamKey = function(df, subject, attribute, sample, keys, digits) {
      value <- 0
      for (i in 1:length(keys)) {
        key <- keys[i]
        value <- value + df[df[, kColumnSubject] == subject & 
                              df[, kColumnAttribute] == attribute & 
                              df[, kColumnStatisticsSample] == sample &
                              df[, kColumnKey] == key, kColumnValue] 
      }
      return(round(value, digits = digits))
    },
    
    SubTopScoSamKey = function(df, subject, topic, 
                               scope, sample, keys, digits) {
      value <- 0
      for (i in 1:length(keys)) {
        key <- keys[i]
        value <- value + df[df[, kColumnSubject] == subject & 
                              df[, kColumnTopic] == topic & 
                              df[, kColumnStatisticsScope] == scope &
                              df[, kColumnStatisticsSample] == sample &
                              df[, kColumnKey] == key, kColumnValue] 
      }
      return(round(value, digits = digits))
    },
    
    SubTopSamKey = function(df, subject, topic, sample, keys, digits) {
      value <- 0
      for (i in 1:length(keys)) {
        key <- keys[i]
        value <- value + df[df[, kColumnSubject] == subject & 
                              df[, kColumnTopic] == topic & 
                              df[, kColumnStatisticsSample] == sample &
                              df[, kColumnKey] == key, kColumnValue] 
      }
      return(round(value, digits = digits))
    },
    
    AttSamKey = function(df, attribute, sample, keys, digits) {
      value <- 0
      for (i in 1:length(keys)) {
        key <- keys[i]
        value <- value + df[df[, kColumnAttribute] == attribute & 
                              df[, kColumnStatisticsSample] == sample &
                              df[, kColumnKey] == key, kColumnValue] 
      }
      return(round(value, digits = digits))
    },
    
    SubDomSamKey = function(df, subject, domain, sample, keys, digits) {
      value <- 0
      for (i in 1:length(keys)) {
        key <- keys[i]
        value <- value + df[df[, kColumnSubject] == subject & 
                              df[, kColumnDomain] == domain & 
                              df[, kColumnStatisticsSample] == sample &
                              df[, kColumnKey] == key, kColumnValue] 
      }
      return(round(value, digits = digits))
    },
    
    SamTopKey = function(df, sample, topic, keys, digits) {
      value <- 0
      for (i in 1:length(keys)) {
        key <- keys[i]
        value <- value + df[df[, kColumnStatisticsSample] == sample & 
                              df[, kColumnTopic] == topic & 
                              df[, kColumnKey] == key, kColumnValue] 
      }
      return(round(value, digits = digits))
    },
    
    CoefSamDimTop = function(df, sample, dimention, topic, digits) {
      value <- df[df[, kColumnStatisticsIndexType] == kCoefficient & 
                    df[, kColumnKey] == kTRUE & 
                    df[, kColumnStatisticsSample] == sample &
                    df[, kColumnDimention] == dimention &
                    df[, kColumnTopic] == topic, 
                  kColumnValue]
      return(round(value, digits = digits))
    },
    
    CoefSamDomTop = function(df, sample, domain, topic, digits) {
      value <- df[df[, kColumnStatisticsIndexType] == kCoefficient & 
                    df[, kColumnKey] == kTRUE & 
                    df[, kColumnStatisticsSample] == sample &
                    df[, kColumnDomain] == domain &
                    df[, kColumnTopic] == topic, 
                  kColumnValue]
      return(round(value, digits = digits))
    },
    
    CoefDomDimSGrpScoSam = function(df, domain, dimention, group, 
                                    scope, sample, digits) {
      value <- df[df[, kColumnStatisticsIndexType] == kCoefficient & 
                    df[, kColumnKey] == kTRUE & 
                    df[, kColumnDomain] == domain &
                    df[, kColumnDimention] == dimention &
                    df[, kColumnGroup] == group & 
                    df[, kColumnStatisticsScope] == scope &
                    df[, kColumnStatisticsSample] == sample, 
                  kColumnValue]
      return(round(value, digits = digits))
    }
    
  )
)
