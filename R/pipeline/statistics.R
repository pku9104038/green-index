## StatisticsData class for green index data processing and report generation


## flag for source loaded checking
statistics.loaded <- TRUE

## define GreenIndexXlsx
library(methods)
library(digest)

GreenIndexStatisticsData <- setRefClass(
  "GreenIndexStatisticsData",
  contains = "GreenIndexBase",
  
  fields = list(
    
    database = "GreenIndexDatabase",
    
    in.df = "data.frame",
    output.table = "character",
    out.df = "data.frame",
    filter.df = "data.frame",
    choice.df = "data.frame",
    choice.code = "character",
    choice.key = "character",
    point.df = "data.frame",
    point.code = "character",
    point.value = "character",
    sample.df = "data.frame",
    stat.df = "data.frame",
    stat.list = "list",
    
    district.df = "data.frame",
    school.df = "data.frame",
    attribute.df = "data.frame",
    
    process = "data.frame",
    algorithm = "character",
    tier.name = "character",
    scope.name = "character",
    perspective.name = "character",
    sample.name = "character",
    variable.name = "character",
    choice.name = "character",
    statistics.value = "numeric",
    
    RUN = "character",
    pilot = "list"
    
  ),
  
  methods = list(
    
    Init = function(module.name, config.obj, database.obj){
      callSuper(module.name, config.obj)
      database <<- database.obj
    },
    
    InitStatList = function() {
      
      stat.list[kColumnHashDigest] <<- kHashDigestDefault
      stat.list[kColumnAssessment] <<- config$GetAssessmentName()
      stat.list[kColumnGrade] <<- process[1, kColumnGrade]
      stat.list[kColumnSubject] <<- process[1, kColumnSubject]
      stat.list[kColumnDomain] <<- process[1, kColumnDomain]
      stat.list[kColumnDimention] <<- process[1, kColumnDimention]
      stat.list[kColumnGroup] <<- process[1, kColumnGroup]
      stat.list[kColumnAttribute] <<- process[1, kColumnAttribute]
      stat.list[kColumnTopic] <<- process[1, kColumnTopic]
      stat.list[kColumnStatisticsTier] <<- ""
      stat.list[kColumnStatisticsScope] <<- ""
      stat.list[kColumnStatisticsPerspective] <<- ""
      stat.list[kColumnStatisticsSample] <<- ""
      stat.list[kColumnStatisticsVariable] <<- ""
      stat.list[kColumnStatisticsAlgorithm] <<- process[1, kColumnStatisticsAlgorithm]
      stat.list[kColumnStatisticsIndexType] <<- process[1, kColumnStatisticsIndexType]
      stat.list[kColumnValueType] <<- process[1, kColumnValueType]
      stat.list[kColumnKey] <<- ""
      stat.list[kColumnValue] <<- NA
      stat.list[kColumnTimeStamp] <<- as.character(Sys.time())
      
    },
    
    HashStatList = function() {
      msg <- paste(stat.list[kColumnAssessment],
                   stat.list[kColumnGrade],
                   stat.list[kColumnSubject],
                   stat.list[kColumnDomain],
                   stat.list[kColumnDimention],
                   stat.list[kColumnGroup],
                   stat.list[kColumnAttribute],
                   stat.list[kColumnTopic],
                   stat.list[kColumnStatisticsTier],
                   stat.list[kColumnStatisticsScope],
                   stat.list[kColumnCity],
                   stat.list[kColumnDistrict],
                   stat.list[kColumnSchool],
                   stat.list[kColumnStatisticsPerspective],
                   stat.list[kColumnStatisticsSample],
                   stat.list[kColumnStatisticsVariable],
                   # stat.list[kColumnStatisticsAlgorithm],
                   stat.list[kColumnStatisticsIndexType],
                   # stat.list[kColumnValueType],
                   stat.list[kColumnKey])
      stat.list[kColumnHashDigest] <<- digest(msg, algo = "sha256")
      
    },
    
    AddStatDataframe = function() {
      
      stat.list[kColumnStatisticsTier] <<- tier.name
      stat.list[kColumnStatisticsScope] <<- scope.name
      stat.list[kColumnStatisticsPerspective] <<- perspective.name
      stat.list[kColumnStatisticsSample] <<- sample.name
      stat.list[kColumnStatisticsVariable] <<- variable.name
      stat.list[kColumnKey] <<- choice.name
      stat.list[kColumnValue] <<- statistics.value
      stat.list[kColumnTimeStamp] <<- as.character(Sys.time())
    
      if (stat.list[kColumnStatisticsTier] == kColumnCity) {
        stat.list[kColumnCity] <<- stat.list[kColumnStatisticsScope]
        stat.list[kColumnDistrict] <<- kColumnDistrict
        stat.list[kColumnSchool] <<- kColumnSchool
      } else if (stat.list[kColumnStatisticsTier] == kColumnDistrict) {
        stat.list[kColumnCity] <<- district.df[district.df[, kColumnDistrict] ==
                                                stat.list[kColumnStatisticsScope],
                                              kColumnCity]
        stat.list[kColumnDistrict] <<- stat.list[kColumnStatisticsScope]
        stat.list[kColumnSchool] <<- kColumnSchool
      } else if (stat.list[kColumnStatisticsTier] == kColumnSchool ) {
        stat.list[kColumnCity] <<- school.df[school.df[, kColumnSchool] ==
                                                stat.list[kColumnStatisticsScope],
                                              kColumnCity]
        stat.list[kColumnDistrict] <<- school.df[school.df[, kColumnSchool] ==
                                              stat.list[kColumnStatisticsScope],
                                            kColumnDistrict]
        stat.list[kColumnSchool] <<- stat.list[kColumnStatisticsScope]
        
      } else if (stat.list[kColumnStatisticsTier] == kColumnRegion) {
        stat.list[kColumnCity] <<- stat.list[kColumnStatisticsScope]
        stat.list[kColumnDistrict] <<- stat.list[kColumnStatisticsScope]
        stat.list[kColumnSchool] <<- stat.list[kColumnStatisticsScope]
      }
      
      # LogDebug(paste( stat.list[kColumnCity], stat.list[kColumnDistrict], 
      #                stat.list[kColumnSchool] ))
      
      if (is.na(stat.list[kColumnKey]) || is.na(stat.list[kColumnValue][[1]])) {
        msg <- paste("AddStat",
                     stat.list[kColumnSubject], 
                       stat.list[kColumnStatisticsAlgorithm], 
                       stat.list[kColumnStatisticsVariable], 
                       stat.list[kColumnDomain],
                       stat.list[kColumnDimention],
                       stat.list[kColumnGroup],
                       stat.list[kColumnAttribute],
                       stat.list[kColumnTopic],
                       stat.list[kColumnStatisticsTier], 
                       stat.list[kColumnStatisticsPerspective],
                       stat.list[kColumnStatisticsScope], 
                       stat.list[kColumnStatisticsSample], 
                       stat.list[kColumnKey],
                       stat.list[kColumnValue])
        LogError(gsub("%*", "", msg))
                 
      } else {
        HashStatList()
        
        msg <- paste(stat.list[kColumnSubject],
                     algorithm, variable.name, 
                     tier.name, perspective.name,
                     scope.name, sample.name, 
                     choice.name, statistics.value)
        
        LogDebug(gsub("%*", "", msg))
        
        stat.df <<- rbind(stat.df, data.frame(stat.list))
      }
      
    },
    
    InsertStatDataframe = function(){
      database$Connect()
      
      i <- 1
      while (i <= nrow(stat.df)) {
        
        msg <- paste("Insert",
                       stat.df[i, kColumnSubject],
                       stat.df[i, kColumnStatisticsTier], 
                       stat.df[i, kColumnStatisticsPerspective],
                       stat.df[i, kColumnStatisticsScope], 
                       stat.df[i, kColumnStatisticsSample], 
                       stat.df[i, kColumnStatisticsAlgorithm], 
                       stat.df[i, kColumnStatisticsVariable], 
                       stat.df[i, kColumnKey],
                       stat.df[i, kColumnValue])
        LogDebug(gsub("%*", "", msg))
        
        SQL <- paste0("SELECT ", kColumnHashDigest, " FROM ", output.table, 
                     " WHERE ", kColumnHashDigest, " = ",  
                     "'", stat.df[i, kColumnHashDigest], "';")
        df <- database$GetQuery(SQL)
        
        if (nrow(df) == 0) {
          SQL <- paste0("INSERT INTO ", output.table, " ( ", 
                        kColumnHashDigest, ",",
                        kColumnAssessment, ",",
                        kColumnGrade, ",",
                        kColumnSubject, ",",
                        kColumnDomain, ",",
                        kColumnDimention, ",",
                        kColumnGroup, ",",
                        kColumnAttribute, ",",
                        kColumnTopic, ",",
                        kColumnStatisticsTier, ",",
                        kColumnStatisticsScope, ",",
                        kColumnCity, ",",
                        kColumnDistrict, ",",
                        kColumnSchool, ",",
                        kColumnStatisticsPerspective, ",",
                        kColumnStatisticsSample, ",",
                        kColumnStatisticsVariable, ",",
                        kColumnStatisticsAlgorithm, ",",
                        kColumnStatisticsIndexType, ",",
                        kColumnValueType, ",",
                        kColumnKey, ",",
                        kColumnValue, ",",
                        kColumnTimeStamp, " ) ",
                        " VALUES ", " ( ", 
                        "'", stat.df[i, kColumnHashDigest], "',",
                        "'", stat.df[i, kColumnAssessment], "',",
                        "'", stat.df[i, kColumnGrade], "',",
                        "'", stat.df[i, kColumnSubject], "',",
                        "'", stat.df[i, kColumnDomain], "',",
                        "'", stat.df[i, kColumnDimention], "',",
                        "'", stat.df[i, kColumnGroup], "',",
                        "'", stat.df[i, kColumnAttribute], "',",
                        "'", stat.df[i, kColumnTopic], "',",
                        "'", stat.df[i, kColumnStatisticsTier], "',",
                        "'", stat.df[i, kColumnStatisticsScope], "',",
                        "'", stat.df[i, kColumnCity], "',",
                        "'", stat.df[i, kColumnDistrict], "',",
                        "'", stat.df[i, kColumnSchool], "',",
                        "'", stat.df[i, kColumnStatisticsPerspective], "',",
                        "'", stat.df[i, kColumnStatisticsSample], "',",
                        "'", stat.df[i, kColumnStatisticsVariable], "',",
                        "'", stat.df[i, kColumnStatisticsAlgorithm], "',",
                        "'", stat.df[i, kColumnStatisticsIndexType], "',",
                        "'", stat.df[i, kColumnValueType], "',",
                        "'", stat.df[i, kColumnKey], "',",
                        stat.df[i, kColumnValue], ",",
                        "'", stat.df[i, kColumnTimeStamp], "');")
          
          df <- database$GetQuery(SQL)
        } else {
          
          SQL <- paste0("UPDATE ", output.table, " SET ", 
                        kColumnAssessment, " = ", "'", stat.df[i, kColumnAssessment], "', ",
                        kColumnGrade, " = ", "'", stat.df[i, kColumnGrade], "', ",
                        kColumnSubject, " = ", "'", stat.df[i, kColumnSubject], "', ",
                        kColumnDomain, " = ", "'", stat.df[i, kColumnDomain], "', ",
                        kColumnDimention, " = ", "'", stat.df[i, kColumnDimention], "', ",
                        kColumnGroup, " = ", "'", stat.df[i, kColumnGroup], "', ",
                        kColumnAttribute, " = ", "'", stat.df[i, kColumnAttribute], "', ",
                        kColumnTopic, " = ", "'", stat.df[i, kColumnTopic], "', ",
                        kColumnStatisticsTier, " = ", "'", stat.df[i, kColumnStatisticsTier], "', ",
                        kColumnStatisticsScope, " = ", "'", stat.df[i, kColumnStatisticsScope], "', ",
                        kColumnCity, " = ", "'", stat.df[i, kColumnCity], "', ",
                        kColumnDistrict, " = ", "'", stat.df[i, kColumnDistrict], "', ",
                        kColumnSchool, " = ", "'", stat.df[i, kColumnSchool], "', ",
                        kColumnStatisticsPerspective, " = ", "'", stat.df[i, kColumnStatisticsPerspective], "', ",
                        kColumnStatisticsSample, " = ", "'", stat.df[i, kColumnStatisticsSample], "', ",
                        kColumnStatisticsVariable, " = ", "'", stat.df[i, kColumnStatisticsVariable], "', ",
                        kColumnStatisticsAlgorithm, " = ", "'", stat.df[i, kColumnStatisticsAlgorithm], "', ",
                        kColumnStatisticsIndexType, " = ", "'", stat.df[i, kColumnStatisticsIndexType], "', ",
                        kColumnValueType, " = ", "'", stat.df[i, kColumnValueType], "', ",
                        kColumnKey, " = ", "'", stat.df[i, kColumnKey], "', ",
                        kColumnValue, " = ", as.character(stat.df[i, kColumnValue]), ", ",
                        kColumnTimeStamp, " = ", "'", stat.df[i, kColumnTimeStamp], "'",
                        " WHERE ", kColumnHashDigest, " = ", 
                        "'", stat.df[i, kColumnHashDigest], "';")
          df <- database$GetQuery(SQL)
          
          
        }
        i <- i + 1
      }
      
      database$Disconnect()
    },
    
    SingleChoicePercent = function() {
      
      sample.size <- nrow(sample.df)
      
      choice.set <- unique(choice.df[choice.df[, choice.code] == 
                                       variable.name, choice.key])
      
      for (n in 1:length(choice.set)) {
        choice.name <<- choice.set[n]
        
        choice.size <- nrow(sample.df[sample.df[, variable.name] == choice.name, ])
        statistics.value <<- choice.size / sample.size * 100.0
        
        # msg <- paste(algorithm, variable.name, 
        #               kColumnStatisticsTier, tier.name, 
        #               kColumnStatisticsPerspective, perspective.name,
        #                kColumnStatisticsScope, scope.name, 
        #                kColumnStatisticsSample, sample.name, 
        #                choice.name, statistics.value)
        # LogDebug(gsub("%*", "", msg))
        
        AddStatDataframe()    
      }
    },
    
    ValueSpacePercent = function() {
      
      sample.df <<- sample.df[!is.na(sample.df[, variable.name]), ]
      sample.size <- nrow(sample.df)
      
      choice.set <- unique(sample.df[, variable.name])
      choice.set <- choice.set[!is.na(choice.set)]
      
      for (n in 1:length(choice.set)) {
        choice.name <<- choice.set[n]
        
        choice.size <- nrow(sample.df[sample.df[, variable.name] == choice.name, ])
        statistics.value <<- choice.size / sample.size * 100.0
        
        # msg <- paste(algorithm, variable.name, 
        #                kColumnStatisticsTier, tier.name, 
        #                kColumnStatisticsPerspective, perspective.name,
        #                kColumnStatisticsScope, scope.name, 
        #                kColumnStatisticsSample, sample.name, 
        #                choice.name, statistics.value, 
        #              as.character(choice.size), as.character(sample.size))
  
        # LogDebug(gsub("%*", "", msg))

        AddStatDataframe()    
      }
    },
    
    ValueOptionPercent = function() {
      
      stat.list[kColumnTopic] <<- variable.name
      
      sample.df <<- sample.df[!is.na(sample.df[, variable.name]), ]
      sample.size <- nrow(sample.df)
      
      choice.set <- unique(as.character(sample.df[, variable.name]))
      choice.set <- choice.set[!is.na(choice.set)]
      
      for (n in 1:length(choice.set)) {
        choice.name <<- choice.set[n]
        
        choice.size <- nrow(sample.df[as.character(sample.df[, variable.name]) == choice.name, ])
        statistics.value <<- choice.size / sample.size * 100.0
        
        # msg <- paste(algorithm, variable.name, 
        #                kColumnStatisticsTier, tier.name, 
        #                kColumnStatisticsPerspective, perspective.name,
        #                kColumnStatisticsScope, scope.name, 
        #                kColumnStatisticsSample, sample.name, 
        #                choice.name, statistics.value, 
        #              as.character(choice.size), as.character(sample.size))
        
        # LogDebug(gsub("%*", "", msg))
        
        AddStatDataframe()    
      }
    },
    
    MultipleChoicePercent = function() {
      
      sample.size <- nrow(sample.df)
      
      variable.list <- unlist(strsplit(variable.name, kColumnMultipleChoice))
      i <- strtoi(variable.list[length(variable.list)])
      
      choice.set <- unique(choice.df[choice.df[, choice.code] == 
                                       variable.list[1], choice.key])
      
      choice.name <<- choice.set[i]
        
      choice.size <- nrow(sample.df[sample.df[, variable.name] == 1, ])
      statistics.value <<- choice.size / sample.size * 100.0

      
      AddStatDataframe()    

    },
 
    Quantile = function() {
      
      quantile.df <- sample.df[!is.na(sample.df[, variable.name]), ]
      quantile.set <- quantile(quantile.df[, variable.name], 
                               probs = seq(0, 1, 0.05), na.rm = TRUE)
      
      stat.list[kColumnStatisticsIndexType] <<- kStatistics
      choice.name <<- kMin
      statistics.value <<- quantile.set[kMinPercent]
      AddStatDataframe()  
      
      choice.name <<- kLower
      statistics.value <<- quantile.set[kLowerPercent]
      AddStatDataframe()
      
      choice.name <<- kMedian
      statistics.value <<- quantile.set[kMedianPercent]
      AddStatDataframe() 
      
      choice.name <<- kUpper
      statistics.value <<- quantile.set[kUpperPercent]
      AddStatDataframe() 
      
      choice.name <<- kMax
      statistics.value <<- quantile.set[kMaxPercent]
      AddStatDataframe() 
      
      choice.name <<- kMean
      statistics.value <<- mean(sample.df[, variable.name], na.rm = TRUE)
      AddStatDataframe() 
      
      miu <- statistics.value
      choice.name <<- kSigma
      statistics.value <<- sd(sample.df[, variable.name], na.rm = TRUE)
      AddStatDataframe() 
      sigma <- statistics.value
      
      choice.name <<- kCoefVar
      statistics.value <<- sigma/miu
      AddStatDataframe() 
      coefvar <- statistics.value
      
      choice.name <<- kCoefBalanceIndivadual # <- "个体均衡系数"
      stat.list[kColumnStatisticsIndexType] <<- kCoefficient
      stat.list[kColumnValueType] <<- kValueTypePercent
      statistics.value <<- (1-2.5*(coefvar))*10
      AddStatDataframe() 
      
      choice.name <<- kIndexBalanceIndividual # <- "个体均衡指数"
      stat.list[kColumnStatisticsIndexType] <<- kIndex
      stat.list[kColumnValueType] <<- kValueTypeInteger
      statistics.value <<- min(9,max(1, floor((1-2.5*(coefvar))*10)))
      AddStatDataframe() 
      
      # msg <- paste(algorithm, variable.name, 
      #              kColumnStatisticsTier, tier.name, 
      #              kColumnStatisticsPerspective, perspective.name,
      #              kColumnStatisticsScope, scope.name, 
      #              kColumnStatisticsSample, sample.name, 
      #              choice.name, statistics.value)
      
      # LogDebug(gsub("%*", "", msg))
      
    },
    
    PointRate = function() {
      
      code.name <- unlist(strsplit(variable.name, kSuffixSeparator))[1]
      point.row <- point.df[point.df[, point.code] == code.name, ]
      point <- as.numeric(point.row[1, point.value])      
      stat.list[kColumnStatisticsVariable] <<- code.name
      stat.list[kColumnTopic] <<- code.name
      
      choice.name <<- kPerspectiveTotal
      statistics.value <<- mean(as.numeric(sample.df[, variable.name]), 
                                na.rm = TRUE) / point * 100.0
      AddStatDataframe()  
      
      ranks <- unlist(unique(sample.df[, kVariableTotalRank]))
      for (i in 1:length(ranks)) {
        choice.name <<- ranks[i]
        statistics.value <<- 
          mean(as.numeric(sample.df[sample.df[, kVariableTotalRank] == 
                                      choice.name, variable.name]),
               na.rm = TRUE) / point * 100.0
        AddStatDataframe() 
      }
      
      
      # msg <- paste(algorithm, variable.name, 
      #              kColumnStatisticsTier, tier.name,
      #              kColumnStatisticsPerspective, perspective.name,
      #              kColumnStatisticsScope, scope.name,
      #              kColumnStatisticsSample, sample.name,
      #              choice.name, statistics.value)
      # LogDebug(gsub("%*", "", msg))
        
          
      
      
    },

    PointRateMean = function() {
      
      stat.list[kColumnTopic] <<- variable.name
      
      choice.name <<- kPerspectiveTotal
      statistics.value <<- mean(as.numeric(sample.df[, variable.name]), 
                                na.rm = TRUE) 
      AddStatDataframe()  
      
      ranks <- unlist(unique(sample.df[, kVariableTotalRank]))
      for (i in 1:length(ranks)) {
        choice.name <<- ranks[i]
        statistics.value <<- 
          mean(as.numeric(sample.df[sample.df[, kVariableTotalRank] == 
                                      choice.name, variable.name]),
               na.rm = TRUE)
        AddStatDataframe() 
      }
      
    },
    
    ProcessJob = function(){
      
      if (process[1, kColumnTODO] == "FALSE" && RUN == kPilotRun ){
        return(NA)
        
      } else if (process[1, kColumnTODO] == "TRUE"){
        
        LogInfo(paste("Process",
                      process[1, kColumnSubject], 
                      process[1, kColumnDomain], 
                      process[1, kColumnDimention],
                      process[1, kColumnGroup],
                      process[1, kColumnAttribute],
                      process[1, kColumnTopic],
                      process[1, kColumnStatisticsTier],
                      process[1, kColumnStatisticsPerspective],
                      process[1, kColumnVariableName],
                      process[1, kColumnFilterName],
                      process[1, kColumnStatisticsAlgorithm]))
        
        InitStatList()
        
        filter.df <<- FilteringDataframe(in.df, 
                                         process[1, kColumnFilterName], 
                                         process[1, kColumnFilterType], 
                                         process[1, kColumnFilterValue])
        
        algorithm <<- process[1, kColumnStatisticsAlgorithm]
        
        tiers <- process[1, kColumnStatisticsTier]
        tiers <- unlist(strsplit(tiers, kSeparator))
          
        perspectives <- process[1, kColumnStatisticsPerspective]
        perspectives <- unlist(strsplit(perspectives, kSeparator))
        
        
        if (process[1, kColumnVariableName] == kColumnQuestionGroup) {
          attr.df <- attribute.df[attribute.df[, kColumnSubject] == 
                                    process[1, kColumnSubject],]
          
          attr <- process[1, kColumnQuestionGroup]
          var <- process[1, kColumnQuestionCode]
          attr.df <- attr.df[attr.df[, attr ] == var, ]
          variables <- attr.df[, kColumnQuestionCode]
          variables <- unlist(variables)
          
        } else if (process[1, kColumnVariableName] == kAlgorithmPointRate && 
                   process[1, kColumnVariableSuffix] == kColumnSuffixPointValue ){
          point.set <- point.df[point.df[, kColumnSubject] == 
                                  process[1, kColumnSubject], ]
          variables <- unlist(unique(point.set[, point.code]))
        } else if (process[1, kColumnVariableName] == 
                   kAlgorithmSingleChoicePercent) {
          point.set <- point.df[point.df[, kColumnSubject] == 
                                  process[1, kColumnSubject], ]
          point.set <- point.set[point.set[, kColumnQuestionType] == 
                                   kQuestionTypeSingleChoice, ]
          variables <- unlist(unique(point.set[, point.code]))
        } else {
          variables <- process[1, kColumnVariableName]
          variables <- unlist(strsplit(variables, kSeparator))
        }
        
        
        
        
        # loop of variable
        for (i in 1:length(variables)) {
          variable.name <<- paste0(variables[i], 
                                  process[1, kColumnVariableSuffix])
          
          # loop for tier
          for (j in 1:length(tiers)) {
            
            data.colnames <- colnames(filter.df)
            if (!is.element(variable.name, data.colnames)) {
              LogWarn(paste("variable.name", variable.name, "undifined in data.frame !" ))
              break   # if variable.name not in filter.df, break to next variable
            }
            
            tier.name <<- tiers[j]
            
            # limit tier for pilot run
            if (RUN == kPilotRun && tier.name != kTierCity) {
              break
            }
            # limit tier for agile run
            if (RUN == kAgileRun && tier.name == kTierSchool) {
              break
            }
            
            # loop for pespective
            for (k in 1:length(perspectives)) {
              perspective.name <<- perspectives[k]
              
              # limit perspective for tier region and school
              if ( (tier.name == kTierSchool || tier.name == kTierRegion) && 
                   perspective.name != kPerspectiveTotal ) {
                break
              }
              
              # limit perspective for pilot run
              if (RUN == kPilotRun && perspective.name != kPerspectiveTotal) {
                break
              }
              
              # limit perspective for agile run
              if (RUN == kAgileRun && tier.name == kTierDistrict && 
                  perspective.name != kPerspectiveTotal) {
                break
              } 

              perspective.df <- filter.df
              
              
              # limit scope for milestone run
              if (RUN == kMileStone && tier.name == kTierSchool) {
                perspective.df <- 
                  perspective.df[perspective.df[, kTierDistrict] == 
                                                   pilot$scope$district, ]
              }
              scopes <- unique(perspective.df[!is.na(perspective.df[, tier.name]), 
                                              tier.name])
             
              # loop for scope
              for (l in 1:length(scopes)) {
                scope.name <<- scopes[l] 
                scope.df <- perspective.df[perspective.df[, tier.name] == 
                                             scope.name, ]
                # loop for sample
                samples <- unique(scope.df[!is.na(scope.df[, perspective.name]), 
                                           perspective.name])
                for (m in 1:length(samples)) {
                  sample.name <<- samples[m]
                  if (is.na(sample.name))
                    break
                  sample.df <<- scope.df[scope.df[, perspective.name] == 
                                           sample.name, ]
                  
                  if (is.element(kVariableTotalRank, names(sample.df))) {
                    LogDebug(paste(variable.name, kVariableTotalRank, 
                                   tier.name, perspective.name))
                    sample.df <<- sample.df[, c(variable.name, 
                                                kVariableTotalRank,
                                                tier.name, perspective.name)]
                  } else {
                    sample.df <<- sample.df[, c(variable.name, 
                                                tier.name, perspective.name)]
                  }
                
                  
                  #LogDebug(paste(algorithm, variable.name, tier.name, 
                  #               perspective.name))                 
                  
                  if (algorithm == kAlgorithmSingleChoicePercent) {
                    SingleChoicePercent()
                  } else if (algorithm == kAlgorithmValueSpacePercent) {
                    ValueSpacePercent()
                  } else if (algorithm == kAlgorithmMultipleChoicePercent) {
                    MultipleChoicePercent()
                  } else if (algorithm == kAlgorithmQuantile) {
                    Quantile()
                  } else if (algorithm == kAlgorithmPointRate) {
                    PointRate()
                  } else if (algorithm == kAlgorithmPointRateMean) {
                    PointRateMean()
                  } else if (algorithm == kAlgorithmValueOptionPercent) {
                    ValueOptionPercent()
                  } 
                  
                }
              }
              
            }
          }
        }
        
        InsertStatDataframe()
      }
    },
    
    StatisticsData = function(jobs){
      
      # get job configuration
      reworkall <- config$IsReworkAll()
      dropdata <- config$IsDropData()
      reworkjobs <- jobs$TODO
      RUN <<- jobs$RUN
      pilot <<- jobs$pilot
      DDL <- jobs$DDL
      
      district.table <- paste0(jobs$info$district$table, 
                               jobs$info$district$suffix)
      district.df <<- database$ReadTable(district.table)
      school.table <- paste0(jobs$info$school$table, 
                               jobs$info$school$suffix)
      school.df <<- database$ReadTable(school.table)
      
      attribute.table <- paste0(jobs$info$attribute$table, jobs$info$attribute$suffix)
      attribute.df <<- database$ReadTable(attribute.table)  
      
      
      for (i in 1:length(jobs$table)){
        job <- jobs$table[[i]]
        TODO <- job$TODO
        
        if (TODO || reworkjobs || reworkall) {
          
          # read input, process data table
          input.table.name <- job$input$table
          input.table.suffix <- job$input$suffix
          input.table <- paste0(job$input$table, job$input$suffix)
          in.df <<- database$ReadTable(input.table)
          in.df <<- ColumnProcessDataFrame(in.df, job$input)
          
          process.table <- paste0(job$process$table, job$process$suffix)
          process.df <- database$ReadTable(process.table)
          process.df <- process.df[
            process.df[, kColumnTableName] == input.table.name, ]
          # process.df <- process.df[
          #   process.df[, kColumnTableSuffix] == input.table.suffix, ]
          
          choice.table <- paste0(job$choice$table, job$choice$suffix)
          choice.df <<- database$ReadTable(choice.table)
          choice.code <<- job$choice$column$code
          choice.key <<- job$choice$column$key
          choice.df <<- choice.df[, c(choice.code, choice.key)]
          
          if(!is.null(job$point)){
            point.table <- paste0(job$point$table, job$point$suffix)
            point.df <<- database$ReadTable(point.table)
            point.code <<- job$point$column$code
            point.value <<- job$point$column$value
            point.df <<- point.df[, c(kColumnSubject, point.code,
                                      kColumnQuestionType, point.value)]
          }
          
          
          output.table <<- paste0(job$output$table, job$output$suffix)
          if (dropdata && database$ExistsTable(output.table)) {
            database$RemoveTable(output.table)
          }
          SQL <- paste("CREATE TABLE IF NOT EXISTS", output.table, DDL)
          LogDebug(SQL)
          database$CreateTable(output.table, SQL)
          
          
          LogInfo(paste("Statistics", input.table, "through", process.table,
                        "into", output.table))
          
          k <- 1
          while (k <= nrow(process.df)) {
            process <<- process.df[k, ]
            stat.df <<- data.frame()
            stat.list <<- list()
            ProcessJob()
            k <- k + 1
          }
          
        }
        
      }
      
      
      
    },
    
    StatisticsSurvey = function() {
      LogInfo("Statistics Survey!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$surveystat
      
      StatisticsData(jobs)
    },
    
    StatisticsScore = function() {
      LogInfo("Statistics Score!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$scorestat
      
      StatisticsData(jobs)
    },
    
    StatisticsMerged = function() {
      LogInfo("Statistics Merged!")
      
      # get job configuration
      jobs <- config$GetConfigJob()$mergedstat
      
      StatisticsData(jobs)
    }
  )
)
