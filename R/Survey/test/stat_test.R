############################################################

############################################################

library(readr)
library(dplyr)

############################################################
StatRecord<-setRefClass(
  "StatRecord",
  fields = list(
    assessment = "character",
    grade = "character",
    subject = "character",
    tier = "character",
    scope = "character",
    perspective = "character",
    sample = "character",
    domain = "character",
    dimention = "character",
    topic = "character",
    variable = "character",
    weight = "character",
    key = "character",
    value = "numeric"    
  ),
  methods = list(
    initialize = function(){
      assessment <<- ""
      grade <<- ""
      subject <<- ""
      tier <<- ""
      scope <<- ""
      perspective <<- ""
      sample <<- ""
      domain <<- ""
      dimention <<- ""
      topic <<- ""
      variable <<- ""
      weight <<-  ""
      key <<- ""
      value <<- -1          
    }
    
  )
)
############################################################

stat.record.set<-function(
  assessment = NULL,
  grade = NULL,
  subject = NULL,
  tier = NULL,
  scope = NULL,
  perspective = NULL,
  sample = NULL,
  domain = NULL,
  dimention = NULL,
  topic = NULL,
  variable = NULL,
  weight = NULL,
  key = NULL,
  value = NULL,
  stat.record.in
){
  
  stat.record.out <- stat.record.in
  if(!is.null(assessment)){
    stat.record.out$assessment <- assessment
  }
  
  if(!is.null(grade)){
    stat.record.out$grade  <-  grade
  }
  
  if(!is.null(subject)){
    stat.record.out$subject <- subject
  }
  
  if(!is.null(tier)){
    stat.record.out$tier <- tier
  }
  
  if(!is.null(scope)){
    stat.record.out$scope <- scope
  }
  
  if(!is.null(perspective)){
    stat.record.out$perspective <- perspective
  }
  
  if(!is.null(sample)){
    stat.record.out$sample <- sample
  }
  
  if(!is.null(domain)){
    stat.record.out$domain <- domain
  }
  
  if(!is.null(dimention)){
    stat.record.out$dimention <- dimention
  }
  
  if(!is.null(topic)){
    stat.record.out$topic <- topic
  }
  
  if(!is.null(variable)){
    stat.record.out$variable <- variable
  }
  
  if(!is.null(weight)){
    stat.record.out$weight <- weight
  }
  
  if(!is.null(key)){
    stat.record.out$key <- key
  }
  
  if(!is.null(value)){
    stat.record.out$value <- value
  }
  
  return(stat.record.out)
}

##########################################################
stat.record.add<-function(
  field,
  stat.data.in,
  stat.record
){
  stat.data.out  <- stat.data.in
  n <- nrow(stat.data.out) + 1
  stat.data.out[n,field$assessment] <- stat.record$assessment
  stat.data.out[n,field$grade] <- stat.record$grade
  stat.data.out[n,field$subject] <- stat.record$subject
  stat.data.out[n,field$tier] <- stat.record$tier
  stat.data.out[n,field$scope] <- stat.record$scope
  stat.data.out[n,field$perspective] <- stat.record$perspective
  stat.data.out[n,field$sample] <- stat.record$sample
  stat.data.out[n,field$domain] <- stat.record$domain
  stat.data.out[n,field$dimention] <- stat.record$dimention
  stat.data.out[n,field$topic] <- stat.record$topic
  stat.data.out[n,field$variable] <- stat.record$variable
  stat.data.out[n,field$weight] <- stat.record$weight
  stat.data.out[n,field$key] <- stat.record$key
  stat.data.out[n,field$value] <- stat.record$value
  
  return(stat.data.out)
  
}

##########################################################
# calculate statistics 
stat.surveys <- function(stat, surveys, dryrun, mintest){
  #db.Connect()
  if(length(surveys)>0){
    for(i in 1:length(surveys)){
      subject <- surveys[[i]]$name
      sub.algorithm <- surveys[[i]]$algorithm
      
      table <- surveys[[i]]$table$weight
      timestamp()
      print(paste(subject,"ReadTable:",table))
      survey.data <- db.ReadTable(table)
      stat.data <- data.frame()
      ########################@#########
      stat.record <- StatRecord$new()
      stat.record <- stat.record.set(
        assessment = "2015年绿色指标",
        grade = "九年级",
        subject = subject,
        stat.record.in = stat.record
      )
      domains <- surveys[[i]]$domain
      dom.size <- length(domains)
      print(dom.size)
      if(dom.size>0){
        if(mintest){
          dom.size  <- 1
        }
        
        for(j in 1:dom.size){
          domain <- domains[[j]]$dom.name
          stat.record <- stat.record.set(domain = domain,
                                         stat.record.in = stat.record)
          print(domain)
          dom.algorithm <- domains[[j]]$algorithm
          if(is.null(dom.algorithm)){
            dom.algorithm <- sub.algorithm
          }
          #print(dom.algorithm)
          dimentions <- domains[[j]]$dimention
          dim.size <- length(dimentions)
          if(dim.size>0){
            if(mintest){
              dim.size < -1
            }
            
            for(k in 1:dim.size){
              dimention <- dimentions[[k]]$dim.name
              stat.record <- stat.record.set(dimention = dimention,
                                             stat.record.in = stat.record)
              dim.algorithm <- dimentions[[k]]$algorithm
              if(is.null(dim.algorithm)){
                dim.algorithm <- dom.algorithm
              }
              print(dimention)
              #print(dim.algorithm)
              topics <- dimentions[[k]]$topic
              top.size <- length(topics)
              
              if(top.size>0){
                if(mintest){
                  top.size <- 1
                }
                
                for(l in 1:top.size){
                  topic <- topics[[l]]$top.name
                  stat.record <- stat.record.set(topic = topic,
                                                 stat.record.in = stat.record)
                  #print(topic)
                  column <- topics[[l]]$col.name
                  top.algorithm <- topics[[l]]$algorithm
                  stat.record <- stat.record.set(variable = "百分比",
                                                 stat.record.in = stat.record)
                  if(is.null(top.algorithm)){
                    top.algorithm <- dim.algorithm
                  }
                  #print(top.algorithm)
                  
                  weights <- stat$weight
                  #print(weights)
                  perspectives <- surveys[[i]]$perspective
                  
                  tiers <- stat$tier
                  #print(tiers)
                  for(m in 1:length(tiers)){
                    tier  <- tiers[[m]]
                    stat.record <- stat.record.set(tier = tier,
                                                   stat.record.in = stat.record)
                    #print(tier)
                    scopes <-  levels(factor(survey.data[,tier]))
                    tier.perspects <- perspectives
                    if(tier == stat$column$school){
                      tier.perspects <- list(c(stat$variable$total))
                    }
                    #print(tier.perspects)
                    sco.size = length(scopes)
                    if(mintest){
                      sco.size = 1
                    }
                    
                    for(n in 1:sco.size){
                      scope <- scopes[[n]]
                      stat.record <- stat.record.set(scope = scope,
                                                     stat.record.in = stat.record)
                      #print(scope)
                      scope.data <- survey.data[survey.data[,tier] == scope,]
                      for(p in 1:length(tier.perspects)){
                        perspective <-  tier.perspects[[p]]
                        #print(perspective)
                        stat.record <- stat.record.set(perspective = perspective,
                                                       stat.record.in = stat.record)
                        scope.perspects <- levels(factor(scope.data[,perspective]))
                        per.size <- length(scope.perspects)
                        if(mintest){
                          per.size <- 1
                        }
                        
                        for(s in 1:per.size){
                          if(scope.perspects[[s]]==perspective){
                            sample <-  scope
                          }
                          else{
                            sample <- scope.perspects[[s]]
                          }
                          #print(sample)
                          stat.record <- stat.record.set(sample = sample,
                                                         stat.record.in = stat.record)
                          #print(perspective)
                          if(scope.perspects[[s]]==perspective){
                            sample.data <- scope.data[scope.data[,perspective]==perspective,]
                          }
                          else{
                            sample.data <- scope.data[scope.data[,perspective]==sample,]
                          }
                          
                          
                          
                          for(w in 1:length(weights)){
                            weight <- weights[[w]]
                            stat.record <- stat.record.set(weight = weight,
                                                           stat.record.in = stat.record)
                            #print(weight)
                            total <- sum(sample.data[,weight])
                            groups <- aggregate(sample.data[,weight], by = list(sample.data[,column]), FUN=sum)
                            #print(total)
                            #print(groups)
                            for(g in 1:nrow(groups)){
                              stat.record <- stat.record.set(key = groups[g,]$Group.1, 
                                                             value = groups[g,]$x/total*100,
                                                             stat.record.in = stat.record)
                              
                              stat.data <-  stat.record.add(
                                stat$field,
                                stat.data,
                                stat.record
                              )
                              #print(stat.record)                                
                            }
                            
                          }
                          
                        }
                        
                      }
                    }
                  }
                }
              }
              
            }
          }
          
        }
        
      }
      
      ##################################
      
      if(!dryrun){
        table <- surveys[[i]]$table$stat
        if(!is.null(table)){
          timestamp()
          print(paste(subject,"Writable:",table))
          db.RemoveTable(table)
          db.WriteTable(stat.data, table, append = TRUE)
        }
        
      }
    }  
    
  }
  timestamp()
  #db.Disconnect()
  
}

##########################################################
library(yaml)
# init global configurations
g.dir <- yaml.load_file("yaml/conf.yaml")$dir
stat <- yaml.load_file(paste0(g.dir$yaml,"survey.yaml"))$define$statistics
surveys <- stat$survey
field <- stat$field
g.weight <-stat$type$weight
g.perspective <-  stat$type$perspective


stat.surveys(stat = stat,
             surveys = surveys,
             FALSE, FALSE)
