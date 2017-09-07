#############################
library(yaml)
library(dplyr)
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.var <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat$var
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"statistics/statistics.R"))
##############################

############################
index.percent <- function(
  data,
  data.stat,
  stat,
  update =FALSE){
  
  print(paste("Percent",stat$tier,stat$perspective,stat$topic,stat$stat))
  obs <- stat
  data.out  <- data.stat
  
  obs$topic <- stat$index
  data.in <-  data[data[,g.var$tier]==stat$tier &
                       data[,g.var$perspective]==stat$perspective &
                       data[,g.var$domain]==stat$domain &
                       data[,g.var$dimention]==stat$dimention &
                       data[,g.var$topic]==stat$topic &
                       data[,g.var$statistics]==stat$stat,]
  scopes <-  levels(factor(data.in[,g.var$scope]))
  for(i in 1:length(scopes)){
    obs$scope <- scopes[i]
    data.scope <- data.in[data.in[,g.var$scope]==obs$scope,]
    samples <- levels(factor(data.scope[,g.var$sample]))
    for(j in 1:length(samples)){
      obs$sample <- samples[j]
      
      data.sample <- data.scope[data.scope[,g.var$sample]==obs$sample,]
      #print(data.sample)
      obs$key <- "指标"
      obs$variable <- ""
      obs$weight <- ""
      value <- 0
      #print(data.sample)
      for(k  in  1:length(stat$key)){
        key  <- stat$key[[k]]
        #print(key)
        data.value <-  data.sample[data.sample[,g.var$key]==key,]
        obs$variable  <- max(data.value[,g.var$variable])
        obs$weight <-  max(data.value[,g.var$weight])
        #print(data.value)
        value <- value + data.value[1 ,g.var$value]
      }
      obs$value <- max(1, floor(value/10))
      data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
          
    }

  }

  return(data.out)
  
}

############################
index.topic <- function(
  data,
  data.stat = NULL,
  topic,
  algorithms,
  update = FALSE
){
  if(is.null(data.stat)){
    data.out <-  data.frame()
  }
  else{
    data.out <- data.stat
  }
  
  stat <- list()
  stat$assesment <- topic$assesment
  stat$grade <- topic$grade
  stat$subject <- topic$subject
  
  if(length(topic$tier)>0){
    for(i in 1:length(topic$tier)){
      stat$tier <- topic$tier[[i]]
      perspectives <- statistics.get.perspective(tier = stat$tier, 
                                                 perspectives = topic$perspective)
      if(length(perspectives)>0){
        for(j in 1:length(perspectives)){
          stat$perspective <- perspectives[[j]]
          
          stat$domain <- topic$domain
          stat$dimention <- topic$dimention
          
          if(length(topic$statistics)>0){
            for(k in 1:length(topic$statistics)){
              stat$statistics <- topic$statistics[[k]]
              
              stat$topic <- topic$name
              stat$index <- topic$index
              stat$stat <- topic$stat
                  
              if(stat$statistics == algorithms$percent){
                stat$key <- topic$key
                data.out  <-  index.percent(data = data,
                                            data.stat = data.out, 
                                            stat = stat,
                                            update = update)
              }
              
                
              
            }
          }
          
        }
      }
      
    }
  }
  
  return(data.out)
}

#############################
index.subject <-  function(
  subject,
  algorithms,
  update = FALSE
){
  if(!is.null(subject$index) && subject$process){
    table.in <- subject$index$table.in
    table.out <- subject$index$table.out
    timestamp()
    print(paste("Read Table:",table.in))
    data <- db.ReadTable(table.in)
    if(update){
      data.out <- db.ReadTable(table.out)
    }
    else{
      data.out <-  data.frame()
    }
    
    # must have
    assesment <- subject$index$assesment
    grade <- subject$index$grade
    perspective <- subject$index$perspective
    tier <- subject$index$tier
    
    stats <- subject$index$stat
    
    if(length(stats)>0){
      for(i in 1:length(stats)){
        stat <- stats[[i]]
        
        if(is.null(stat$perspective)){
          stat$perspective <- perspective
        }
        
        if(is.null(stat$tier)){
          stat$tier <- tier
        }
        # must  have
        #stat$statistics
        
        domains <- stat$domain
        if(length(domains)>0){
          for(j in 1:length(domains)){
            domain <- domains[[j]]
            # must have
            # domain$name
            
            if(is.null(domain$perspective)){
              domain$perspective <- stat$perspective
            }
            
            if(is.null(domain$tier)){
              domain$tier <- stat$tier
            }
            
            if(is.null(domain$statistics)){
              domain$statistics <- stat$statistics
            }
            
            
            
            dimentions <- domain$dimention
            if(length(dimentions)>0){
              for(k in 1:length(dimentions)){
                dimention <-  dimentions[[k]]
                # must have
                # dimention$name
                if(is.null(dimention$perspective)){
                  dimention$perspective <- domain$perspective
                }
                if(is.null(dimention$tier)){
                  dimention$tier  <-  domain$tier
                }
                
                if(is.null(dimention$statistics)){
                  dimention$statistics <- domain$statistics
                }
                
                
                
                topics <- dimention$topic
                if(length(topics)>0){
                  for(l in 1:length(topics)){
                    topic <- topics[[l]]
                    # must have
                    # topic$name
                    if(is.null(topic$perspective)){
                      topic$perspective <- dimention$perspective
                    }
                    if(is.null(topic$tier)){
                      topic$tier <- dimention$tier
                    }
                    
                    if(is.null(topic$statistics)){
                      topic$statistics <- dimention$statistics
                    }
                    
                    
                    topic$assesment <-  assesment
                    topic$grade <- grade
                    topic$subject  <- subject$subject
                    topic$domain <- domain$name
                    topic$dimention <-  dimention$name
                    
                    
                    ########
                    data.out <- index.topic(
                      data = data, 
                      data.stat = data.out,
                      topic = topic,
                      algorithms = algorithms,
                      update = update)
                    
                  }
                }
                
              }
            }
          }
        }
        
      }
    }
    
    timestamp()
    print(paste("Write Table:",table.out))
    db.WriteTable(data = data.out,  table = table.out)    
  }
  
}