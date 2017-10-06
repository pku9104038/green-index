#############################
library(yaml)
library(dplyr)
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml
g.var <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat$var
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))

#########################################
statistics.get.perspective <- function(
  tier,
  perspectives
){
  for(i in  1:length(perspectives)){
    perspective <- perspectives[[i]]
    if(perspective$tier == tier){
      return(perspective$perspective)
    }
  }
}
#############################
statistics.add  <- function(
  data.stat,
  obs,
  update =FALSE
){
  data.out <- data.stat
  if(update){
    index <- data.out[, g.var$assesment] == obs$assesment &
                    data.out[, g.var$grade] == obs$grade &
                    data.out[, g.var$subject] == obs$subject &
                    data.out[, g.var$tier] == obs$tier &
                    data.out[, g.var$scope] == obs$scope &
                    data.out[, g.var$perspect] == obs$perspect &
                    data.out[, g.var$sample] == obs$sample &
                    data.out[, g.var$domain] == obs$domain &
                    data.out[, g.var$dimention] == obs$dimention &
                    data.out[, g.var$topic] == obs$topic &
                    data.out[, g.var$variable] == obs$variable &
                    data.out[, g.var$statistics] == obs$statistics &
                    data.out[, g.var$weight] == obs$weight &
                    data.out[, g.var$key] == obs$key
                     
    data.out <- data.out[!index,]
  }
  
  index <- nrow(data.out)
  index <- index +1
  
  data.out[index, g.var$assesment] <- obs$assesment
  data.out[index, g.var$grade] <- obs$grade
  data.out[index, g.var$subject] <- obs$subject
  data.out[index, g.var$tier] <- obs$tier
  data.out[index, g.var$scope] <- obs$scope
  data.out[index, g.var$perspect] <- obs$perspect
  data.out[index, g.var$sample] <- obs$sample
  data.out[index, g.var$domain] <- obs$domain
  data.out[index, g.var$dimention] <- obs$dimention
  data.out[index, g.var$topic] <- obs$topic
  data.out[index, g.var$variable] <- obs$variable
  data.out[index, g.var$statistics] <- obs$statistics
  data.out[index, g.var$weight] <- obs$weight
  data.out[index, g.var$key] <- obs$key
  data.out[index, g.var$value] <- obs$value
  
  #print(data.out[index,])
  return(data.out)
}


#############################
obs.dataframe.2list  <- function(
  obs.dataframe
){
  obs <- list()
  obs$assesment <- obs.dataframe[1, g.var$assesment]
  obs$grade <- obs.dataframe[1, g.var$grade]
  obs$subject <- obs.dataframe[1, g.var$subject]
  obs$tier <- obs.dataframe[1, g.var$tier]
  obs$scope <- obs.dataframe[1, g.var$scope]
  obs$perspective <- obs.dataframe[1, g.var$perspective]
  obs$sample <- obs.dataframe[1, g.var$sample]
  obs$domain <- obs.dataframe[1, g.var$domain]
  obs$dimention <- obs.dataframe[1, g.var$dimention]
  obs$topic <- obs.dataframe[1, g.var$topic]
  obs$variable <- obs.dataframe[1, g.var$variable]
  obs$statistics <- obs.dataframe[1, g.var$statistics]
  obs$weight <- obs.dataframe[1, g.var$weight]
  obs$key <- obs.dataframe[1, g.var$key]
  obs$value <- obs.dataframe[1, g.var$value]

  
  return(obs)
}

#############################
statistics.obs.merge  <- function(
  obs1,
  obs2
){
  obs <- data.frame()
  if(!is.null(obs1[1,g.var$assesment])){
    obs[1,g.var$assesment] <- obs1[1,g.var$assesment]
  }
  else if(!is.null(obs2[1,g.var$assesment])){
    obs[1,g.var$assesment] <- obs2[1,g.var$assesment]
  }
  if(!is.null(obs1[1,g.var$grade])){
    obs[1,g.var$grade] <- obs1[1,g.var$grade]
  }
  else if(!is.null(obs2[1,g.var$grade])){
    obs[1,g.var$grade] <- obs2[1,g.var$grade]
  }
  if(!is.null(obs1[1,g.var$subject])){
    obs[1,g.var$subject] <- obs1[1,g.var$subject]
  }
  else if(!is.null(obs2[1,g.var$subject])){
    obs[1,g.var$subject] <- obs2[1,g.var$subject]
  }
  if(!is.null(obs1[1,g.var$tier])){
    obs[1,g.var$tier] <- obs1[1,g.var$tier]
  }
  else if(!is.null(obs2[1,g.var$tier])){
    obs[1,g.var$tier] <- obs2[1,g.var$tier]
  }
  if(!is.null(obs1[1,g.var$scope])){
    obs[1,g.var$scope] <- obs1[1,g.var$scope]
  }
  else if(!is.null(obs2[1,g.var$scope])){
    obs[1,g.var$scope] <- obs2[1,g.var$scope]
  }
  if(!is.null(obs1[1,g.var$perspective])){
    obs[1,g.var$perspective] <- obs1[1,g.var$perspective]
  }
  else if(!is.null(obs2[1,g.var$perspective])){
    obs[1,g.var$perspective] <- obs2[1,g.var$perspective]
  }
  if(!is.null(obs1[1,g.var$sample])){
    obs[1,g.var$sample] <- obs1[1,g.var$sample]
  }
  else if(!is.null(obs2[1,g.var$sample])){
    obs[1,g.var$sample] <- obs2[1,g.var$sample]
  }
  if(!is.null(obs1[1,g.var$domain])){
    obs[1,g.var$domain] <- obs1[1,g.var$domain]
  }
  else if(!is.null(obs2[1,g.var$domain])){
    obs[1,g.var$domain] <- obs2[1,g.var$domain]
  }
  if(!is.null(obs1[1,g.var$dimention])){
    obs[1,g.var$dimention] <- obs1[1,g.var$dimention]
  }
  else if(!is.null(obs2[1,g.var$dimention])){
    obs[1,g.var$dimention] <- obs2[1,g.var$dimention]
  }
  if(!is.null(obs1[1,g.var$topic])){
    obs[1,g.var$topic] <- obs1[1,g.var$topic]
  }
  else if(!is.null(obs2[1,g.var$topic])){
    obs[1,g.var$topic] <- obs2[1,g.var$topic]
  }
  if(!is.null(obs1[1,g.var$variable])){
    obs[1,g.var$variable] <- obs1[1,g.var$variable]
  }
  else if(!is.null(obs2[1,g.var$variable])){
    obs[1,g.var$variable] <- obs2[1,g.var$variable]
  }
  if(!is.null(obs1[1,g.var$statistics])){
    obs[1,g.var$statistics] <- obs1[1,g.var$statistics]
  }
  else if(!is.null(obs2[1,g.var$statistics])){
    obs[1,g.var$statistics] <- obs2[1,g.var$statistics]
  }

  if(!is.null(obs1[1,g.var$key])){
    obs[1,g.var$key] <- obs1[1,g.var$key]
  }
  else if(!is.null(obs2[1,g.var$key])){
    obs[1,g.var$key] <- obs2[1,g.var$key]
  }
  
  if(!is.null(obs1[1,g.var$value])){
    obs[1,g.var$value] <- obs1[1,g.var$value]
  }
  else if(!is.null(obs2[1,g.var$value])){
    obs[1,g.var$value] <- obs2[1,g.var$value]
  }
  
  if(!is.null(obs1[1,g.var$weight])){
    obs[1,g.var$weight] <- obs1[1,g.var$weight]
  }
  else if(!is.null(obs2[1,g.var$weight])){
    obs[1,g.var$weight] <- obs2[1,g.var$weight]
  }
  
  return(obs)
}
############################
statistics.percent <- function(
  data,
  data.stat,
  stat,
  update = FALSE
  ){
  
  print(paste("Percent",stat$tier,stat$perspective,stat$topic,stat$variable,Sys.time()))
  obs <- stat
  data.out  <- data.stat
  weight.total <- sum(data[,stat$weight])
  
  for(v in 1:length(stat$variable)){
    variable <- stat$variable[v]
    obs$variable <- variable
    
    groups <- aggregate(data[,stat$weight], by = data[,c(stat$tier,stat$perspective,variable)], FUN=sum)
    scopes <-  levels(factor(groups[,stat$tier]))
    for(i in 1:length(scopes)){
      obs$scope <- scopes[i]
      
      samples <- levels(factor(groups[groups[,stat$tier]==obs$scope,stat$perspective]))
      for(j in 1:length(samples)){
        obs$sample <- samples[j]
        
        weight.total <- sum(groups[groups[,stat$tier]==obs$scope & 
                                     groups[,stat$perspective] == obs$sample,
                                   "x"])
        keys <- levels(factor(groups[groups[,stat$tier]==obs$scope & 
                                       groups[,stat$perspective] == obs$sample,
                                     variable]))
        if(is.null(stat$option)){
          
          for(k in  1:length(keys)){
            obs$key <- keys[k]
            if(obs$key != ""){
              obs$value <- 100.0*groups[groups[,stat$tier]==obs$scope & 
                                        groups[,stat$perspective] == obs$sample &
                                        groups[,variable] == obs$key,
                                        "x"]/weight.total
              
              data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
            }
          }
        }
        else{
          value  <- 0
          
          for(k in  1:length(keys)){
            key <- keys[k]
            if(is.element(key,unlist(stat$option))){
              value <- value + 100.0*groups[groups[,stat$tier]==obs$scope & 
                                          groups[,stat$perspective] == obs$sample &
                                          groups[,variable] == key,
                                        "x"]/weight.total
              
              
            }
          }
          obs$key <- TRUE
          obs$value <- value
          data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
        }
      }
      
    }
    
  }
  
  
  return(data.out)
  
}
############################
############################
score.statistics.gt_mean <- function(
  data,
  data.stat,
  stat,
  update = FALSE
){
  
  print(paste("Great than mean",stat$tier,stat$perspective,stat$topic,stat$variable,Sys.time()))
  obs <- stat
  data.out  <- data.stat
  weight.total <- sum(data[,stat$weight])
  mean <- (t(data[,stat$variable])%*%data[,stat$weight])/weight.total
  #print(summary(mean))
  mean <- mean[1,1]
  
  for(v in 1:length(stat$variable)){
    variable <- stat$variable[v]
    obs$variable <- variable
    
    #groups <- aggregate(x = data[,variable], 
    #                    by = data[,c(stat$tier,stat$perspective)], 
    #                    FUN = mean)
    
    #print(groups)
    scopes <-  levels(factor(data[,stat$tier]))
    for(i in 1:length(scopes)){
      obs$scope <- scopes[i]
      data.scope <- data[data[,stat$tier]==obs$scope,]
      #print(stat$tier)
      #print(obs$scope)
      #print(nrow(data.scope))
      data.scope <- data.scope[!is.na(data.scope[,stat$perspective]),]
      if(nrow(data.scope)>0){
        samples <- levels(factor(data.scope[,stat$perspective]))
        for(j in 1:length(samples)){
          obs$sample <- samples[j]
          data.sample <- data.scope[data.scope[,stat$perspective]==obs$sample,]
          total<- nrow(data.sample)
          #mean <- groups[groups[,stat$tier]==obs$scope & groups[,stat$perspective] == obs$sample,"x"]
          
          if(total>0){
            #print(stat$perspective)
            #print(obs$sample)
            
            
            #print(total)
            
            #print(mean)
            
            weight.total <- sum(data.sample[,stat$weight])
            data.gt_mean <- data.sample[data.sample[,obs$variable] >  mean,]
            weight.gt  <- sum(data.gt_mean[,stat$weight])
            #print(gt)
            obs$value <- 100.0*weight.gt/weight.total
            obs$key <- TRUE
            data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
            
          }
          
        }      
      }

      
    }
    
  }
  
  
  return(data.out)
  
}

############################
statistics.percent.weight.count <- function(
  data,
  data.stat,
  stat,
  update = FALSE
){
  
  print(paste("Percent.Weight.Count",stat$tier,stat$perspective,stat$topic,stat$variable$count,Sys.time()))
  obs <- stat
  data.out  <- data.stat
  
  col.count <- stat$variable$count
  
  #weight.total <- sum(data[,stat$weight])
  
  for(v in 1:length(stat$variable$column)){
    variable <- stat$variable$column[[v]]$variable
    key <- stat$variable$column[[v]]$key
    
    obs$variable <- variable
    obs$key <- key
    
    data.in <- subset(data, !is.na(data[,variable]))
    
    scopes <-  levels(factor(data.in[,stat$tier]))
    for(i in 1:length(scopes)){
      obs$scope <- scopes[i]
      data.scope <- data.in[data.in[,stat$tier]==obs$scope,]
      
      samples <- levels(factor(data.scope[,stat$perspective]))
      
      data.perspective <-  subset(data.scope,!is.na(data.scope[,stat$perspective]))
      for(j in 1:length(samples)){
        obs$sample <- samples[j]
        data.sample <- data.perspective[data.perspective[,stat$perspective]==obs$sample,]
        
        weight.total <- t(data.sample[,col.count]) %*% data.sample[,stat$weight]
        weight.percent <- t(data.sample[,col.count]*data.sample[,variable]) %*% data.sample[,stat$weight]
        
        obs$value <- weight.percent/ weight.total
        
        data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
        
      }
      
    }
    
  }
  
  
  return(data.out)
  
}

############################
statistics.mean <- function(
  data,
  data.stat,
  stat,
  update =FALSE
  ){
  
  print(paste("Mean",stat$tier,stat$perspective,stat$topic,stat$variable,Sys.time()))
  obs <- stat
  data.out  <- data.stat
  #print("data")
  #print(nrow(data))
  scopes <- levels(factor(data[,stat$tier]))
  for(j  in 1:length(scopes)){
    obs$scope <- scopes[j]
    data.scope <- data[data[,stat$tier]==obs$scope,]
    
    #print(paste("data.scope",obs$scope))
    #print(nrow(data.scope))
    
    samples <-  levels(factor(data.scope[,stat$perspective]))
    data.perspective <-  subset(data.scope,!is.na(data.scope[,stat$perspective]))
    #print(paste("data.perspective",stat$perspective))
    #print(nrow(data.perspective))
    for(k in 1:length(samples)){
      obs$sample <- samples[k]
      data.sample <- data.perspective[data.perspective[,stat$perspective]==obs$sample,]
      data.sample <- subset(data.sample,!is.na(data.sample[,stat$variable]))
      #print(paste("data.sample",obs$sample))
      #print(nrow(data.sample))
      obs$value <- weighted.mean(x = data.sample[,stat$variable],
                                 w = data.sample[,stat$weight])
      #print(obs$value)
      data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
      
    }
  }
  
  return(data.out)
  
}

############################
statistics.point_rate <- function(
  data,
  data.stat,
  stat,
  update =FALSE
){
  
  print(paste("Point Rate",stat$tier,stat$perspective,stat$topic,stat$variable,Sys.time()))
  obs <- stat
  data.out  <- data.stat
  #print("data")
  #print(nrow(data))
  scopes <- levels(factor(data[,stat$tier]))
  for(j  in 1:length(scopes)){
    obs$scope <- scopes[j]
    data.scope <- data[data[,stat$tier]==obs$scope,]
    
    #print(paste("data.scope",obs$scope))
    #print(nrow(data.scope))
    
    samples <-  levels(factor(data.scope[,stat$perspective]))
    data.perspective <-  subset(data.scope,!is.na(data.scope[,stat$perspective]))
    #print(paste("data.perspective",stat$perspective))
    #print(nrow(data.perspective))
    for(k in 1:length(samples)){
      obs$sample <- samples[k]
      data.sample <- data.perspective[data.perspective[,stat$perspective]==obs$sample,]
      data.sample <- subset(data.sample,!is.na(data.sample[,stat$variable]))
      #print(paste("data.sample",obs$sample))
      #print(nrow(data.sample))
      obs$value <- weighted.mean(x = data.sample[,stat$variable],
                                 w = data.sample[,stat$weight])/stat$point*100.0
      #print(obs$value)
      if(is.null(stat$key)){
        obs$key <- obs$sample
      }
      data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
      
    }
  }
  
  return(data.out)
  
}
############################
statistics.coefvar <- function(
  data,
  data.stat,
  stat,
  update =FALSE
){
  
  print(paste("Coefficient of Variation",stat$tier,stat$perspective,stat$topic,stat$variable,Sys.time()))
  obs <- stat
  data.out  <- data.stat
  #print("data")
  #print(nrow(data))
  scopes <- levels(factor(data[,stat$tier]))
  for(j  in 1:length(scopes)){
    obs$scope <- scopes[j]
    data.scope <- data[data[,stat$tier]==obs$scope,]
    
    #print(paste("data.scope",obs$scope))
    #print(nrow(data.scope))
    
    samples <-  levels(factor(data.scope[,stat$perspective]))
    data.perspective <-  subset(data.scope,!is.na(data.scope[,stat$perspective]))
    #print(paste("data.perspective",stat$perspective))
    #print(nrow(data.perspective))
    for(k in 1:length(samples)){
      obs$sample <- samples[k]
      data.sample <- data.perspective[data.perspective[,stat$perspective]==obs$sample,]
      data.sample <- subset(data.sample,!is.na(data.sample[,stat$variable]))
      #print(paste("data.sample",obs$sample))
      #print(nrow(data.sample))
      mean <- weighted.mean(x = data.sample[,stat$variable],
                                 w = data.sample[,stat$weight])
      
      data.delta <- data.sample
      data.delta[,"delta"] <- data.delta[,stat$variable] - mean
      data.delta[,"delta"] <- data.delta[,"delta"] * data.delta[,"delta"]
      data.delta[,"delta"] <- data.delta[,"delta"] * data.delta[,stat$weight]
      stdev <- sqrt(sum(data.delta[,"delta"])/sum(data.delta[,stat$weight]))
      coefvar  <-  stdev/mean
      obs$value <- coefvar
      #print(obs$value)
      data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
      
    }
  }
  
  return(data.out)
  
}

############################
statistics.quantile <- function(
  data,
  data.stat,
  stat,
  update = FALSE
  ){
  
  print(paste("Quantile",stat$tier,stat$perspective,stat$topic,stat$variable,Sys.time()))
  obs <- stat
  data.out  <- data.stat
  
  
  scopes <- levels(factor(data[,stat$tier]))
  for(j  in 1:length(scopes)){
    obs$scope <- scopes[j]
    data.scope <- data[data[,stat$tier]==obs$scope,]
      
    samples <-  levels(factor(data.scope[,stat$perspective]))
    for(k in 1:length(samples)){
      obs$sample <- samples[k]
      data.sample <- data.scope[data.scope[,stat$perspective]==obs$sample,]
      
      for(l in 1:length(stat$quantile)){
        quantile <- stat$quantile[[l]]
        
        quant <- quantile(data.sample[,stat$variable],probs = c(quantile$prob), na.rm = TRUE)
        
        obs$key <- quantile$key
        obs$value <- quant[quantile$quant]
        
        data.out <- statistics.add(data.stat = data.out, obs = obs, update = update)
      }
      
    }
  }
  
  
  return(data.out)
  
}

############################
statistics.topic <- function(
  data,
  data.stat,
  topic,
  algorithms,
  update =  FALSE
){
  
  data.out <- data.stat
  
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
              
              if(length(topic$weight)>0){
                for(l in 1:length(topic$weight)){
                  stat$weight <- topic$weight[[l]]
                  stat$topic <- topic$name
                  stat$variable <- topic$variable
                  #print(stat)
                  #print(paste(assesment,grade,subject,tier,perspective,
                  #            domain,dimention,name,statistics,weight,var.name))
                  
                  if(stat$statistics == algorithms$percent){
                    if(!is.null(topic$condition$name)){
                      data.in <- data[data[,topic$condition$name]==topic$condition$value,]
                    }
                    else{
                      data.in <- data
                    }
                    if(!is.null(topic$option)){
                      stat$option <- topic$option
                    }
                    data.out  <-  statistics.percent(data = data.in,
                                                     data.stat = data.out, 
                                                     stat = stat,
                                                     update = update)
                  }
                  else if(stat$statistics == algorithms$quantile){
                    stat$quantile <- topic$quantile
                    data.out  <-  statistics.quantile(data = data,
                                                     data.stat = data.out, 
                                                     stat = stat,
                                                     update = update)
                  
                  }
                  else if(stat$statistics==algorithms$gt_mean){
                    data.out  <- score.statistics.gt_mean(data = data,
                                                          data.stat = data.out,
                                                          stat = stat,
                                                          update =  update)
                  }
                  else if(stat$statistics == algorithms$mean){
                    stat$key <- topic$key
                    data.out  <-  statistics.mean(data = data,
                                                  data.stat = data.out, 
                                                  stat = stat,
                                                  update = update)
                  }
                  else if(stat$statistics == algorithms$point_rate){
                    if(!is.null(topic$condition$name)){
                      data.in <- data[data[,topic$condition$name]==topic$condition$value,]
                    }
                    else{
                      data.in <- data
                    }
                    stat$key <- topic$key
                    stat$point <- topic$point
                    data.out  <-  statistics.point_rate(data = data.in,
                                                  data.stat = data.out, 
                                                  stat = stat,
                                                  update = update)
                  }
                  else if(stat$statistics == algorithms$coefvar){
                    stat$key <- topic$key
                    data.out  <-  statistics.coefvar(data = data,
                                                  data.stat = data.out, 
                                                  stat = stat,
                                                  update = update)
                  }
                  else if(stat$statistics == algorithms$percent_weight_count){
                      data.out  <-  statistics.percent.weight.count(data = data,
                                                    data.stat = data.out, 
                                                    stat = stat,
                                                    update = update)
                  
                  }
                  else if(stat$statistics==algorithms$mean){
                    data.out  <- score.statistics.mean(data.in,data.out,stat,update)
                  }
                  
                  
                }
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
statistics.subject <-  function(
  subject,
  algorithms,
  update = FALSE,
  topic.name = NULL
){
  
  if(!is.null(subject$statistics) && subject$process){
    table.in <- subject$statistics$table.in
    table.out <- subject$statistics$table.out
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
    assesment <- subject$statistics$assesment
    grade <- subject$statistics$grade
    perspective <- subject$statistics$perspective
    tier <- subject$statistics$tier
    
    stats <- subject$statistics$stat
    
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
        #stat$weight
        
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
            
            if(is.null(domain$weight)){
              domain$weight <- stat$weight
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
                
                if(is.null(dimention$weight)){
                  dimention$weight <-  domain$weight
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
                    
                    if(is.null(topic$weight)){
                      topic$weight <- dimention$weight
                    }
                    
                    topic$assesment <-  assesment
                    topic$grade <- grade
                    topic$subject  <- subject$subject
                    topic$domain <- domain$name
                    topic$dimention <-  dimention$name
                    
                    
                    ########
                    if(is.null(topic.name)){
                      data.out <- statistics.topic(
                        data = data, 
                        data.stat = data.out,
                        topic = topic,
                        algorithms = algorithms,
                        update = update)
                    }
                    else{
                      if(topic.name == topic$name){
                        data.out <- statistics.topic(
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
        
      }
    }
    
    timestamp()
    print(paste("Write Table:",table.out))
    db.WriteTable(data = data.out,  table = table.out)    
  }
  
}


#########################################################
score.bind <- function(
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

######################################################
score.statistics.mean <- function(
  data.in,
  data.out,
  stat,
  update =  FALSE
){
  
  print("Score.Mean")
  data.stat  <- data.in
  
  set <-  data.frame()
  if(!is.null(stat$set)){
    for(i in 1:length(stat$set)){
      set[1,stat$set[[i]]$col.name] <-  stat$set[[i]]$col.value
    }
  }
  #print(set)
  
  equal  <- data.frame()
  if(!is.null(stat$equal)){
    for(i in 1:length(stat$equal)){
      equal[1,stat$equal[[i]]$col.name] <- stat$equal[[i]]$col.value
      data.stat <- data.stat[data.stat[,stat$equal[[i]]$col.name]==stat$equal[[i]]$col.value,]
    }
  }
  #print(equal)
  obs1 <- statistics.obs.merge(set,equal)
  #print(obs1)
  if(nrow(data.stat)>0){
    obs <- data.frame()
    if(!is.null(stat$groupby)){
      by.columns <- unlist(stat$groupby)
      groups <- aggregate(data.stat[,g.var$value],by=data.stat[,by.columns],FUN=mean)
      n  <- nrow(groups)
      if(n>0){
        for(m in 1:n){
          obs2 <- groups[m,]
          obs2[1, g.var$value] <- groups[m,"x"]    
          obs <-statistics.obs.merge(obs1,obs2)
          obs <- obs.dataframe.2list(obs)
          #print(obs)
          data.out  <- statistics.add(data.out,obs,update)
          #index <-  nrow(data.out)+1
          #data.out[index,]  <-  obs
        }
      }
    }
    else{
      obs2[1, g.var$value] <- mean(data.stat[,g.var$value])
      
      obs <-statistics.obs.merge(obs1,obs2)
      obs <- obs.dataframe.2list(obs)
      #print(obs)
      data.out  <- statistics.add(data.out,obs,update)
     
      #index <-  nrow(data.out)+1
      #data.out[index,]  <-  obs
      
    }
  }
  
  
  return(data.out)
}
##############################################
score.statistics <- function(
  statistics,
  algorithms
){
  table.in  <- statistics$table.in
  table.out <- statistics$table.out
  timestamp()
  print(paste("Read Table:",table.in))
  data.in <- db.ReadTable(table = table.in)
  # this function require table.out  = table.in
  data.out <- data.in
  
  stats <- statistics$stat
  if(length(stats)>0){
    for(i in 1:length(stats)){
      stat <- stats[[i]]
      
      if(stat$statistics==algorithms$mean){
        data.out  <- score.statistics.mean(data.in,data.out,stat)
      }
      
    }
    
  }
  
  timestamp()
  print(paste("Write Table:",table.out))
  db.WriteTable(data = data.out,  table = table.out)    
}