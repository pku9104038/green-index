#  


#  merge suevey table with sutdent, school table
survey.merge <- function(surveys, dryrun){
  #db.Connect()
  
  if(length(surveys)>0){
    for(i in 1:length(surveys)){
      sur.name <- surveys[[i]]$name
      
      merges <- surveys[[i]]$merge
      if(length(merges)>0){
        for(i in 1:length(merges)){
          table.left <- merges[[i]]$table.left
          
          timestamp()
          print(paste(sur.name,"ReadTable:",table.left$tab.name))
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
          
          table.rights <- merges[[i]]$table.right
          if(length(table.rights)>0){
            for(j in 1:length(table.rights)){
              table.right <- table.rights[[j]]
              timestamp()
              print(paste(sur.name,"ReadTable:",table.right$tab.name))
              data.right <- db.ReadTable(table.right$tab.name)
              
              
              rename.right <- table.right$rename
              if(length(rename.right)>0){
                for(j in 1:length(rename.right)){
                  data.right[,rename.right[[j]]$new] <- data.right[,rename.right[[j]]$old]
                  data.right[,rename.right[[j]]$old] <- NULL
                }
              }
              
              drop.right <- table.right$drop
              if(length(drop.right)>0){
                for(j in 1:length(drop.right)){
                  data.right[,drop.right[[j]]] <- NULL 
                }
              }
              
              keep.right <- table.right$keep
              if(length(keep.right)>0){
                data.right <- data.right[,unlist(keep.right)]
              }
              
              timestamp()
              print(paste(sur.name, "merge:",table.left$tab.name,table.right$tab.name))
              data.left <- merge(data.left, data.right, by = merges[[i]]$merge.by, all.x = TRUE)
              
            }
          }
          
          drop <- merges[[i]]$drop
          if(length(drop)>0){
            for(j in 1:length(drop)){
              data.left[,drop[[j]]] <- NULL 
            }
          }
          
          if(!dryrun){
            table <- merges[[i]]$table.merge
            if(!is.null(table)){
              timestamp()
              print(paste(sur.name, "WriteTable:",table))
              db.RemoveTable(table)
              db.WriteTable(data.left, table, append = TRUE)
            }
          }
        }
      }
      
    }
  }
  timestamp()
  #db.Disconnect()
  
}


#  weight suevey records at school, county table
survey.weight <- function(surveys, dryrun){
  #db.Connect()
  
  if(length(surveys)>0){
    for(i in 1:length(surveys)){
      sur.name <- surveys[[i]]$name
      
      weights <- surveys[[i]]$weight
      if(length(weights)>0){
        for(i in 1:length(weights)){
          table.in <- weights[[i]]$table.in
          
          timestamp()
          print(paste(sur.name,"ReadTable:",table.in))
          data.in <- db.ReadTable(table.in)
          
          weight <- weights[[i]]$weight
          print(length(weight))
          if(length(weight)>0){
            for(j in 1:length(weight)){
              print(weight)
              if(weight[[j]] == stat$variable$weight_school){
                schools  <- levels(factor(data.in[,stat$column$school]))
                total <-  nrow(data.in)
                weight_school <- total/length(schools)
                for(k in 1:length(schools)){
                  n =  nrow(data.in[data.in[,stat$column$school] == schools[k],])
                  data.in[data.in[,stat$column$school] == schools[k], stat$variable$weight_school] <- weight_school/n
                  #print(schools[k])
                  #print(paste(total,weight_school,n,weight_school/n))
                  
                }
                
              }
               
            }
          }
          
          if(!dryrun){
            table <- weights[[i]]$table.out
            if(!is.null(table)){
              timestamp()
              print(paste(sur.name, "WriteTable:",table))
              db.RemoveTable(table)
              db.WriteTable(data.in, table, append = TRUE)
            }
          }
        }
      }
      
    }
  }
  timestamp()
  #db.Disconnect()
  
}


# calculate variables of survey
survey.variables <- function(stat, surveys, dryrun){
  #db.Connect()
  if(length(surveys)>0){
    for(i in 1:length(surveys)){
      sur.name <- surveys[[i]]$sur.name
      
      
      table <- surveys[[i]]$table$merge
      timestamp()
      print(paste(sur.name,"ReadTable:",table))
      survey.data <- db.ReadTable(table)
      
      
      ################################
      
      corrects <- surveys[[i]]$correct
      if(length(corrects)>0){
        for(j in 1:length(corrects)){
          columns <- corrects[[j]]$column
          values <- corrects[[j]]$value
          if(length(columns)>0 & length(values)>0 ){
            for(k in 1:length(columns)){
              timestamp()
              print(paste(sur.name,"correct:",columns[[k]]))
              for(l in 1:length(values)){
                survey.data[survey.data[,columns[[k]]]==values[[l]]$error,columns[[k]]] <- values[[l]]$correct
              }
            }
          }
        }
      }
      
      
      ########################@#########
      
      variables <- surveys[[i]]$variable
      if(length(variables)>0){
        for(j in 1:length(variables)){
          variable <- variables[[j]]
          var.type <- variable$var.type
          var.name <- variable$var.name
          default <- variable$default
          threshold <- variable$threshold
          timestamp()
          if(var.type == stat$type$variable$constant){
            print(paste(sur.name, "variable:", var.name, var.type, default))
            survey.data[,var.name] <- default
          }
          else if(var.type == stat$type$variable$polymer){
            col.tmp <- "tmp"
            survey.data[,col.tmp] <- 0
            
            columns <- variable$column
            
            threshold_p <- threshold + 0.5
            
            print(paste(sur.name, "variable:", var.name, var.type, threshold,threshold_p))
            if(length(columns) >0){
              for(k in 1:length(columns)){
                col.names <- columns[[k]]$col.name
                values <- columns[[k]]$value
                if(length(col.names) > 0 & length(values)>0){
                  for(l in 1:length(col.names)){
                    col.name <- col.names[[l]]
                    for(m in 1:length(values)){
                      value <- values[[m]]
                      
                      survey.data[survey.data[,col.name] == value, col.tmp] <- 
                        survey.data[survey.data[,col.name] == value, col.tmp] + 1
                      
                    }
                  }
                  
                }
                
              }
              
              survey.data[,var.name] <- stat$option$F
              
              survey.data[survey.data[,col.tmp] > threshold_p, var.name] <- stat$option$T
              
              #if(length(columns)<2){
              #  survey.data[survey.data[,col.tmp] >= threshold,var.name] <- stat$option$T
              #  survey.data[survey.data[,col.tmp] < threshold,var.name] <- stat$option$F
              #} 
              #else{
              #  survey.data[survey.data[,col.tmp] > threshold,var.name] <- stat$option$T
              #  survey.data[survey.data[,col.tmp] <= threshold,var.name] <- stat$option$F
              #}
              #if(var.name == "课程领导力评价较高"){
                p.name  <- paste0(var.name,"_p")
                survey.data[,p.name] <-survey.data[,col.tmp]
              #}
              survey.data[,col.tmp] <- NULL
              
            }
            
            
          }
          else if(var.type == stat$type$variable$derivative){
            
            columns <- variable$column
            if(length(columns)>0){
              for(k in 1:length(columns)){
                col.names <- columns[[k]]$col.name
                error <- columns[[k]]$value$error
                correct <- columns[[k]]$value$correct
                if(length(col.names)>0){
                  for(l in 1:length(col.names)){
                    col.name <- col.names[[l]]
                    print(paste(sur.name,"variable:",var.type, col.name))
                    groups  <- levels(factor(survey.data[,col.name]))
                    for(m in 1:length(groups)){
                      select.items<-unlist(strsplit(groups[m],"┋"))
                      if(length(select.items)>0){
                        for(n in 1:length(select.items)){
                          var.name <- select.items[n]
                          if(var.name == error){
                            var.name <- correct
                          }
                          if(!is.element(var.name,colnames(survey.data))){
                            survey.data[,var.name] <- stat$option$F
                          }
                          survey.data[survey.data[,col.name] == groups[m],var.name] <- stat$option$T
                          
                        }
                      }
                      
                      
                    }
                  }
                  
                }
                
              }
            }
          }
          
          else if(var.type == stat$type$variable$classify){
            var.name <- variable$var.name
            columns <- variable$column
            if(length(columns)>0){
              for(k in 1:length(columns)){
                col.name <- columns[[k]]$col.name
                default <- columns[[k]]$default
                values <- columns[[k]]$value
                print(paste(sur.name,"variable:",var.type, col.name, default))
                groups  <- levels(factor(survey.data[,col.name]))
                for(l in 1:length(groups)){
                  select.items<-unlist(strsplit(groups[l],"┋"))
                  value <- default
                  for(m in 1:length(values)){
                    if(is.element(values[[m]]$option,select.items)){
                      value <- values[[m]]$class
                    }
                  }
                  survey.data[survey.data[,col.name] == groups[l],var.name] <- value
                }
              }
            }
          }
          else if(var.type == stat$type$variable$school_statistics){
            print(var.type)
            var.name <- variable$var.name
            col.name <- variable$col.name
            var.name.percent <- variable$var.name.percent
            threshold <- variable$threshold
            value <- variable$value
            
            #survey.data[,var.name] <- stat$option$F
            schools <- levels(factor(survey.data[,stat$column$school]))
            for(k in 1:length(schools)){
              school <- schools[k]
              
              school.data <- survey.data[survey.data[,stat$column$school] == school,]
              value.data <- school.data[school.data[,col.name]==value,]
              #school.weight <- sum(school.data[, stat$variable$weight]) 
              #value.weight <- sum(value.data[,stat$variable$weight])
              school.weight <- sum(school.data[, stat$variable$count]) 
              value.weight <- sum(value.data[,stat$variable$count])
              
              
              #percent<- floor((1-value.weight/school.weight) * 100)
              percent  <- value.weight/school.weight * 100
              if(percent > threshold){
                bin <-  stat$option$T
              }
              else{
                bin <- stat$option$F
              }
              #bin <- (percent > threshold) 
              survey.data[survey.data[,stat$column$school] == school,var.name.percent] <- percent
              
              survey.data[survey.data[,stat$column$school] == school,var.name] <- bin
              
              #print(paste(school,col.name, percent, bin))
              
            }
            
          }
          
          else if(var.type == stat$type$variable$sum){
            col.tmp <- "tmp"
            survey.data[,var.name] <- 0
            columns <- variable$column
            print(paste(sur.name, "variable:", var.name, var.type))
            if(length(columns) >0){
              for(k in 1:length(columns)){
                col.name <- columns[[k]]
                survey.data[, var.name] <- survey.data[, var.name]+survey.data[, col.name]
              }
            }
          }

          else if(var.type == stat$type$variable$above_mean){
            columns <- variable$column
            score   <- survey.data[,columns$col.name]
            weight <- survey.data[,columns$col.weight]
            mean <- (t(score)%*%weight)/sum(survey.data[,columns$col.weight])
            print(mean)
            print(typeof(mean))
            
            survey.data[,var.name] <- stat$option$F
            #survey.data[,"tmp"] <- round(mean,2)
            #survey.data[,var.name] <- (survey.data[,columns$col.name] >= survey.data[,"tmp"])
            print(paste(sur.name, "variable:", var.name, var.type, mean))
            #a  <- survey.data[,columns$col.name] >= mean
            #print(a)
            survey.data[survey.data[,columns$col.name] >= mean[1,1],var.name] <- stat$option$T
            
          } 
          
          
        }
      }
      
      
      
      ################################
      
      drops <- surveys[[i]]$drop
      if(!is.null(drops)){
        timestamp()
        print("droping")
        if(length(drops)>0){
          for(j in 1:length(drops)){
            survey.data[,drops[[j]]] <- NULL
          }
        }
        
      }
      
      ##################################
      
      if(!dryrun){
        table <- surveys[[i]]$table$clean
        if(!is.null(table)){
          timestamp()
          print(paste(sur.name,"Writable:",table))
          db.RemoveTable(table)
          db.WriteTable(survey.data, table, append = TRUE)
        }
        
      }
    }  
    
  }
  timestamp()
  #db.Disconnect()
  
}
