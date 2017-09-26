#############################
library(yaml)
library(rmarkdown)
library(showtext)
library(ggplot2)
library(dplyr)
library(ggthemes)

conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.var <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat$var
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"report/report.R"))
############################
plot.data <- function(
  report,
  plot
){
  data.out <- data.frame()
  filters <- plot$data$filter
  for(i in  1:length(filters)){
    print("data filter")
    filter <- filters[[i]]
    data <- report$data
    for(j in 1:length(filter)){
      fil <- filter[[j]]
      if(!is.null(fil$x)){
        data[,fil$x] <- data[,fil$from]
      }
      else if(!is.null(fil$fill)){
        #print(fil)
        for(k in 1:length(fil$by)){
          if(k==1){
            data[,fil$fill] <- data[,fil$by[k]]
          }
          else{
            data[,fil$fill] <- paste(data[,fil$fill] , data[,fil$by[k]])
          }
          
        }
        
      }
      else{
        data <- data[data[,fil$variable] == fil$value,]
      }
      #data <- data[data[,fil$variable] == fil$value,]
    }
    
    data.out <- bind_rows(data.out,data)
  }
  #data.out[,g.var$value] <- data.out[,g.var$value]/100.0
  #data.out[,g.var$value] <- round(data.out[,g.var$value], digits = plot$data$digits)
  data.out[,g.var$label] <- round(data.out[,g.var$value], digits = 0)
  
  if(!is.null(plot$data$derivative)){
    print("dara derivative")
    data.in <- data.out
    print(data.in)
    data.out <- data.frame()
    groups <- levels(factor(data.in[,plot$data$derivative$group]))
    for(i  in 1:length(groups)){
      group <- groups[i]
      #print(group)
      #print(plot$data$derivative$group)
      #data <- subset(data.in,plot$data$derivative$group==group)
      data <- data.in[data.in[,plot$data$derivative$group]==group,]
      #print(data)
      n <- nrow(data.out)
      for(j in 1:length(plot$data$derivative$keep)){
        var <- plot$data$derivative$keep[[j]]
        data.out[n+1,var] <- data[1,var]
      }
      
      #print(data[1,])
      keys <- levels(factor(data[,plot$data$derivative$key]))
      for(j in 1:length(keys)){
        key <-  keys[j]
        data.out[nrow(data.out),key] <- 
          data[data[,plot$data$derivative$key]==key,g.var$value]
      }
      
    }
    
  }
  
  if(!is.null(plot$data$join)){
    print("data join")
    data.left <- data.out[data.out[,plot$data$join$left$variable]
                          ==plot$data$join$left$value, ]
    data.right <- data.out[data.out[,plot$data$join$right$variable]
                          ==plot$data$join$right$value, ]
    data.right[,plot$data$join$newvariable] <- data.right[,g.var$value]
    data.right <- data.right[,c(plot$data$join$linkon,plot$data$join$newvariable)]
    
    data.out <- merge(x=data.left,y=data.right,by=plot$data$join$linkon,all.x=TRUE)
  }
  
  
    
  aes <- plot$data$aes
  for(i in  1:length(aes)){
    arg <- aes[[i]]
    data.out[,arg$arg] <- data.out[,arg$var]
    #if(!is.null(arg$order)){
    #  data.out[,arg$arg] <- factor(data.out[,arg$arg],levels = unlist(arg$order))
    #}
  }
  
  if(!is.null(report$aliasFlag)){
    print("aliasFlag")
    data.left <- data.out
    data.right <- report$aliasdata[,c(g.var$scope,g.var$alias)]
    scopes <- data.right[,g.var$scope]
    for(i in 1:nrow(data.left)){
      if(is.element(data.left[i,"x"],unlist(scopes))){
        data.out[i,"x"] <- data.right[data.right[,g.var$scope]==data.left[i,"x"],g.var$alias]
      }
      
    }
      
    
  }
  
  if(!is.null(plot$data$order)){
    print("data order")
    for(i in 1:length(plot$data$order)){
      order <- plot$data$order[[i]]
      data.out[,order$var] <- factor(data.out[,order$var], levels = unlist(order$list))
    }
        
  }
  
  if(!is.null(plot$data$sort)){
    print("data sort")
    for(i in 1:length(plot$data$sort)){
      sort <- plot$data$sort[[i]]
      data.sort <- data.out[data.out[,sort$variable]==sort$option,]
      print(data.sort)
      data.sorted <- arrange(data.sort,desc(data.sort[,sort$value]))
      print(data.sorted)
      sort$list <-list()
      n <- nrow(data.sorted)
      for(i in 1:n){
        if(sort$asc_desc == "DESC"){
          sort$list[[i]] <- data.sorted[n-i+1,sort$var]
        }
        else{
          sort$list[[i]] <- data.sorted[i,sort$var]
        }
        
        #print(sort$list[[i]])
      }
      data.out[,sort$var] <- factor(data.out[,sort$var], levels = unlist(sort$list))
    }
    
  }
  
  #print(data.out)
  return(data.out)
}

#############################
plot.dummy <-  function(
  
){
  plot <-  plot.in
  plot$output <-  report$output[[1]]
  plot$title <- report$title
  plot$dir  <- report$plot.out
  
  plot$file <- paste0(plot$dir,plot$title,"_",fig_name,"_",report.datetime(),plot$output$ext)
  print(plot$file)
  plot$data$set <- plot.data(report,plot)
  
  if(nrow(plot$data$set)>0){
    pdf(plot$file, width = plot$ggplot$width, height = plot$ggplot$height)
    showtext.begin()
    
    figure <- ggplot(data=plot$data$set, aes(x=x, y=y , fill = fill))
    figure <- figure + geom_bar( stat="identity", width = plot$ggplot$bar$width) 
    figure <- figure + geom_text(aes(label = label), vjust = "inward") 
    #figure <- figure + ggtitle(plot$fig.name) 
    figure <- figure + xlab(plot$ggplot$label$xlab) + ylab(plot$ggplot$label$ylab) 
    figure <- figure + guides(fill=FALSE) 
    figure <- figure + theme(panel.background = element_blank()) 
    figure <- figure + theme(text = element_text(family = "wqy-microhei"))
    
    print(figure)
    showtext.end()
    dev.off()
  }
  else{
    print("Plot Data NULL!")
  }
  
  return(plot$file)
  
}
##############################
plot.bar_segment <- function(
  plot
){
  figure <- ggplot(data=plot$data$set, aes(x=x, y=y , fill = fill))
  figure <- figure + geom_bar( stat="identity", width = plot$ggplot$bar$width) 
  figure <- figure + geom_text(aes(label = label), position = position_stack(vjust = 0.5)) 
  #figure <- figure + ggtitle(plot$fig.name) 
  figure <- figure + xlab(plot$ggplot$label$xlab) + ylab(plot$ggplot$label$ylab) 
  figure <- figure + guides(fill=FALSE) 
  figure <- figure + theme(panel.background = element_blank()) 
  figure <- figure + theme(text = element_text(family = "wqy-microhei"))
  return(figure)
}
#############################
plot.bar_stacking  <- function(
  plot
){
  print(plot$data$set)
  figure <- ggplot(data=plot$data$set, aes(x=x, y=y , fill = fill))
  figure <- figure + theme_economist() + scale_fill_economist()
  figure <- figure + geom_bar( stat="identity", width = plot$ggplot$bar$width,position = "stack") 
  figure <- figure + geom_text(aes(label = label), colour="red", position = position_stack(vjust = 0.5)) 
  if(!is.null(plot$ggplot$legend)){
    figure <- figure + guides(fill=guide_legend(keywidth = 0.6, keyheight = 0.6))
    figure <- figure +  theme(legend.position = plot$ggplot$legend$position, 
                              legend.direction = plot$ggplot$legend$direction , 
                              legend.text = element_text(size = 8),legend.title=element_blank()) #+
  }
  else{
    figure <- figure + guides(fill=FALSE) 
  }
  if(!is.null(plot$ggplot$group)){
    figure <- figure + facet_wrap(facets = ~Group, nrow = 1,
                                  strip.position = plot$ggplot$group$position, 
                                  scales = plot$ggplot$group$scales)
  }
  if(!is.null(plot$ggplot$axisx)){
    figure <- figure + theme(axis.text.x = element_text(angle = plot$ggplot$axisx$text_angle,
                                                        size = plot$ggplot$axisx$text_size,  hjust = 1))
  }
  
  #figure <- figure + ggtitle(plot$fig.name) 
  figure <- figure + xlab(plot$ggplot$label$xlab) + ylab(plot$ggplot$label$ylab) 
  
  
  figure <- figure + theme(plot.background = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(legend.background = element_blank()) +
    theme(text = element_text(family = "wqy-microhei"))
  #figure <- figure + theme(panel.background = element_blank()) 
  #figure <- figure + theme(text = element_text(family = "wqy-microhei"))
  
  return(figure)
}
############################
#############################
plot.bar_dodging  <- function(
  plot
){
  #print(plot$data$set)
 
  figure <- ggplot(data=plot$data$set, aes(x=x, y=y , fill = fill))
  figure <- figure + theme_economist() + scale_fill_economist()
  figure <- figure + geom_bar( stat="identity", width = plot$ggplot$bar$width,position = plot$ggplot$position) 
  if(!is.null(plot$ggplot$text)){
    
    figure <- figure + geom_text(aes(label = label),colour="red", position = position_stack(vjust = plot$ggplot$text$vjust)) 
    #figure <- figure + geom_text(aes(label = label),colour="red", vjust = plot$ggplot$text$vjust) 
  }
  if(!is.null(plot$ggplot$coord)){
    if(plot$ggplot$coord=="flip"){
      figure <- figure + coord_flip()
    }
    
  }

  if(!is.null(plot$ggplot$legend)){
    figure <- figure + guides(fill=guide_legend(keywidth = 0.6, keyheight = 0.6))
    figure <- figure +  theme(legend.position = plot$ggplot$legend$position, 
                              legend.direction = plot$ggplot$legend$direction , 
    legend.text = element_text(size = 8),legend.title=element_blank()) #+
    
  }
  else{
    figure <- figure + guides(fill=FALSE) 
  }
  if(!is.null(plot$ggplot$group)){
    figure <- figure + facet_wrap(facets = ~Group, nrow = 1,
                           strip.position = plot$ggplot$group$position, 
                           scales = plot$ggplot$group$scales)
  }
  
  if(!is.null(plot$ggplot$axisx)){
    figure <- figure + theme(axis.text.x = element_text(angle = plot$ggplot$axisx$text_angle,
                                                        size = plot$ggplot$axisx$text_size,  hjust = 1))
  }
  if(!is.null(plot$ggplot$axisy)){
    figure <- figure + scale_y_discrete(limits=unlist(plot$ggplot$axisy$discrete_limits))
    if(!is.null(plot$ggplot$axisy$ylim)){
      figure <- figure + coord_cartesian(ylim=unlist(plot$ggplot$axisy$ylim))
    }
  }
  
  
  #figure <- figure + ggtitle(plot$fig.name) 
  figure <- figure + xlab(plot$ggplot$label$xlab) + ylab(plot$ggplot$label$ylab) 
  
  figure <- figure + theme(plot.background = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.major.x = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(legend.background = element_blank()) +
    theme(text = element_text(family = "wqy-microhei"))
  #figure <- figure + theme(panel.background = element_blank()) 
  #figure <- figure + theme(text = element_text(family = "wqy-microhei"))
  return(figure)
}
############################
plot.box  <- function(
  plot
){
  #print(plot$data$set)
  figure <- ggplot(data=plot$data$set, aes(x=x,y=mean,alpha=0.1))
  figure <- figure + geom_errorbar(aes(ymin=ymin,ymax=ymax),size = 0.5, width = 0.15)
  figure <- figure + geom_boxplot( stat="identity", 
                                   aes(width =0.4,ymin=ymin, lower=lower,middle=middle,upper=upper,ymax=ymax,fill = fill)
                                  ) 
  figure <- figure + geom_point(shape=20, size=3,colour="red")
  #figure <- figure + geom_text(aes(label = label), position = position_stack(vjust = 0.5)) 
  #figure <- figure + ggtitle(plot$fig.name) 
  figure <- figure + xlab(plot$ggplot$label$xlab) + ylab(plot$ggplot$label$ylab) 
  figure <- figure + guides(fill=FALSE, alpha=FALSE) 
  
  #figure <- figure + theme(panel.background = element_blank()) 
  figure <- figure + theme(text = element_text(family = "wqy-microhei"))
  return(figure)
}
#############################
plot.point <- function(
  plot
){
  #print(plot$data$set)
  figure <- ggplot(data=plot$data$set,aes(x,y))
  figure <- figure + theme_economist() #+ scale_fill_economist()
  figure <- figure + geom_point(stat="identity", colour="white", shape=21, size = 4, 
                                aes(alpha = 0.8, fill = factor(fill))) + 
    scale_fill_manual(values=c("cyan4", "red"))
  #figure <- figure + coord_cartesian(xlim=c(0, 100))
  figure <- figure + geom_text(aes(label=label,alpha = 0.8),hjust=-0.1, vjust=0.5)
  #figure <- figure + geom_text(aes(label = label), position = position_stack(vjust = 0.5)) 
  #figure <- figure + ggtitle(plot$fig.name) 
  figure <- figure + xlab(plot$ggplot$label$xlab) + ylab(plot$ggplot$label$ylab) 
  figure <- figure + guides(fill=FALSE,alpha=FALSE) 
  
  figure <- figure + theme(plot.background = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.major.y  = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.major.x  = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(legend.background = element_blank()) +
    theme(text = element_text(family = "wqy-microhei"))
  #figure <- figure + theme(panel.background = element_blank()) 
  #figure <- figure + theme(text = element_text(family = "wqy-microhei"))
  return(figure)
}

#############################
plot.radar  <- function(
  plot
){
  #print(plot$data$set)
  
  figure <- ggplot(data=plot$data$set, aes(x=x, y=y ,group =1))
  figure <- figure + theme_economist() + scale_fill_economist()
  #figure <- figure + geom_bar( stat="identity", width = plot$ggplot$bar$width) 
  figure <- figure + geom_line( stat="identity", width = plot$ggplot$bar$width) 
  if(!is.null(plot$ggplot$text)){
    
    figure <- figure + geom_text(aes(label = label),colour="red", position = position_stack(vjust = plot$ggplot$text$vjust)) 
    #figure <- figure + geom_text(aes(label = label),colour="red", vjust = plot$ggplot$text$vjust) 
  }
  
  
  if(!is.null(plot$ggplot$legend)){
    figure <- figure + guides(fill=guide_legend(keywidth = 0.6, keyheight = 0.6))
    figure <- figure +  theme(legend.position = plot$ggplot$legend$position, 
                              legend.direction = plot$ggplot$legend$direction , 
                              legend.text = element_text(size = 8),legend.title=element_blank()) #+
    
  }
  else{
    figure <- figure + guides(fill=FALSE) 
  }
  
  figure <- figure +  coord_polar()
  
  if(!is.null(plot$ggplot$axisx)){
    figure <- figure + theme(axis.text.x = element_text(angle = plot$ggplot$axisx$text_angle,
                                                        size = plot$ggplot$axisx$text_size,  hjust = 1))
  }
  if(!is.null(plot$ggplot$axisy)){
    figure <- figure + scale_y_discrete(limits=unlist(plot$ggplot$axisy$discrete_limits))
    if(!is.null(plot$ggplot$axisy$ylim)){
      figure <- figure + coord_cartesian(ylim=unlist(plot$ggplot$axisy$ylim))
    }
  }
  
  
  #figure <- figure + ggtitle(plot$fig.name) 
  figure <- figure + xlab(plot$ggplot$label$xlab) + ylab(plot$ggplot$label$ylab) 
  
  figure <- figure + theme(plot.background = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.major.x = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(legend.background = element_blank()) +
    theme(text = element_text(family = "wqy-microhei"))
  #figure <- figure + theme(panel.background = element_blank()) 
  #figure <- figure + theme(text = element_text(family = "wqy-microhei"))

  return(figure)
}
#############################
plot.figure  <- function(
  report,
  plot.in,
  fig_name = NULL
){
  plot <-  plot.in
  plot$output <-  report$output[[1]]
  plot$title <- report$title
  plot$dir  <- report$plot.out
  
  plot$file <- paste0(plot$dir,plot$title,"_",fig_name,"_",report.datetime(),plot$output$ext)
  print(plot$file)
  plot$data$set <- plot.data(report,plot)
  
  if(nrow(plot$data$set)>0){
    pdf(plot$file, width = plot$ggplot$width, height = plot$ggplot$height)
    showtext.begin()
    
    
    if(plot$ggplot$geom == report$geom$bar_dodging){
      figure  <-  plot.bar_dodging(plot)
    }
    else if(plot$ggplot$geom == report$geom$bar_stacking){
      figure  <-  plot.bar_stacking(plot)
    }
    else if(plot$ggplot$geom == report$geom$box){
      figure  <-  plot.box(plot)
    }
    else if(plot$ggplot$geom == report$geom$point){
      figure  <-  plot.point(plot)
    }
    else if(plot$ggplot$geom == report$geom$bar_segment){
      figure  <-  plot.bar_segment(plot)
    }
    else if(plot$ggplot$geom == report$geom$radar){
      figure  <-  plot.radar(plot)
    }
    
    print(figure)
    showtext.end()
    dev.off()
  }
  else{
    print("Plot Data NULL!")
  }
  
  return(plot$file)

  
}