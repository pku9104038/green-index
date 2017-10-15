#############################
library(yaml)
library(rmarkdown)
library(showtext)
library(ggplot2)
library(dplyr)
library(ggthemes)
#library(ggradar)

conf <- yaml.load_file("yaml/conf.yaml")
yaml <- yaml.load_file(paste0(conf$dir$yaml,conf$yaml$survey))
g.dir <- conf$dir
g.yaml <- conf$yaml
g.var <- yaml$global$stat$var
g.tier <- yaml$global$stat$def$tier
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"report/report.R"))

############################
#' ggradar
#' @author Ricardo Bion
#' @export ggradar
#'
#' @export
# most of the code is from http://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html


ggradar <- function(plot.data,
                    font.radar="Circular Air Light",
                    values.radar = c("0%", "50%", "100%"),                       
                    axis.labels=colnames(plot.data)[-1],                             
                    grid.min=0,  #10,
                    grid.mid=0.5,  #50,
                    grid.max=1,  #100,
                    centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                    plot.extent.x.sf=1,
                    plot.extent.y.sf=1.2,
                    x.centre.range=0.02*(grid.max-centre.y),
                    label.centre.y=FALSE,
                    grid.line.width=0.5,
                    gridline.min.linetype="longdash",
                    gridline.mid.linetype="longdash",
                    gridline.max.linetype="longdash",
                    gridline.min.colour="grey",
                    gridline.mid.colour="#007A87",
                    gridline.max.colour="grey",
                    grid.label.size=7,
                    gridline.label.offset=-0.1*(grid.max-centre.y),
                    label.gridline.min=TRUE,
                    axis.label.offset=1.15,
                    axis.label.size=8,
                    axis.line.colour="grey",
                    group.line.width=1.5,
                    group.point.size=6,
                    background.circle.colour="#D7D6D1",
                    background.circle.transparency=0.2,
                    plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                    legend.title="",
                    plot.title="",
                    legend.text.size=grid.label.size ) {
  
  library(ggplot2)
  
  plot.data <- as.data.frame(plot.data)
  
  plot.data[,1] <- as.factor(as.character(plot.data[,1]))
  names(plot.data)[1] <- "group"
  
  var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n
  
  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
  
  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")
  #Declare required internal functions
  
  CalculateGroupPath <- function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
    
    path <- df[,1]
    
    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]
    
    for(i in levels(path)){
      pathData = subset(df, df[,1]==i)
      for(j in c(2:ncol(df))){
        #pathData[,j]= pathData[,j]
        
        
        graphData=rbind(graphData, data.frame(group=i, 
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))
    }
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]
    graphData #data frame returned by function
  }
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  
  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)
  # (e) Create Circular grid-lines + labels
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(grid.mid))
  #print(gridline$min$label)
  #print(gridline$max$label)
  #print(gridline$mid$label)
  ### Start building up the radar plot
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size=20) + 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))
  
  if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
  
  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
  # then centred labels for axis labels almost immediately above/below x= 0 
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly 
  # identify plot extent when plotting first (base) layer]
  
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1, family=font.radar) +
    scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) + 
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5, family=font.radar)
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0, family=font.radar)
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)
  
  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)
  
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                           size=group.line.width)
  
  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)
  
  
  #... + amend Legend title
  if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                            lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                            lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                            lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  if (label.gridline.min==TRUE) {
    
    base <- base + geom_text(aes(x=x,y=y,label=values.radar[1]),data=gridline$min$label,size=grid.label.size*0.8, hjust=1, family=font.radar) }
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[2]),data=gridline$mid$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[3]),data=gridline$max$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,size=grid.label.size, hjust=0.5, family=font.radar) }
  
  base <- base + theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20,
                                                                                    family = font.radar)) +
    theme(legend.text = element_text(size = legend.text.size), legend.position="left") +
    theme(legend.key.height=unit(2,"line")) +
    scale_colour_manual(values=rep(c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051", 
                                     "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)) +
    theme(text=element_text(family=font.radar)) + 
    theme(legend.title=element_blank())
  
  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }
  
  return(base)
  
}



############################
#' ggradar
#' @author Ricardo Bion
#' @export ggradar
#'
#' @export
# most of the code is from http://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html

CalculateGroupPath <- function(df) {
  #Converts variable values into a set of radial x-y coordinates
  #Code adapted from a solution posted by Tony M to
  #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
  #Args:
  #  df: Col 1 -  group ('unique' cluster / group ID of entity)
  #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
  
  print("CalculateGroupPath")
  
  path <- df[,1]
  
  ##find increment
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
  ##create graph data frame
  graphData= data.frame(seg="", x=0,y=0)
  graphData=graphData[-1,]
  
  for(i in levels(path)){
    pathData = subset(df, df[,1]==i)
    for(j in c(2:ncol(df))){
      #pathData[,j]= pathData[,j]
      
      
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,j]*sin(angles[j-1]),
                                            y=pathData[,j]*cos(angles[j-1])))
    }
    ##complete the path by repeating first pair of coords in the path
    graphData=rbind(graphData, data.frame(group=i, 
                                          x=pathData[,2]*sin(angles[1]),
                                          y=pathData[,2]*cos(angles[1])))
  }
  #Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  graphData #data frame returned by function
}

CalculateGridPath <- function(data, grid.levels) {
  #Converts variable values into a set of radial x-y coordinates
  #Code adapted from a solution posted by Tony M to
  #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
  #Args:
  #  df: Col 1 -  group ('unique' cluster / group ID of entity)
  #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
  
  df <- data.frame()
  for(i in 1:length(grid.levels)){
    for(j in 1:ncol(data)){
      if(j==1){
        df[i,j] <- as.character(i) 
      }
      else{
        df[i,j] <- grid.levels[i]
      }
      
    }
  }
  colnames(df) <- colnames(data)
  names(df)[1] <- "grid"
  print("CalculateGridPath")
  
  path <- df[,1]
  
  ##find increment
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
  ##create graph data frame
  graphData= data.frame(seg="", x=0,y=0)
  graphData=graphData[-1,]
  
  for(i in levels(factor(path))){
    pathData = subset(df, df[,1]==i)
    for(j in c(2:ncol(df))){
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,j]*sin(angles[j-1]),
                                            y=pathData[,j]*cos(angles[j-1])))
    }
    ##complete the path by repeating first pair of coords in the path
    graphData=rbind(graphData, data.frame(group=i, 
                                          x=pathData[,2]*sin(angles[1]),
                                          y=pathData[,2]*cos(angles[1])))
  }
  #Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  graphData #data frame returned by function
}

CaclulateAxisPath <- function(var.names,min,max) {
  #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
  #Args:
  #var.names - list of variables to be plotted on radar plot
  #min - MININUM value required for the plotted axes (same value will be applied to all axes)
  #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
  #var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  #Cacluate required number of angles (in radians)
  angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  #calculate vectors of min and max x+y coords
  min.x <- min*sin(angles)
  min.y <- min*cos(angles)
  max.x <- max*sin(angles)
  max.y <- max*cos(angles)
  #Combine into a set of uniquely numbered paths (one per variable)
  axisData <- NULL
  for (i in 1:n.vars) {
    a <- c(i,min.x[i],min.y[i])
    b <- c(i,max.x[i],max.y[i])
    axisData <- rbind(axisData,a,b)
  }
  #Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no","x","y")
  rownames(axisData) <- seq(1:nrow(axisData))
  #Return calculated axis paths
  as.data.frame(axisData)
}

funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

############

myggradar <- function(plot.data,
                    font.radar="Circular Air Light",
                    values.radar = c("0%", "50%", "100%"),                       
                    axis.labels=colnames(plot.data)[-1],   
                    grid.levels = c(0,1,2,3,4,5,6,7,8,9),
                    grid.min= grid.levels[1], #0,  #10,
                    grid.mid= grid.levels[roundup(length(grid.levels)/2,0)],  #0.5,  #50,
                    grid.max=grid.levels[length(grid.levels)], #1,  #100,
                    centre.y= 0, #grid.min - ((1/9)*(grid.max-grid.min)),
                    plot.extent.x.sf=1,
                    plot.extent.y.sf=1.2,
                    x.centre.range=0.02*(grid.max-centre.y),
                    label.centre.y=FALSE,
                    grid.line.width=0.5,
                    gridline.min.linetype="longdash",
                    gridline.mid.linetype="longdash",
                    gridline.max.linetype="longdash",
                    gridline.min.colour="grey",
                    gridline.mid.colour="#007A87",
                    gridline.max.colour="grey",
                    grid.label.size=5,
                    gridline.label.offset=-0.1*(grid.max-centre.y),
                    label.gridline.min=TRUE,
                    axis.label.offset=1.15,
                    axis.label.size=3,
                    axis.line.colour="grey",
                    group.line.width=1.5,
                    group.point.size=6,
                    group.keep = "ALL",
                    background.circle.colour="#D7D6D1",
                    background.circle.transparency=0.2,
                    plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                    legend.title="",
                    plot.title="",
                    legend.text.size=8 ) {
  
  #library(ggplot2)
  
  plot.data <- as.data.frame(plot.data)
  
  plot.data[,1] <- as.factor(as.character(plot.data[,1]))
  names(plot.data)[1] <- "group"
  
  var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n
  
  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
  
  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")
  #Declare required internal functions
  
  
  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  print(group$path)
  #if(group.keep!="ALL"){
  #  data.path <- group$path
  #  group$path <- data.frame()
  #  for(i in 1:length(unlist(group.keep))){
  #    print(group.keep[i])
  #    data.group <- data.path[data.path[,"group"]==group.keep[i],]
  #    group$path <- rbind(group$path, data.group)
  #  }
  #}
  
  #print(group.order)
  #group$path[,"group"] <- factor(group$path[,"group"], levels = unlist(group.order))
  #group$path <- group$path[with(group$path, order(-group)), ]
  #group$path <- group$path[order(group$path[,1]), ]
  #print(group$path)
  
  grid <- NULL
  grid$path <- CalculateGridPath(plot.data.offset,grid.levels)
  
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  
  # (e) Create Circular grid-lines + labels
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(grid.mid))
  ### Start building up the radar plot
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size=20) + 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))
  
  if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
  
  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
  # then centred labels for axis labels almost immediately above/below x= 0 
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly 
  # identify plot extent when plotting first (base) layer]
  
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1, family=font.radar) +
    scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) + 
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5, family=font.radar)
  
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0, family=font.radar)
  
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text
  base <- base + theme_clear
  
  #  + background circle against which to plot radar data
  #base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
  #                            fill=background.circle.colour,
  #                            alpha=background.circle.transparency)
  
  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)
  
  # ... + grid (cluster) 'paths'
  base <- base + geom_path(data=grid$path,aes(x=x,y=y, group=grid),
                           colour=axis.line.colour)
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group, alpha = 0.5),
                           size=group.line.width)
  
  
  
  # ... + group points (cluster data)
  #base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)
  
  
  #... + amend Legend title
  if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)
  base <-  base + guides(group=TRUE,alpha=FALSE)
  
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  #base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
  #                          lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  #base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
  #                          lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  #base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
  #                          lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
  
  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  #if (label.gridline.min==TRUE) {
    
  #  base <- base + geom_text(aes(x=x,y=y,label=values.radar[1]),data=gridline$min$label,size=grid.label.size*0.8, hjust=1, family=font.radar) }
  #base <- base + geom_text(aes(x=x,y=y,label=values.radar[2]),data=gridline$mid$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  #base <- base + geom_text(aes(x=x,y=y,label=values.radar[3]),data=gridline$max$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  
  
  grid$label <- data.frame()
  for(i in 1:length(grid.levels)){
    grid$label[i,"x"] =  gridline.label.offset
    grid$label[i,"y"] =  grid.min+abs(centre.y) + as.numeric(grid.levels)[i]
    grid$label[i,"label"] = grid.levels[i]
    
  }
                          
  base <- base + geom_text(aes(x=x,y=y,label=label),data=grid$label,size=grid.label.size*0.8, hjust=0.5, family=font.radar)
  
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,size=grid.label.size, hjust=0.5, family=font.radar) }
  
  
  base <- base + theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20,
                                                                                    family = font.radar)) +
    theme(legend.text = element_text(size = legend.text.size), legend.position="left") +
    theme(legend.key.height=unit(2,"line")) +
    scale_colour_manual(values=rep(c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051", 
                                     "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)) +
    theme(text=element_text(family=font.radar)) + 
    theme(legend.title=element_blank())
  
  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }
  
  return(base)
  
}


############################
plot.data <- function(
  report,
  plot
){
  data.in <- report$data
  
  data.out <- data.frame()
  filters <- plot$data$filter
  for(i in  1:length(filters)){
    print("data filter")
    filter <- filters[[i]]
    data <- data.in
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
      else if(!is.null(fil$year)){
        for(k in 1: nrow(data)){
          data[k,fil$year] <- substr(data[k,fil$from],1,5)
        }
        
      }
      else{
        data <- data[data[,fil$variable] == fil$value,]
      }
      #data <- data[data[,fil$variable] == fil$value,]
    }
    
    data.out <- bind_rows(data.out,data)
  }
  
  if(!is.null(plot$data$county.filter)){
    if(plot$data$county.filter==TRUE){
      print("county.filter")
      counties <- report$aliasdata[,c("区县","统计范围")]
      data.out <- merge(x=data.out,y=counties,by ="统计范围",all.x=TRUE)  
      #print(summary(data.out))
      data.province <- data.frame()
      data.county <-  data.frame()
      data.province <- data.out[data.out[,"区县"]==report$province,]
      data.county <- data.out[data.out[,"区县"]==report$county,]
      
      data.out <- rbind(data.province,data.county)
      data.out <- data.out[!is.na(data.out[,"统计范围"]),]
      #print(summary(data.out))
      
    }
  }
  
  if(!is.null(plot$data$keep)){
    print("data keep")
    keeps <- plot$data$keep
    data.in <- data.out
    data.out <- data.frame()
    for(i in 1:length(keeps)){
      keep <- keeps[[i]]
      var <- keep$var
      #print(var)
      values <- keep$value
      for(j in 1:length(values)){
        value <- values[[j]]
        #print(value)
        data.keep <- data.in[data.in[,var] == value,]
        #print(data.keep)
        data.out <- rbind(data.out,data.keep)
      }
    }
  }
  
  
  
  #print(data.out)
  
  #data.out[,g.var$value] <- data.out[,g.var$value]/100.0
  #data.out[,g.var$value] <- round(data.out[,g.var$value], digits = plot$data$digits)
  data.out[,g.var$label] <- round(data.out[,g.var$value], digits = plot$data$digits)
  
  
  if(!is.null(plot$data$derivative)){
    print("data derivative")
    data.in <- data.out
    #print(data.in)
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
        #print(key)
        #print(data[data[,plot$data$derivative$key]==key,g.var$value])
        data.out[nrow(data.out),key] <- 
          data[data[,plot$data$derivative$key]==key,g.var$value]
      }
      
    }
    #print(data.out)
    
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
    #print(data.out)
    print("data aes")
    arg <- aes[[i]]
    data.out[,arg$arg] <- data.out[,arg$var]
    #if(!is.null(arg$order)){
    #  data.out[,arg$arg] <- factor(data.out[,arg$arg],levels = unlist(arg$order))
    #}
    #print(data.out)
  }
  
  if(!is.null(report$aliasFlag)){
    print("aliasFlag")
    data.left <- data.out
    #g.var$scope <-  "统计范围"
    #g.var$alias <-  "别名"
    #print(report$aliasdata)
    data.right <- report$aliasdata[,c(g.var$scope,g.var$alias)]
    scopes <- data.right[,g.var$scope]
    for(i in 1:nrow(data.left)){
      if(is.element(data.left[i,"x"],unlist(scopes))){
        data.out[i,"x"] <- data.right[data.right[,g.var$scope]==data.left[i,"x"],g.var$alias]
      }
      
    }
      
    
  }
  
  if(!is.null(plot$data$aliaskeeper)){
    print("data aliaskeeper")
    keeps <- plot$data$aliaskeeper
    data.in <- data.out
    data.out <- data.frame()
    for(i in 1:length(keeps)){
      keep <- keeps[[i]]
      var <- keep$var
      #print(var)
      values <- keep$value
      for(j in 1:length(values)){
        value <- values[[j]]
        #print(value)
        data.keep <- data.in[data.in[,var] == value,]
        #print(data.keep)
        data.out <- rbind(data.out,data.keep)
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
  #print(data.out)
  
  if(!is.null(plot$data$sort)){
    print("data sort")
    for(i in 1:length(plot$data$sort)){
      sort <- plot$data$sort[[i]]
      #print(data.out)
      data.sort <- data.out[data.out[,sort$variable]==sort$option,]
      #print(data.sort)
      data.sorted <- arrange(data.sort,desc(data.sort[,sort$value]))
      #print(data.sorted)
      sort$list <-list()
      n <- nrow(data.sorted)
      for(i in 1:n){
        if(sort$asc_desc == "DESC"){
          sort$list[[i]] <- data.sorted[i,sort$var]
        }
        else{
          sort$list[[i]] <- data.sorted[n-i+1,sort$var]
        }
        
        #print(sort$list[[i]])
      }
      data.out[,sort$var] <- factor(data.out[,sort$var], levels = unlist(sort$list))
    }
    
  }
  
  
  
  
  print(data.out)
  return(data.out)
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
  #print(plot$data$set)
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
#############################
plot.bar_stacking.raw  <- function(
  plot
){
  print(plot$data$set)
  figure <- ggplot(data=plot$data$set, aes(x=x, y=y , fill = fill)) + scale_fill_economist()
  #figure <- figure + theme_economist() + scale_fill_economist()
  figure <- figure + geom_bar( stat="identity", width = plot$ggplot$bar$width,position = "stack") 
  figure <- figure + geom_text(aes(label = label), colour="red", position = position_stack(vjust = 0.5)) 
  figure <- figure + guides(fill=FALSE) 
  figure <- figure + coord_flip()
   
  

  
  figure <- figure + theme(plot.background = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major = element_line(colour="grey", size=0.4, linetype = "dashed") ) +
    theme(panel.grid.minor.y = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(legend.background = element_blank()) +
    theme(text = element_text(family = "wqy-microhei"))+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          plot.background=element_blank())
  #figure <- figure + theme(panel.background = element_blank()) 
  #figure <- figure + theme(text = element_text(family = "wqy-microhei"))
  
  return(figure)
}

#############################
plot.bar_dodging  <- function(
  plot
){
  #print(plot$data$set)
 
  figure <- ggplot(data=plot$data$set, aes(x=x, y=y , fill = fill))
  figure <- figure + theme_economist() + scale_fill_economist()
  figure <- figure + geom_bar( stat="identity", width = plot$ggplot$bar$width,position = plot$ggplot$position) 
  if(!is.null(plot$ggplot$text)){
    if(!is.null(plot$ggplot$text$vjust)){
      figure <- figure + geom_text(aes(label = label),colour="red", 
                                   position = position_stack(vjust = plot$ggplot$text$vjust)) 
    }
    else if(!is.null(plot$ggplot$text$hjust)){
      figure <- figure + geom_text(aes(label = label),colour="red", 
                                   position = position_dodge(width = plot$ggplot$text$hjust)) 
    }
    
    
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
    #theme(panel.grid.major.x = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    #theme(panel.grid.major.y = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
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
  if(!is.null(plot$ggplot$box$error.width)){
    error.width <- plot$ggplot$box$error.width
  }
  else{
    error.width <- 0.1
  }
  figure <- figure + geom_errorbar(aes(x=x, ymin=ymin,ymax=ymax),size = 0.5, width = error.width)
  if(!is.null(plot$ggplot$box$box.width)){
    box.width <- plot$ggplot$box$box.width
  }
  else{
    box.width <- 0.4
  }
  figure <- figure + geom_boxplot( stat="identity", 
                                   aes(x =x,width =  box.width,ymin=ymin, lower=lower,middle=middle,upper=upper,ymax=ymax,fill = fill)
                                  ) 
  figure <- figure + geom_point(shape=20, size=3,colour="red")
  #figure <- figure + geom_text(aes(label = label), position = position_stack(vjust = 0.5)) 
  #figure <- figure + ggtitle(plot$fig.name) 
  if(!is.null(plot$ggplot$axisy)){
    figure <- figure + scale_y_continuous(limits=unlist(plot$ggplot$axisy$continuous_limits))
    if(!is.null(plot$ggplot$axisy$ylim)){
      figure <- figure + coord_cartesian(ylim=unlist(plot$ggplot$axisy$ylim))
    }
  }
  
  figure <- figure + xlab(plot$ggplot$label$xlab) + ylab(plot$ggplot$label$ylab) 
  figure <- figure + guides(fill=FALSE, alpha=FALSE) 
  
  figure <- figure + theme_economist() + scale_fill_economist()
  figure <- figure + theme(plot.background = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.major.y  = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.minor.y = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(panel.grid.major.x  = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
    theme(legend.background = element_blank()) 
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
  print(plot$data$set)
  data.in <- plot$data$set
  print(plot$data$keep)
  if(!is.null(plot$data$keep)){
    keeps <- plot$data$keep
    data <- data.frame()
    for(i in 1:length(keeps)){
      keep <- keeps[[i]]
      var <- keep$var
      print(var)
      values <- keep$value
      for(j in 1:length(values)){
        value <- values[[j]]
        print(value)
        data.keep <- data.in[data.in[,var] == value,]
        print(data.keep)
        data <- rbind(data,data.keep)
      }
    }
  }
  else{
    data <- plot$data$set
  }
  
  print(data)
  data.radar <- data.frame()
  groups <- levels(factor(data[,"x"] ))
  for(i in 1:length(groups)){
    group <- groups[[i]]
    data.group <- data[data[,"x"]==group,]
    data.radar[i,"group"] <- groups[[i]]
    for(j in 1:nrow(data.group)){
      data.radar[i,data.group[j,g.var$topic]] <- data.group[j,g.var$value]
    }
  }
  
  #print(data.radar)
  figure <- myggradar(data.radar, 
                      grid.levels = c(1,2,3,4,5,6,7,8,9),
                    grid.min=0,  #10,
                    grid.mid=5,  #50,
                    grid.max=9,
                    font.radar="wqy-microhei"
                    #group.keep = c("本市","73区")
                    ) 
  return(figure)
}
#############################
plot.figure  <- function(
  report,
  plot.in,
  fig_name = NULL
){
  plot <-  plot.in
  if(is.null(plot$fig.limit)){
    
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
      else if(plot$ggplot$geom == "plot_bar_stacking.raw"){
        figure  <-  plot.bar_stacking.raw(plot)
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
  else{
    print("plot multi figure")
    plot.multi.figure(
      report,
      plot.in,
      fig_name
    )
  }
  
  
}

##########################################

#############################
plot.multi.figure  <- function(
  report,
  plot.in,
  fig_name = NULL
){
  plot <-  plot.in
  
  plot$data$set.multi.fig <- plot.data(report,plot)
  
  items  <- levels(factor(plot$data$set.multi.fig[,"x"]))
  #print(items)
  fig.number <-  floor( length(items)/plot$fig.limit)+1
  #print(fig.number)
  fig.items <- floor(length(items)/fig.number)+1
  
  tex.name <- paste0(plot$entry,plot$ext.tex)
  tex.file <- paste0(report$plot.out,tex.name)
  tex.out  <- c("","")
  for(i in 1:fig.number){
    
    plot$output <-  report$output[[1]]
    plot$title <- report$title
    plot$dir  <- report$plot.out
    caption <- paste0(plot$fig.name,"-",LETTERS[i])
    plot.fig.name <- paste0(plot$title,"_",caption,"_",report.datetime(),plot$output$ext)
    plot$file <- paste0(plot$dir,plot.fig.name)
    print(plot$file)
    
    tex.out[length(tex.out)+1] <- paste0("`r fig_name <- \"",caption,"\" `")
    tex.out[length(tex.out)+1] <- paste0("`r fig_file <- \"",plot$file,"\" `")
    tex.out[length(tex.out)+1] <-  " "
    tex.out[length(tex.out)+1] <- "\\begin{figure}[H]"
    tex.out[length(tex.out)+1] <- "\\includegraphics[width=25cm]{`r fig_file`}"
    tex.out[length(tex.out)+1] <- "\\caption{`r fig_name`}"
    tex.out[length(tex.out)+1] <- "\\label{fig:`r fig_name`}"
    tex.out[length(tex.out)+1] <- "\\end{figure}"
    tex.out[length(tex.out)+1] <-  " "
    tex.out[length(tex.out)+1] <-  " "
    
    
    
    df <- data.frame()
    plot$data$set  <-  data.frame()
    order.list <-  list()
    for(j in 1:length(plot$x.leader)){
      df  <-  plot$data$set.multi.fig[plot$data$set.multi.fig[,"x"]==plot$x.leader[j],]
      order.list[[j]] <- plot$x.leader[j]
      plot$data$set <- rbind(plot$data$set,df)
    }
    for(j in ((i-1)*fig.items+1):min(length(items),i*fig.items)){
      if(!is.element(items[j],unlist(plot$x.leader))){
        df  <-  plot$data$set.multi.fig[plot$data$set.multi.fig[,"x"]==items[j],]
        n <- length(order.list)
        order.list[[n+1]] <- items[j]
        plot$data$set <- rbind(plot$data$set,df)
      }
    }
    plot$data$set[,"x"] <- factor(plot$data$set[,"x"], levels = unlist(order.list))
    
    
    if(nrow(plot$data$set)>0){
      pdf(plot$file, width = plot$ggplot$width, height = plot$ggplot$height)
      showtext.begin()
    
      if(plot$ggplot$geom == report$geom$bar_dodging){
        figure  <-  plot.bar_dodging(plot)
      }
      else if(plot$ggplot$geom == report$geom$bar_stacking){
        figure  <-  plot.bar_stacking(plot)
      }
      else if(plot$ggplot$geom == "plot_bar_stacking.raw"){
        figure  <-  plot.bar_stacking.raw(plot)
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
    
  }
  
  fileConn<-file(tex.file)
  writeLines(tex.out, fileConn)
  close(fileConn)
  
  return(tex.file)
  
  
}


