## PlotFigure class for green index data processing and report generation


## flag for source loaded checking
plotfigure.loaded <- TRUE

## define 
library(methods)
library(ggplot2)
library(showtext)
library(ggthemes)
library(ggsci)
library(dplyr)
library(ggrepel)

GreenIndexPlotFigure <- setRefClass(
  "GreenIndexPlotFigure",
  contains = "GreenIndexQueryData",
  
  fields = list(
    plot.df = "data.frame",       # plot parameter table
    
    plot.param = "data.frame",    # parameter for one plot
    
    plot.data = "data.frame",     # data for one plot
    
    plot.order.x = "character"
  
  ),
  
  methods = list(
    PrepareDataframe = function() {
      callSuper()
      jobs <- config$GetConfigJob()$plotfigure
      dataframe <- jobs$dataframe
      plot.table <- paste0(dataframe$plot$table, 
                           dataframe$plot$suffix)
      plot.df <<- database$ReadTable(plot.table)
      
      
    },
    
    OrderData = function(in.df, order.column, order.string) {
      plot.data <<- in.df
      plot.data[, order.column] <<- 
        factor(plot.data[, order.column],
               levels = unlist(strsplit(order.string, kSeparator)))
    },
    
    SortPlotData = function() {
      if (plot.param[1, kColumnSortX] == kStringNone) {
        print(plot.data)
        OrderData(plot.data, plot.param[1, kColumnAxisX],
                  plot.param[1, kColumnOrderX])
        print(plot.data)
      } else if(plot.param[1, kColumnSortX] == kSortAsc) {
        
      }
    },
    
    PreparePlotData = function(plot.code) {
      
      plot.param <<- plot.df[plot.df[, kColumnPlotCode] == plot.code, ]
      
      if (nrow(plot.param) != 1) {
        LogError(paste("plot_code", plot.code, "error!"))  
        return(NULL)
      }
      
      plot.data <<- QueryData(plot.code)
      if (nrow(plot.data) < 1) {
        LogError(paste("plot.data", plot.code, "error!"))
        return(NULL)
      }
      
      # prepare x, y, label, fill column
      plot.data[, kColumnAxisX] <<- 
        plot.data[, plot.param[1, kColumnAxisX] ]
      plot.data[, kColumnAxisY] <<- 
        plot.data[, plot.param[1, kColumnAxisY] ]
      plot.data[, kColumnLabel] <<- 
        plot.data[, plot.param[1, kColumnLabel] ]
      
      fill <- unlist(strsplit(plot.param[1, kColumnFill], kSeparator))
      plot.data[, kColumnFill] <<- ""
      for (i in 1:length(fill)) {
        plot.data[, kColumnFill] <<- paste0(plot.data[, kColumnFill], 
                                              plot.data[, fill[i]])
      }
      
      # write.csv2(plot.data, "plot.data.csv")
      return(plot.data)
    },
    
    FigurePlot = function() {
      
      if (plot.param[1, kColumnFillOrder] == kStringNone){
        
      } else {
        fill.order <- unlist(strsplit(plot.param[1, kColumnFillOrder], 
                                       kSeparator))
        plot.data[, kColumnFill] <<- factor(plot.data[, kColumnFill],
                                            levels = fill.order)
        
      }
      
      figure <- ggplot(data = plot.data, 
                       aes(x = plot_x, 
                           y = plot_y, 
                           fill = plot_fill, 
                           label = plot_label))
      return(figure)
    },
    
    FigureTheme = function(figure, theme.name) {
      if (theme.name == "economist") {
        figure <- figure + theme_economist() + scale_fill_economist()
      } else if (theme.name == "npg") {
        figure <- figure + scale_color_npg() + scale_fill_npg()
      }
      
      figure <- figure + theme(plot.background = element_blank()) +
        theme(panel.background = element_blank()) +
        theme(panel.grid.major = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
        theme(panel.grid.major.y  = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
        theme(panel.grid.major.x  = element_line(colour="grey", size=0.2, linetype = "dashed") ) +
        theme(legend.background = element_blank()) +
        theme(text = element_text(family = "wqy-microhei"))
      
      return(figure)
      
    },
    
    FigureLabel = function(figure) {
      
      plot.label.x <- plot.param[1, kColumnLabelX]
      plot.label.y <- plot.param[1, kColumnLabelY]
      
      if (plot.label.x == kStringNull) {
        plot.label.x <- ""
      }
      
      if (plot.label.y == kStringNull) {
        plot.label.y <- ""
      }
      
      figure <- figure + labs(title = plot.param[1, kColumnPlotTitle],
                              x = plot.label.x, y = plot.label.y)
      figure <- figure + 
       theme(axis.text.x = element_text(
         angle = as.numeric(plot.param[1, kColumnTextAngleX]), 
         vjust = 1, hjust = 1))
      
      if (plot.param[1, kColumnLabelPosition] == kStringNone) {
        
      } else if (plot.param[1, kColumnLabelPosition] == kPositionDodge) {
        figure <- figure + 
          geom_text(aes(label = plot_label), # , y = plot_y/2 ), 
                    colour = plot.param[1, kColumnLabelColour], #
                    size = as.numeric(plot.param[1, kColumnLabelSize]),
                    position = 
                      position_dodge(
                        as.numeric(plot.param[1, kColumnPlotBarWidth])),
                    vjust = as.numeric(plot.param[1, kColumnLabelVjust])) #,
                    #hjust = as.numeric(plot.param[1, kColumnLabelHjust]))
        
      } else if (plot.param[1, kColumnLabelPosition] == kPositionStack) {
        figure <- figure + 
          geom_text(aes(label = plot_label), 
                    colour = plot.param[1, kColumnLabelColour], #
                    size = as.numeric(plot.param[1, kColumnLabelSize]),
                    position = 
                      position_stack(vjust = 
                                       as.numeric(
                                         plot.param[1, kColumnLabelVjust])))  
      } 
      # figure <- figure + geom_text_repel()
      return(figure)
    },
    
    FigureSortX = function(figure) {
      
      if (plot.param[1, kColumnSortX] == kStringNone) {
        
        plot.order.x <<- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
      } else if(plot.param[1, kColumnSortX] == kSortAscAll) {
        
        df <- arrange(plot.data, desc(plot.data[, kColumnValue]))
        plot.order.x <<- unlist(df[, kColumnAxisX])
        plot.order.x <<- rev(plot.order.x)
      } else if(plot.param[1, kColumnSortX] == kSortDescAll) {
        
        df <- arrange(plot.data, desc(plot.data[, kColumnValue]))
        plot.order.x <<- unlist(df[, kColumnAxisX])
      } else if(plot.param[1, kColumnSortX] == kSortAsc) {
        
        order <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        order <- order[1:length(order)-1]
        df <- plot.data[!(plot.data[, kColumnAxisX] %in% order), ]
        df <- arrange(df, desc(df[, kColumnValue]))
        order.x <- df[, kColumnAxisX]
        order.x <- rev(order.x)
        plot.order.x <<- unlist(append(order, order.x))
      } else if(plot.param[1, kColumnSortX] == kSortDesc) {
        
        order <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        order <- order[1:length(order)-1]
        df <- plot.data[!(plot.data[, kColumnAxisX] %in% order), ]
        df <- arrange(df, desc(df[, kColumnValue]))
        order.x <- df[, kColumnAxisX]
        plot.order.x <<- unlist(append(order, order.x))
      } 
      
      figure <- figure + scale_x_discrete(limits = plot.order.x)
      
      return(figure)
    },
    
    FigureLimitY = function(figure) {
      
      if (plot.param[1, kColumnDiscreteY] != kStringNone) {
        limit.y <- unlist(strsplit(plot.param[1, kColumnDiscreteY],
                                   kSeparator))
        figure <- figure + scale_y_discrete(limits = limit.y)
      } else if (plot.param[1, kColumnLimitY] != kStringNone) {
        limit.y <- as.numeric(unlist(strsplit(plot.param[1, kColumnLimitY],
                                   kSeparator)))
        figure <- figure + 
          scale_y_continuous(limits = limit.y,
                             breaks=seq(limit.y[1], limit.y[2],
                                        round((limit.y[2]-limit.y[1])/10)))
      } else {
        
      }
      
      return(figure)
    },
    
    FigureLegend = function(figure) {
      
      figure <- figure + 
        guides(fill = guide_legend(keywidth = 
                                     plot.param[1, kColumnLegendWidth], 
                                   keyheight = 
                                     plot.param[1, kColumnLegendHeight]))
      
      figure <- figure +  
        theme(legend.position = plot.param[1, kColumnLegendPosition],
              legend.direction =  plot.param[1, kColumnLegendDirection],
              legend.text = 
                element_text(size = plot.param[1, kColumnLegendFontSize]),
              legend.title=element_blank())
      
      return(figure)
    },
    
    FigureCoord = function(figure) {
      if (plot.param[1, kColumnPlotCoord] == kStringNone) {
        
      } else if (plot.param[1, kColumnPlotCoord] == kCoordFlip) {
        figure <- figure + coord_flip()
      } else if (plot.param[1, kColumnPlotCoord] == kCoordPolar) {
        figure <- figure + coord_polar()
      } 
      
      return(figure)
    },
    
    PlotGeomBarDodge = function() {
 
      figure <- FigurePlot()
      
      figure <- figure + 
        geom_bar( stat="identity", 
                  width = as.numeric(plot.param[1, kColumnPlotBarWidth]),
                  position = plot.param[1, kColumnPlotBarPosition] ) 
      
      #figure <- FigureTheme(figure, "economist")  #npg
      
      figure <- FigureLabel(figure)
      
      figure <- FigureLegend(figure)
      
      figure <- FigureSortX(figure)
      
      figure <- FigureLimitY(figure)
      
      figure <- FigureCoord(figure)
      
      return(figure)
      
    },
    
    PlotFigure = function(plot.dir, plot.code) {
      PreparePlotData(plot.code)
      
      plot.code.name <- gsub("\\.+", "_", plot.param[1, kColumnPlotCode])
      # plot.code.name <- plot.param[1, kColumnPlotCode]
      plot.file <- paste0(plot.dir, plot.code.name,".pdf")
      
      pdf(plot.file, width = as.numeric(plot.param[1, kColumnPlotWidth]) , 
          height = as.numeric(plot.param[1, kColumnPlotHeight]) )
      
      showtext_begin()
      
      if (plot.param[1, kColumnPlotGeom] == kPlotGeomBarDodge) {
        figure <- PlotGeomBarDodge()
      } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomBarStack) {
        figure <- PlotGeomBarDodge()
      } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomScatter) {
        figure <- PlotGeomScatter()
      } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomBox) {
        figure <- PlotGeomBox()
      }
      
      
      print(figure)
      
      showtext_end()
      dev.off()
      
      plot.fig <- list()
      plot.fig$name <- plot.param[1, kColumnPlotTitle]
      plot.fig$path <- paste0(getwd(), "/", plot.file)
      
      return(plot.fig)
      
    },
    
    GetPlotFigure = function(plot.dir, plot.code) {
      PreparePlotData(plot.code)
      
      plot.code.name <- gsub("\\.+", "_", plot.param[1, kColumnPlotCode])
      # plot.code.name <- plot.param[1, kColumnPlotCode]
      plot.file <- paste0(plot.dir, plot.code.name,".pdf")
      
      plot.fig <- list()
      plot.fig$name <- plot.param[1, kColumnPlotTitle]
      plot.fig$path <- paste0(getwd(), "/", plot.file)
      
      return(plot.fig)
      
    }
  )
)
