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
# library(Cairo)
# library(animation)

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
    
    PreparePlotData = function(plot.code) {
      
      plot.param <<- plot.df[plot.df[, kColumnPlotCode] == plot.code, ]
      
      if (nrow(plot.param) != 1) {
        LogError(paste("plot_code", plot.code, "param not available!"))  
        return(NULL)
      }
      
      plot.data <<- QueryData(plot.code)
      #print(plot.data)
      if (nrow(plot.data) < 1) {
        LogError(paste("plot.data", plot.code, "query error!"))
        return(NULL)
      }
      
      
      # prepare x, y, label, fill column
      plot.data[, kColumnAxisX] <<- 
        plot.data[, plot.param[1, kColumnAxisX] ]
      plot.data[, kColumnAxisY] <<- 
        plot.data[, plot.param[1, kColumnAxisY] ]
      if (plot.param[1, kColumnLabel] != kColumnLabel) {
        plot.data[, kColumnLabel] <<- plot.data[, plot.param[1, kColumnLabel]]
      }
      
      
      fill <- unlist(strsplit(plot.param[1, kColumnFill], kSeparator))
      plot.data[, kColumnFill] <<- ""
      for (i in 1:length(fill)) {
        plot.data[, kColumnFill] <<- paste0(plot.data[, kColumnFill], 
                                              plot.data[, fill[i]])
      }
      
      if (plot.param[1, kColumnFillSkip] != kStringNone) {
        plot.data <<- plot.data[plot.data[,kColumnFill] != 
                                     plot.param[1, kColumnFillSkip],]
      }
      
      if (plot.param[1, kColumnLegendPosition] != kStringNone && 
          plot.param[1, kColumnLegendOrder] != kStringNone ) {
        plot.data[, kColumnFill] <<-  
          factor(plot.data[, kColumnFill],
                 levels = unlist(strsplit(plot.param[1, kColumnLegendOrder], 
                                          kSeparator)))
      }
      
      if (plot.param[1, kColumnFacet] != kStringNone) {
        plot.data[, kColumnFacet] <<- plot.data[, plot.param[1, kColumnFacet]] 
        plot.data[, kColumnFacet] <<- 
          factor(plot.data[, kColumnFacet],
                 levels = unlist(strsplit(plot.param[1, kColumnFacetOrder], 
                                          kSeparator)))
      }
      
      # limit plot_x length
      if (nrow(plot.data) > 0){
        for (i in 1:nrow(plot.data)) {
          
          if (nchar(plot.data[i, kColumnAxisX]) > kAxisXTextLimit){
            plot.data[i, kColumnAxisX] <<- 
              substr(plot.data[i, kColumnAxisX], 1, kAxisXTextLimit)
          }
        }
      }
      write.csv2(plot.data, "plot.data.csv")
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
    
    FigureFacet = function(figure) {
      if (plot.param[1, kColumnFacet] != kStringNone) {
        
        figure <- figure + 
          facet_wrap(facets = ~plot_facet, nrow = 1,
                     strip.position = plot.param[1, kColumnFacetPosition],
                     scales = plot.param[1, kColumnFacetScale])
      }
      return(figure)
    },
    
    FigureTheme = function(figure, theme.name) {
      
      
      if (theme.name == "economist") {
        figure <- figure + theme_economist() + scale_fill_economist()
      } else if (theme.name == "npg") {
        figure <- figure + theme_economist() + 
          scale_color_npg() + scale_fill_npg()
      }  else if (theme.name == "aaas") {
        figure <- figure + theme_economist() + 
          scale_color_aaas() + scale_fill_aaas()
      }  else if (theme.name == "nejm") {
        figure <- figure + theme_economist() + 
          scale_color_nejm() + scale_fill_nejm()
      } else if (theme.name == "lancet") {
        figure <- figure + theme_economist() + 
          scale_color_lancet() + scale_fill_lancet()
      } else if (theme.name == "jama") {
        figure <- figure + theme_economist() + 
          scale_color_jama() + scale_fill_jama()
      } else if (theme.name == "jco") {
        figure <- figure + theme_economist() + 
          scale_color_jco() + scale_fill_jco()
      } else if (theme.name == "ucscgb") {
        figure <- figure + theme_economist() + 
          scale_color_ucscgb() + scale_fill_ucscgb()
      } else if (theme.name == "d3") {
        figure <- figure + theme_economist() + 
          scale_color_d3() + scale_fill_d3()
      } else if (theme.name == "locuszoom") {
        figure <- figure + theme_economist() + 
          scale_color_locuszoom() + scale_fill_locuszoom()
      } else if (theme.name == "igv") {
        figure <- figure + theme_economist() + 
          scale_color_igv() + scale_fill_igv()
      } else if (theme.name == "cosmic") {
        figure <- figure + theme_economist() + 
          scale_color_cosmic("hallmarks_light") + 
          scale_fill_cosmic("hallmarks_light")
      } else if (theme.name == "uchicago") {
        figure <- figure + theme_economist() + 
          scale_color_uchicago() + scale_fill_uchicago()
      } else if (theme.name == "startrek") {
        figure <- figure + theme_economist() + 
          scale_color_startrek() + scale_fill_startrek()
      } else if (theme.name == "tron") {
        figure <- figure + theme_economist() + 
          scale_color_tron() + scale_fill_tron()
      } else if (theme.name == "futurama") {
        figure <- figure + theme_economist() + 
          scale_color_futurama() + scale_fill_futurama()
      } else if (theme.name == "rickandmorty") {
        figure <- figure + theme_economist() + 
          scale_color_rickandmorty() + scale_fill_rickandmorty()
      } else if (theme.name == "simpsons") {
        figure <- figure + theme_economist() + 
          scale_color_simpsons() + scale_fill_simpsons()
      } else if (theme.name == "gsea") {
        figure <- figure + theme_economist() + 
          scale_color_gsea() + scale_fill_gsea()
      } 
      
      figure <- figure + 
        # theme(plot.background = element_blank()) +
        theme(plot.background = 
                element_rect(fill = "white", colour = "grey90", size = 1)) +
        # theme(plot.margin = unit(c(0.6, 1, 0.6, 1), "cm")) +
        theme(panel.background = element_blank()) +
        theme(panel.grid.major = 
                element_line(colour="grey", size=0.2, linetype = "dashed") ) +
        theme(panel.grid.major.y  = 
                element_line(colour="grey", size=0.2, linetype = "dashed") ) +
        theme(panel.grid.major.x  = 
                element_line(colour="grey", size=0.2, linetype = "dashed") ) +
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
         size = as.numeric(plot.param[1, kColumnTextSizeX]),
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
    
    FigureLabelTitleX = function(figure) {
      
      plot.label.x <- plot.param[1, kColumnLabelX]
      
      if (plot.label.x == kStringNull) {
        plot.label.x <- ""
      }
      
      figure <- figure + labs(title = plot.param[1, kColumnPlotTitle],
                              x = plot.label.x)
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
    
    FigureLabelXY = function(figure) {
      
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
          size = as.numeric(plot.param[1, kColumnTextSizeX]),
          vjust = 1, hjust = 1))
      figure <- figure + geom_text_repel()
      return(figure)
    },
    
    FigureSortX = function(figure) {
      
      if (plot.param[1, kColumnSortX] == kStringNone) {
        if (plot.param[1, kColumnOrderX] == kStringNone) {
          return(figure)
        } else {
          plot.order.x <<- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        }
        
      } else if(plot.param[1, kColumnSortX] == kSortAscAll) {
        sort.df <- 
          plot.data[plot.data[, plot.param[1, kColumnSortXBy]] == 
                      plot.param[1, kColumnSortXSubset], 
                    c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
        sort.df <- 
          arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
        plot.order.x <<- unlist(sort.df[, kColumnAxisX])
        
        # df <- arrange(plot.data, desc(plot.data[, kColumnValue]))
        # plot.order.x <<- unlist(df[, kColumnAxisX])
        plot.order.x <<- rev(plot.order.x)
      } else if(plot.param[1, kColumnSortX] == kSortDescAll) {
        sort.df <- 
          plot.data[plot.data[, plot.param[1, kColumnSortXBy]] == 
                      plot.param[1, kColumnSortXSubset], 
                    c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
        sort.df <- 
          arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
        plot.order.x <<- unlist(sort.df[, kColumnAxisX])
        
        # df <- arrange(plot.data, desc(plot.data[, kColumnValue]))
        # plot.order.x <<- unlist(df[, kColumnAxisX])
      } else if(plot.param[1, kColumnSortX] == kSortAsc) {
        
        order <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        order <- order[1:length(order)-1]
        df <- plot.data[!(plot.data[, kColumnAxisX] %in% order), ]
        
        sort.df <- 
          df[df[, plot.param[1, kColumnSortXBy]] == 
                      plot.param[1, kColumnSortXSubset], 
                    c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
        sort.df <- 
          arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
        order.x <<- unlist(sort.df[, kColumnAxisX])
        
        # df <- arrange(df, desc(df[, kColumnValue]))
        # order.x <- df[, kColumnAxisX]
        order.x <- rev(order.x)
        plot.order.x <<- unlist(append(order, order.x))
      } else if(plot.param[1, kColumnSortX] == kSortDesc) {
        
        order <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        order <- order[1:length(order)-1]
        df <- plot.data[!(plot.data[, kColumnAxisX] %in% order), ]
        sort.df <- 
          df[df[, plot.param[1, kColumnSortXBy]] == 
               plot.param[1, kColumnSortXSubset], 
             c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
        sort.df <- 
          arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
        order.x <<- unlist(sort.df[, kColumnAxisX])
        
        # df <- arrange(df, desc(df[, kColumnValue]))
        # order.x <- df[, kColumnAxisX]
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
          scale_y_continuous(limits = limit.y[1:2],
                             breaks=seq(limit.y[1], limit.y[2],
                                       # round((limit.y[2]-limit.y[1])/10)))
                                       limit.y[3]))
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
                  alpha = as.numeric(plot.param[1, kColumnFillAlpha]),
                  width = as.numeric(plot.param[1, kColumnPlotBarWidth]),
                  position = plot.param[1, kColumnPlotBarPosition] ) 
      
      figure <- FigureTheme(figure, plot.param[1, kColumnPlotTheme])
      
      figure <- FigureLabel(figure)
      
      figure <- FigureLegend(figure)
      
      figure <- FigureSortX(figure)
      
      figure <- FigureLimitY(figure)
      
      figure <- FigureCoord(figure)
      
      figure <- FigureFacet(figure)
      
      
      return(figure)
      
    },
    
    PlotGeomBoxWhisker = function() {
      
      if (plot.param[1, kColumnFillOrder] == kStringNone){
        
      } else {
        fill.order <- unlist(strsplit(plot.param[1, kColumnFillOrder], 
                                      kSeparator))
        plot.data[, kColumnFill] <<- factor(plot.data[, kColumnFill],
                                            levels = fill.order)
        
      }
      
      plot.merge <- plot.data[plot.data[, kColumnKey] == kMean, ]
      names(plot.merge) <- sub(paste0("^", kColumnAxisY, "$"), 
                            kMean, names(plot.merge))
      
      plot.min <- plot.data[plot.data[, kColumnKey] == kMin, 
                            c(kColumnAxisX, kColumnValue)]
      names(plot.min) <- sub(paste0("^", kColumnValue, "$"), 
                            kMin, names(plot.min))
      plot.merge <- merge(plot.merge, plot.min, by = kColumnAxisX, all.x = TRUE)
      
      plot.max <- plot.data[plot.data[, kColumnKey] == kMax, 
                            c(kColumnAxisX, kColumnValue)]
      names(plot.max) <- sub(paste0("^", kColumnValue, "$"), 
                             kMax, names(plot.max))
      plot.merge <- merge(plot.merge, plot.max, by = kColumnAxisX, all.x = TRUE)
      
      plot.median <- plot.data[plot.data[, kColumnKey] == kMedian, 
                            c(kColumnAxisX, kColumnValue)]
      names(plot.median) <- sub(paste0("^", kColumnValue, "$"), 
                             kMedian, names(plot.median))
      plot.merge <- merge(plot.merge, plot.median, by = kColumnAxisX, all.x = TRUE)
      
      plot.upper <- plot.data[plot.data[, kColumnKey] == kUpper, 
                            c(kColumnAxisX, kColumnValue)]
      names(plot.upper) <- sub(paste0("^", kColumnValue, "$"), 
                             kUpper, names(plot.upper))
      plot.merge <- merge(plot.merge, plot.upper, by = kColumnAxisX, all.x = TRUE)
      
      plot.lower <- plot.data[plot.data[, kColumnKey] == kLower, 
                              c(kColumnAxisX, kColumnValue)]
      names(plot.lower) <- sub(paste0("^", kColumnValue, "$"), 
                               kLower, names(plot.lower))
      plot.merge <- merge(plot.merge, plot.lower, by = kColumnAxisX, all.x = TRUE)
      
      plot.merge[, kColumnValue] <- plot.merge[, kMedian]
      plot.data <<- plot.merge
      
      figure <- ggplot(
        data = plot.data, 
        aes(x = plot_x, y = mean, 
            alpha = as.numeric(plot.param[1, kColumnFillAlpha])))
      
      figure <- figure + geom_errorbar(aes(x = plot_x, ymin = min, ymax = max),
                                       size = 0.5, width = 0.3)
      figure <- figure + 
        geom_boxplot(stat="identity",
                     width = as.numeric(plot.param[1, kColumnPlotBarWidth]),
                       aes(x = plot_x, 
                           # y = mean, 
                           ymin = min, 
                           ymax = max,
                           upper = upper,
                           lower = lower,
                           middle = median,
                           fill = plot_fill
                           # label = plot_label,
                           
                           # position = plot.param[1, kColumnPlotBarPosition]
                           ))
      
      figure <- figure + geom_point(shape = 20, size = 2, colour = "red")
      
      figure <- FigureTheme(figure, plot.param[1, kColumnPlotTheme])
      
      figure <- FigureLabel(figure)
      
      figure <- FigureLegend(figure)
      
      figure <- FigureSortX(figure)
      
      figure <- FigureLimitY(figure)
      
      figure <- FigureCoord(figure)
      
      return(figure)
      
    },
    
    PlotGeomScatter = function() {
      
      if (plot.param[1, kColumnFillOrder] == kStringNone){
        
      } else {
        fill.order <- unlist(strsplit(plot.param[1, kColumnFillOrder], 
                                      kSeparator))
        plot.data[, kColumnFill] <<- factor(plot.data[, kColumnFill],
                                            levels = fill.order)
        
      }
      
      plot.xy <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
      n <- length(plot.xy)
      # LogDebug(paste("plot_x = ", plot.xy[1], ", plot_y = ", plot.xy[2]))
      
      plot.x <- plot.data[plot.data[, kColumnAxisX] == plot.xy[1], ]
      plot.x[, kColumnAxisX] <- plot.x[, kColumnAxisY]
      i <- 2
      while (i < n) {
        plot.x[, kColumnAxisY] <- NULL
        plot.y <- plot.data[plot.data[, kColumnAxisX] == plot.xy[i], 
                            c(kColumnAlias, kColumnAxisY)]
        plot.x <- merge(plot.x, plot.y, by = kColumnAlias, all.x = TRUE)
        plot.x[, "tmp"] <- as.numeric(plot.x[, kColumnAxisY]) + 
          as.numeric(plot.x[, kColumnAxisX])
        plot.x[, kColumnAxisX] <- plot.x[, "tmp"]
        plot.x[, "tmp"] <- NULL
        i <- i + 1
      }
      
      plot.x[, kColumnAxisY] <- NULL
      plot.y <- plot.data[plot.data[, kColumnAxisX] == plot.xy[n], 
                          c(kColumnAlias, kColumnAxisY)]
      plot.merge <- merge(plot.x, plot.y, by = kColumnAlias, all.x = TRUE)
      
      plot.data <<- plot.merge
      
      figure <- ggplot(data = plot.data, 
                       aes(x = plot_x, y = plot_y, label = plot_label))
      
      figure <- figure + 
        geom_point(stat="identity", 
                   colour="white", 
                   shape=21, size = 4,
                   aes(alpha = as.numeric(plot.param[1, kColumnFillAlpha]), 
                       fill = factor(plot_fill)))
      
      figure <- FigureTheme(figure, plot.param[1, kColumnPlotTheme])
      
      figure <- FigureLabelXY(figure)
      
      figure <- FigureLegend(figure)
      
      #figure <- FigureSortX(figure)
      
      figure <- FigureLimitY(figure)
      
      figure <- FigureCoord(figure)
      
      # figure <- figure + geom_text_repel()
      
      return(figure)
      
    },
    
    
    PlotGeomWindRose = function() {
      
      figure <- FigurePlot()
      
      figure <- figure + 
        geom_bar( stat="identity", 
                  alpha = as.numeric(plot.param[1, kColumnFillAlpha]),
                  width = as.numeric(plot.param[1, kColumnPlotBarWidth]),
                  position = plot.param[1, kColumnPlotBarPosition] ) 
      
      
      if (plot.param[1, kColumnPlotTheme] == "economist") {
        figure <- figure + scale_fill_economist()
      } else if (plot.param[1, kColumnPlotTheme] == "npg") {
        figure <- figure + scale_color_npg() + scale_fill_npg()
      } else if (plot.param[1, kColumnPlotTheme] == "d3") {
        figure <- figure + scale_color_d3() + scale_fill_d3()
      } else if (plot.param[1, kColumnPlotTheme] == "jco") {
        figure <- figure + scale_color_jco() + scale_fill_jco()
      } else if (plot.param[1, kColumnPlotTheme] == "ucscgb") {
        figure <- figure + scale_color_ucscgb() + scale_fill_ucscgb()
      }
      
      
      
      
      figure <- FigureLabel(figure)
      
      figure <- FigureLegend(figure)
      
      figure <- FigureSortX(figure)
      
      figure <- FigureLimitY(figure)
      
      figure <- FigureCoord(figure)
      
      figure <- FigureFacet(figure)
      
      figure <- figure + 
        theme(plot.background = 
                element_rect(fill = "white", colour = "grey90", size = 1)) +
        # theme(plot.margin = unit(c(0.6, 1, 0.6, 1), "cm")) +
        theme(panel.background = element_blank()) +
        theme(panel.grid.major = 
                element_line(colour="grey", size=0.2, linetype = "dashed") ) +
        theme(panel.grid.major.y  = 
                element_line(colour="grey", size=0.2, linetype = "dashed") ) +
        theme(panel.grid.major.x  = 
                element_line(colour="grey", size=0.2, linetype = "dashed") ) +
        theme(legend.background = element_blank()) +
        # theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
        theme(text = element_text(family = "wqy-microhei"))
      
      # figure <- figure + geom_text_repel()
      
      return(figure)
      
    },
    
        
    PlotDummy = function(errorinfo) {
      
      plot.data <<- data.frame()
      plot.data[1, c(kColumnAxisX, kColumnAxisY, kColumnLabel, kColumnFill)] <<-
        list(kColumnAxisX = c(100), 
             kColumnAxisY = c(100), 
             kColumnLabel = c("Dummy"), 
             kColumnFill = c("Dummy"))
      figure <- ggplot(data = plot.data, 
                       aes(x = plot_x, y = plot_y, label = plot_label))
      
      figure <- figure + 
        geom_point(stat="identity", 
                   colour="white", 
                   shape=21, size = 5,
                   aes(alpha = 1, 
                       fill = factor(plot_fill)))
      
      figure <- figure + 
        labs(title = paste(errorinfo, 
                           "ERROR! Print dummy figure as placeholder!"))
      
      figure <- FigureTheme(figure, "npg")
      
      
      return(figure)
      
    },
    
    PlotFigure = function(plot.dir, plot.code) {
      PreparePlotData(plot.code)
      
      plot.code.name <- gsub("\\.+", "_", plot.code)
      # plot.code.name <- gsub("\\.+", "_", plot.param[1, kColumnPlotCode])
      # plot.code.name <- plot.param[1, kColumnPlotCode]
      plot.file <- paste0(plot.dir, plot.code.name,".pdf")
      plot.fig <- list()
      plot.fig$path <- paste0(getwd(), "/", plot.file)
      
      if (is.na(plot.param[1, kColumnPlotGeom])) {
        LogError(paste(plot.code, "canvas not available!"))
        #plot.fig$name <- plot.code.name
        plot.fig$name <- paste("绘图参数缺失", plot.code)
        command <- paste0("cp ", config$GetDirReportOut(),
                          config$GetDirFigure(),
                          "plot_point_dummy.pdf ", 
                          plot.fig$path)
        system(command = command)
      } else if (nrow(plot.data) == 0) {
        LogError(paste(plot.code, "data not available!"))
        plot.fig$name <- plot.param[1, kColumnPlotTitle]
        command <- paste0("cp ", config$GetDirReportOut(),
                          config$GetDirFigure(),
                          "plot_point_dummy.pdf ", 
                          plot.fig$path)
        system(command = command)
      } else {
        LogInfo(paste(plot.param[1, kColumnPlotGeom], plot.code))
        plot.fig$name <- plot.param[1, kColumnPlotTitle]
        pdf(plot.file, width = as.numeric(plot.param[1, kColumnPlotWidth]) , 
            height = as.numeric(plot.param[1, kColumnPlotHeight]) ) 
        # CairoPNG("aaaa.png", width = as.numeric(plot.param[1, kColumnPlotWidth]), 
        #    height = as.numeric(plot.param[1, kColumnPlotHeight]), units = "in", res = 150) 
        # CairoPNG("aaaa.png", 1000, 550, res = 300);
        
        showtext_begin()
        
        if (plot.param[1, kColumnPlotGeom] == kPlotGeomBarDodge) {
          figure <- PlotGeomBarDodge()
        } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomBarStack) {
          figure <- PlotGeomBarDodge()
        } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomScatter) {
          figure <- PlotGeomScatter()
        } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomBox) {
          figure <- PlotGeomBoxWhisker()
        } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomWindRose) {
          figure <- PlotGeomWindRose()
        }
        
        print(figure)
        # ggsave(figure,filename = "p.png",width = 12,height = 9)
        showtext_end()
        dev.off()
        
        # im.convert(plot.file, output = "bm.png", extra.opts="-density 720")
        
        
      }
      
      
      
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
