## PlotFigure class for green index data processing and report generation


## flag for source loaded checking
plotfigure.loaded <- TRUE

## define 
library(methods)
library(ggplot2)
library(showtext)
library(ggthemes)  # have been fork and customized
# library("ggthemes", lib="~/local/R_libs/"，) 
library(ggsci)
library(dplyr)
library(tibble)
library(ggrepel)
# library(Cairo)
# library(animation)
library("reshape2")
library(digest)

source(paste0(gi.dir.script,"ggradar2.R"))

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
      
      if (plot.param[1, kColumnFill] == kColumnValue) {
        plot.data[, kColumnFill] <<- ""
        plot.data[, kColumnFill] <<- as.numeric(plot.data[, kColumnFill])
      } else {
        fill <- unlist(strsplit(plot.param[1, kColumnFill], kSeparator))
        plot.data[, kColumnFill] <<- ""
        for (i in 1:length(fill)) {
          plot.data[, kColumnFill] <<- paste0(plot.data[, kColumnFill], 
                                              plot.data[, fill[i]])
        }
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
        
        if (as.integer(plot.param[1, kColumnFacetRow]) >= 1) {
          figure <- figure + 
            facet_wrap(facets = ~plot_facet, 
                       nrow = as.integer(plot.param[1, kColumnFacetRow]),
                       strip.position = plot.param[1, kColumnFacetPosition],
                       scales = plot.param[1, kColumnFacetScale])
        } else {
          figure <- figure + 
            facet_wrap(facets = ~plot_facet, 
                       ncol = as.integer(plot.param[1, kColumnFacetCol]),
                       strip.position = plot.param[1, kColumnFacetPosition],
                       scales = plot.param[1, kColumnFacetScale])
        }
        
      }
      
      figure <- figure + 
        theme(strip.text = 
                element_text(size = as.numeric(plot.param[1, kColumnFacetStripSize]),
                             colour = plot.param[1, kColumnFacetStripColour],
                             # angle = 0))
                             angle = as.numeric(plot.param[1, kColumnFacetStripAngle])))
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
      } else if (theme.name == "light-blue") {
        figure <- figure + theme_economist() + scale_fill_material("light-blue")
      } else if (theme.name == "gdocs") {
        figure <- figure + theme_economist() + 
          scale_color_gdocs() + scale_fill_gdocs()
      } else if (theme.name == "ptol") {
        figure <- figure + theme_economist() + 
          scale_color_ptol() + scale_fill_ptol()
      } else if (theme.name == "hc") {
        figure <- figure + theme_economist() + 
          scale_color_hc() + scale_fill_hc()
      } else if (theme.name == "colorblind") {
        figure <- figure + theme_economist() + 
          scale_color_colorblind() + scale_fill_colorblind()
      } else if (theme.name == "tableau") {
        figure <- figure + theme_economist() + scale_shape_tableau()
      } else if (theme.name == "calc") {
        figure <- figure + theme_economist() + scale_color_calc() + scale_fill_calc()
      } else if (theme.name == "excel") {
        figure <- figure + theme_economist() + scale_color_excel_new() + scale_fill_excel_new()
      } else if (theme.name == "myeconomist") {
        figure <- figure + theme_economist() + scale_fill_myeconomist()
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
      
      # figure <- figure + labs(title = plot.param[1, kColumnPlotTitle])
      figure <- figure + labs(x = plot.label.x, y = plot.label.y)
      
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
          
          if (plot.param[1, kColumnOrderXOption] == kStringNone) {
            plot.order.x <<- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
          } else {
            plot.order.x.option <- unlist(strsplit(plot.param[1, kColumnOrderXOption], kSeparator))
            x.unique <- unlist(unique(plot.data[, plot.param[1, kColumnAxisX]]))
            plot.order.x <<- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
            for (i in 1:length(plot.order.x.option)) {
              option.value <- plot.order.x.option[i]
              if (! option.value %in% x.unique) {
                plot.order.x <<- plot.order.x[plot.order.x != option.value]
              }
            }
          }
          
          # plot.order.x <<- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        }
      } else {
        sort.df <- plot.data
        
        if (plot.param[1, kColumnSortXFilter] != kStringNone) {
          plot.order.x.filter <- 
            unlist(strsplit(plot.param[1, kColumnSortXFilter], kSeparator))
          sort.df <-
            sort.df[sort.df[, plot.order.x.filter[1]]
                    == plot.order.x.filter[2], ]
        }
        
        if (plot.param[1, kColumnSortX] == kSortAscAll) {
          
          subsets <- unlist(strsplit(plot.param[1, kColumnSortXSubset], kSeparator))
          xlist <- unlist(unique(sort.df[, kColumnAxisX]))
          order.first <- TRUE
          for (i in 1:length(subsets)) {
            sort.df <- 
              sort.df[sort.df[, plot.param[1, kColumnSortXBy]] == 
                        subsets[i], 
                      c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
            
            sort.df <- 
              arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
            
            if (order.first){
              plot.order.x <<- unlist(sort.df[, kColumnAxisX])
              order.first <- FALSE
            } else {
              plot.order.x <<- c(plot.order.x ,unlist(sort.df[, kColumnAxisX]))
            }
            
            if (length(plot.order.x) == length(xlist)) {
              break
            } else {
              sort.df <- sort.df[!(sort.df[, kColumnAxisX] %in% plot.order.x), ]
            }
          }
          
          plot.order.x <<- rev(plot.order.x)
        } else if(plot.param[1, kColumnSortX] == kSortDescAll) {
          
          subsets <- unlist(strsplit(plot.param[1, kColumnSortXSubset], kSeparator))
          xlist <- unlist(unique(sort.df[, kColumnAxisX]))
          order.first  <- TRUE
          for (i in 1:length(subsets)) {
            sort.df <- 
              sort.df[sort.df[, plot.param[1, kColumnSortXBy]] == 
                        subsets[i], 
                      c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
            
            sort.df <- 
              arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
            
            if (order.first){
              plot.order.x <<- unlist(sort.df[, kColumnAxisX])
              order.first <- FALSE
            } else {
              plot.order.x <<- c(plot.order.x ,unlist(sort.df[, kColumnAxisX]))
            }
            
            if (length(plot.order.x) == length(xlist)) {
              break
            } else {
              sort.df <- sort.df[!(sort.df[, kColumnAxisX] %in% plot.order.x), ]
            }
          }
          print(plot.order.x)
          
        } else if(plot.param[1, kColumnSortX] == kSortAsc) {
          
          order <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
          order <- order[1:length(order)-1]
          sort.df <- sort.df[!(sort.df[, kColumnAxisX] %in% order), ]
          
          subsets <- unlist(strsplit(plot.param[1, kColumnSortXSubset], kSeparator))
          xlist <- unlist(unique(sort.df[, kColumnAxisX]))
          order.first  <- TRUE
          for (i in 1:length(subsets)) {
            sort.df <- 
              sort.df[sort.df[, plot.param[1, kColumnSortXBy]] == 
                        subsets[i], 
                      c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
            
            sort.df <- 
              arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
            
            if (order.first){
              order.x <- unlist(sort.df[, kColumnAxisX])
              order.first <- FALSE
            } else {
              order.x <- c(order.x ,unlist(sort.df[, kColumnAxisX]))
            }
            
            if (length(order.x) == length(xlist)) {
              break
            } else {
              sort.df <- sort.df[!(sort.df[, kColumnAxisX] %in% order.x), ]
            }
          }
          order.x <- rev(order.x)
          plot.order.x <<- unlist(append(order, order.x))
          
        } else if(plot.param[1, kColumnSortX] == kSortDesc) {
          
          order <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
          order <- order[1:length(order)-1]
          sort.df <- sort.df[!(sort.df[, kColumnAxisX] %in% order), ]
          
          subsets <- unlist(strsplit(plot.param[1, kColumnSortXSubset], kSeparator))
          xlist <- unlist(unique(sort.df[, kColumnAxisX]))
          order.first  <- TRUE
          for (i in 1:length(subsets)) {
            sort.df <- 
              sort.df[sort.df[, plot.param[1, kColumnSortXBy]] == 
                        subsets[i], 
                      c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
            
            sort.df <- 
              arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
            
            if (order.first){
              order.x <- unlist(sort.df[, kColumnAxisX])
              order.first <- FALSE
            } else {
              order.x <- c(order.x ,unlist(sort.df[, kColumnAxisX]))
            }
            
            if (length(order.x) == length(xlist)) {
              break
            } else {
              sort.df <- sort.df[!(sort.df[, kColumnAxisX] %in% order.x), ]
            }
          }
          plot.order.x <<- unlist(append(order, order.x))
        } 
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
      
      if (plot.param[1, kColumnLegendPosition] == "top"){
        direction <- "horizontal"
      } else if (plot.param[1, kColumnLegendPosition] == "right"){
        direction <- "vertical"
      } else if (plot.param[1, kColumnLegendPosition] == kStringNone){
        direction <- "vertical"
      }
      figure <- figure +  
        theme(legend.position = plot.param[1, kColumnLegendPosition],
              legend.direction =  direction,
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
      
      plot.data["msg"] <<- paste(plot.data[, kColumnAssessment],
        plot.data[, kColumnGrade], plot.data[, kColumnSubject],
        plot.data[, kColumnDomain], plot.data[, kColumnDimention],
        plot.data[, kColumnGroup], plot.data[, kColumnAttribute],
        plot.data[, kColumnTopic], plot.data[, kColumnStatisticsTier],
        plot.data[, kColumnStatisticsScope], plot.data[, kColumnCity],
        plot.data[, kColumnDistrict], plot.data[, kColumnSchool],
        plot.data[, kColumnStatisticsPerspective],
        plot.data[, kColumnStatisticsSample],
        plot.data[, kColumnAlias])
      plot.data[, "hash"] <<- ""
      for (i in 1:nrow(plot.data)) {
        plot.data[i, "hash"] <<- digest(plot.data[i, "msg"], algo = "sha256")
      }
       
      plot.merge <- plot.data[plot.data[, kColumnKey] == kMean, ]
      names(plot.merge) <- sub(paste0("^", kColumnAxisY, "$"), 
                            kMean, names(plot.merge))
      
      plot.min <- plot.data[plot.data[, kColumnKey] == kMin, 
                            c("hash", kColumnValue)]
      names(plot.min) <- sub(paste0("^", kColumnValue, "$"), 
                            kMin, names(plot.min))
      plot.merge <- merge(plot.merge, plot.min, by = "hash", all.x = TRUE)
      
      plot.max <- plot.data[plot.data[, kColumnKey] == kMax, 
                            c("hash", kColumnValue)]
      names(plot.max) <- sub(paste0("^", kColumnValue, "$"), 
                             kMax, names(plot.max))
      plot.merge <- merge(plot.merge, plot.max, by = "hash", all.x = TRUE)
      
      plot.median <- plot.data[plot.data[, kColumnKey] == kMedian, 
                            c("hash", kColumnValue)]
      names(plot.median) <- sub(paste0("^", kColumnValue, "$"), 
                             kMedian, names(plot.median))
      plot.merge <- merge(plot.merge, plot.median, by = "hash", all.x = TRUE)
      
      plot.upper <- plot.data[plot.data[, kColumnKey] == kUpper, 
                            c("hash", kColumnValue)]
      names(plot.upper) <- sub(paste0("^", kColumnValue, "$"), 
                             kUpper, names(plot.upper))
      plot.merge <- merge(plot.merge, plot.upper, by = "hash", all.x = TRUE)
      
      plot.lower <- plot.data[plot.data[, kColumnKey] == kLower, 
                              c("hash", kColumnValue)]
      names(plot.lower) <- sub(paste0("^", kColumnValue, "$"), 
                               kLower, names(plot.lower))
      plot.merge <- merge(plot.merge, plot.lower, by = "hash", all.x = TRUE)
      
      plot.merge[, kColumnValue] <- plot.merge[, kMedian]
      # plot.data <<- plot.merge
      
      figure <- ggplot(
        data = plot.merge, 
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
      
      figure <- FigureFacet(figure)
      
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
    
    PlotGeomRadar = function() {
      
      CalculateGroupPath <- function(df) {
        #Converts variable values into a set of radial x-y coordinates
        #Code adapted from a solution posted by Tony M to
        #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
        #Args:
        #  df: Col 1 -  group ('unique' cluster / group ID of entity)
        #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
        
        path <- df[,1]
        path <- factor(path,levels = as.vector(path))
        
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
      
      funcCircleCoords <- function(center = c(0,0), r.min = 1, r.max = 10, r.step = 1, npoints = 360){
        #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
        
        coord.data <- data.frame()
        r <- r.min + r.step
        while (r <= r.max){
          tt <- seq(0,2*pi,length.out = npoints)
          xx <- center[1] + (r-r.min) * cos(tt)
          yy <- center[2] + (r-r.min) * sin(tt)
          gg <- r + tt - tt
          coord.data <- rbind(coord.data, data.frame(group = gg, x = xx, y = yy))
          r <- r + r.step
        }
        
        return(coord.data)
      }
      
      CaclulateAxisPath = function(n.vars = 10, min = 0, max = 10) {
        #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
        #Args:
        #var.names - list of variables to be plotted on radar plot
        #min - MININUM value required for the plotted axes (same value will be applied to all axes)
        #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
        #var.names <- c("v1","v2","v3","v4","v5")
        
        # n.vars <- length(var.names) # number of vars (axes) required
        
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
      
      CalculateAxisLabel = function(label.text, r.min, r.max, r.offset ) {
        axis.label <- data.frame(
          text=plot.x,
          x=NA,
          y=NA )
        #print(axis$label)
        #axis label coordinates
        n.vars <- length(axis.label$text)
        angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
        axis.label$x <- sapply(1:n.vars, function(i, x) {(r.max-r.min+r.offset)*sin(angles[i])})
        axis.label$y <- sapply(1:n.vars, function(i, x) {(r.max-r.min+r.offset)*cos(angles[i])}) 
        axis.label
      }
      
      
      if (plot.param[1, kColumnFillOrder] == kStringNone){
        
      } else {
        fill.order <- unlist(strsplit(plot.param[1, kColumnFillOrder], 
                                      kSeparator))
        plot.data[, kColumnFill] <<- factor(plot.data[, kColumnFill],
                                            levels = fill.order)
        
      }
      
      # figure <- FigurePlot()
      
      # figure <- figure + 
      #   geom_point( shape = 20, size = 1, alpha = 0.5) 
      
      radar.data <- data.frame()
      plot.group <- unlist(strsplit(plot.param[1, kColumnFillOrder], kSeparator))
      group.column <- plot.param[1, kColumnFill]
      plot.x <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
      
      for (i in 1:length(plot.group)) {
        group.name <- plot.group[i]
        df <- plot.data[plot.data[, group.column] == group.name, ]
        dl <- list()
        dl["group"] <- group.name
        for (j in 1:length(plot.x)) {
          x.name <- plot.x[j]
          dl[x.name] <- df[df[, kColumnAxisX] == x.name, kColumnAxisY]
        }
        radar.data <- rbind(radar.data, data.frame(dl))
      }
      
      path.data <- CalculateGroupPath(radar.data)
      # (path.data)
      
      scale.y <- unlist(strsplit(plot.param[1, kColumnLimitY], kSeparator))
      scale.y.min <- as.numeric(scale.y[1])
      scale.y.max <- as.numeric(scale.y[2])
      scale.y.step <- as.numeric(scale.y[3])
      scale.y.labeloffset <- as.numeric(scale.y[4])
      scale.y.labelborder <- as.numeric(scale.y[5])
    
      coord.data <- funcCircleCoords(r.min = scale.y.min, 
                                     r.max = scale.y.max, 
                                     r.step = scale.y.step)
      
      axis.data <- CaclulateAxisPath(n.vars = length(plot.x), 
                                     min = scale.y.min, 
                                     max = scale.y.max + scale.y.labeloffset)
      
      axis.label <- data.frame(
        text=plot.x,
        x=NA,
        y=NA )
      #print(axis$label)
      #axis label coordinates
      n.vars <- length(plot.x)
      angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
      axis.label$x <- sapply(1:n.vars, function(i, x) {((10+abs(1/9))*1.15)*sin(angles[i])})
      axis.label$y <- sapply(1:n.vars, function(i, x) {((10+abs(1/9))*1.15)*cos(angles[i])})
      
      axis.label <- CalculateAxisLabel(label.text = plot.x, 
                                       r.min = scale.y.min, 
                                       r.max = scale.y.max, 
                                       r.offset = scale.y.labeloffset )
        
      figure <- ggplot2::ggplot(path.data) + xlab(NULL) + ylab(NULL) + coord_equal()
      
     
      # ... + group (grid) 'paths'
      figure <- figure + geom_path(data=coord.data, aes(x=x,y=y, group=group),
                                   lty="dashed", colour="grey",
                                   size=0.5)
      
      # ... + group (grid) 'paths'
      figure <- figure + geom_path(data=axis.data, aes(x=x,y=y, group=axis.no),
                                   lty="solid", colour="grey",
                                   size=0.5)
      
      
      # + axis labels for any vertical axes [abs(x)<=x.centre.range]
      figure <- figure + geom_text(data=subset(axis.label,axis.label$x <= 0),
                               aes(x=x,y=y,label=text),size=4,hjust=0.5)
      # + axis labels for any vertical axes [x>x.centre.range]
      figure <- figure + geom_text(data=subset(axis.label,axis.label$x>0),
                               aes(x=x,y=y,label=text),size=4,hjust=0.5)
      
      canvas.data <- data.frame(
        group = c("left","left","right","right"),
        x = c(-scale.y.labelborder, -scale.y.labelborder,
            scale.y.labelborder, scale.y.labelborder),
        y = c(scale.y.max, -scale.y.max, scale.y.max, -scale.y.max) )
      figure <- figure + geom_path(data=canvas.data, aes(x=x,y=y, group=group),
                                   lty="blank", colour="grey",
                                   size=0.5)
      
      # ... + group (cluster) 'paths'
      figure <- figure + geom_path(data=path.data,aes(x=x,y=y,group=group,colour=group),
                                   size=1, alpha = 0.8)
      
      # ... + group points (cluster data)
      figure <- figure + geom_point(data=path.data,aes(x=x,y=y,group=group,colour=group),size=2, alpha = 0.8)
      
      
      figure <- figure + theme_bw() +
        theme(axis.text.y=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              legend.key=element_rect(linetype="blank"))
      
      # figure <- figure + labs(title = plot.param[1, kColumnPlotTitle])
      
      figure <- figure +  
        theme(legend.position = plot.param[1, kColumnLegendPosition],
              legend.direction =  plot.param[1, kColumnLegendDirection],
              legend.text = 
                element_text(size = plot.param[1, kColumnLegendFontSize]),
              legend.title=element_blank())
      
      figure <- figure + 
        # theme(plot.background = element_blank()) +
        theme(plot.background = 
                element_rect(fill = "white", colour = "grey90", size = 1))
      
      #figure <- ggradar2(plot.data = radar.data,
      #                   webtype = "lux",
      #                   grid.label.size = 10,
      #                   gridline.label.offset = 0,
      #                   gridline.label = c("0", "2", "4", "6", "8", "10"))
      
      # figure <- FigureTheme(figure, plot.param[1, kColumnPlotTheme])
      
      # figure <- FigureLabelXY(figure)
      
      # figure <- FigureLegend(figure)
      
      # figure <- FigureSortX(figure)
      
      # figure <- FigureLimitY(figure)
      
      # figure <- FigureCoord(figure)
      
      # figure <- figure + geom_text_repel()
      
      figure <- figure + scale_color_economist()
      
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
      LogInfo(paste(plot.param[1, kColumnPlotGeom], plot.code))
      
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
                          "dummy-plot.pdf", 
                          " ",
                          plot.fig$path)
        system(command = command)
      } else if (nrow(plot.data) == 0) {
        LogError(paste(plot.code, "data not available!"))
        plot.fig$name <- plot.param[1, kColumnPlotTitle]
        command <- paste0("cp ", config$GetDirReportOut(),
                          config$GetDirFigure(),
                          "dummy-plot.pdf", 
                          " ",
                          plot.fig$path)
        system(command = command)
      } else {
        
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
        } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomRadar) {
          figure <- PlotGeomRadar()
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
      
    },
    
    AppendLostX = function(sorted.x, all.x) {
      l <- length(sorted.x) + 1
      out.x <- sorted.x
      for (i in 1:length(all.x)) {
        if(!(all.x[i] %in% sorted.x)) {
          out.x[l] <- all.x[i]
          l <- l + 1
        }
      }
      return(out.x)
    },
    
    MultiPlotSortX = function() {
      
      axisx.names <- unlist(unique(plot.data[, kColumnAxisX]))
      
      if (plot.param[1, kColumnSortX] == kStringNone) {
        if (plot.param[1, kColumnOrderX] == kStringNone) {
          plot.order.x <<- axisx.names
        } else {
          plot.order.x <<- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        }
        
      } else if(plot.param[1, kColumnSortX] == kSortAscAll) {
        
        sort.x.subset <- unlist(strsplit(plot.param[1, kColumnSortXSubset], 
                                         kSeparator))
        df <- plot.data
        for (i in 1:length(sort.x.subset)) {
          subset <- sort.x.subset[i]
          sort.df <- 
            df[df[, plot.param[1, kColumnSortXBy]] == subset, 
               c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
          sort.df <- 
            arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
          sorted.x <- unlist(sort.df[, kColumnAxisX])
          if (i == 1) {
            plot.order.x <<- sorted.x
          } else {
            plot.order.x <<- c(plot.order.x, sorted.x)
          }
          
          if (length(plot.order.x ) == length(axisx.names)) {
            break
          } else {
            df <- df[!(df[, kColumnAxisX] %in% plot.order.x), ]
          }
        }

        plot.order.x <<- rev(plot.order.x)
        
      } else if(plot.param[1, kColumnSortX] == kSortDescAll) {
        
        sort.x.subset <- unlist(strsplit(plot.param[1, kColumnSortXSubset], 
                                          kSeparator))
        df <- plot.data
        for (i in 1:length(sort.x.subset)) {
          subset <- sort.x.subset[i]
          sort.df <- 
            df[df[, plot.param[1, kColumnSortXBy]] == subset, 
                      c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
          sort.df <- 
            arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
          sorted.x <- unlist(sort.df[, kColumnAxisX])
          if (i == 1) {
            plot.order.x <<- sorted.x
          } else {
            plot.order.x <<- c(plot.order.x, sorted.x)
          }
          
          if (length(plot.order.x ) == length(axisx.names)) {
            break
          } else {
            df <- df[!(df[, kColumnAxisX] %in% plot.order.x), ]
          }
        }

      } else if(plot.param[1, kColumnSortX] == kSortAsc) {
        
        order <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        order <- order[1:length(order)-1]
        df <- plot.data[!(plot.data[, kColumnAxisX] %in% order), ]
        
        xlist <- unlist(unique(df[, kColumnAxisX]))
        
        sort.x.subset <- unlist(strsplit(plot.param[1, kColumnSortXSubset], 
                                         kSeparator))
        for (i in 1:length(sort.x.subset)) {
          subset <- sort.x.subset[i]
          sort.df <- 
            df[df[, plot.param[1, kColumnSortXBy]] == subset, 
               c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
          sort.df <- 
            arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
          sorted.x <- unlist(sort.df[, kColumnAxisX])
          print(sorted.x)
          if (i == 1) {
            plot.order.x <<- sorted.x
          } else {
            plot.order.x <<- c(plot.order.x, sorted.x)
          }
          
          if (length(plot.order.x ) == length(xlist)) {
            break
          } else {
            df <- df[!(df[, kColumnAxisX] %in% plot.order.x), ]
          }
        }
        
        plot.order.x <<- rev(plot.order.x)
        
        plot.order.x <<- unlist(append(order, plot.order.x))
        
      } else if(plot.param[1, kColumnSortX] == kSortDesc) {
        
        order <- unlist(strsplit(plot.param[1, kColumnOrderX], kSeparator))
        order <- order[1:length(order)-1]
        df <- plot.data[!(plot.data[, kColumnAxisX] %in% order), ]
        
        xlist <- unlist(unique(df[, kColumnAxisX]))
        
        sort.x.subset <- unlist(strsplit(plot.param[1, kColumnSortXSubset], 
                                          kSeparator))
        for (i in 1:length(sort.x.subset)) {
          subset <- sort.x.subset[i]
          sort.df <- 
            df[df[, plot.param[1, kColumnSortXBy]] == subset, 
               c(kColumnAxisX, plot.param[1, kColumnSortXValue])]
          sort.df <- 
            arrange(sort.df, desc(sort.df[, plot.param[1, kColumnSortXValue]]))
          sorted.x <- unlist(sort.df[, kColumnAxisX])
          
          if (i == 1) {
            plot.order.x <<- sorted.x
          } else {
            plot.order.x <<- c(plot.order.x, sorted.x)
          }
          
          if (length(plot.order.x ) == length(xlist)) {
            break
          } else {
            df <- df[!(df[, kColumnAxisX] %in% plot.order.x), ]
          }
        }
        
        plot.order.x <<- unlist(append(order, plot.order.x))
      } 
      
      return(plot.order.x)
    },
    
    MultiPlotFigure = function(plot.dir, plot.code) {
      PreparePlotData(plot.code)
      write.csv(plot.data, "plot.data.csv")
      plot.code.name <- gsub("\\.+", "_", plot.code)
      # plot.code.name <- gsub("\\.+", "_", plot.param[1, kColumnPlotCode])
      # plot.code.name <- plot.param[1, kColumnPlotCode]
      plot.file <- paste0(plot.dir, plot.code.name,".pdf")
      plot.fig <- list()
      plot.fig$path[1] <- paste0(getwd(), "/", plot.file)
      
      if (is.na(plot.param[1, kColumnPlotGeom])) {
        LogError(paste(plot.code, "canvas not available!"))
        #plot.fig$name <- plot.code.name
        plot.fig$name[1] <- paste("绘图参数缺失", plot.code)
        command <- paste0("cp ", config$GetDirReportOut(),
                          config$GetDirFigure(),
                          "plot_point_dummy.pdf ", 
                          plot.fig$path[1])
        system(command = command)
      } else if (nrow(plot.data) == 0) {
        LogError(paste(plot.code, "data not available!"))
        plot.fig$name[1] <- plot.param[1, kColumnPlotTitle]
        command <- paste0("cp ", config$GetDirReportOut(),
                          config$GetDirFigure(),
                          "plot_point_dummy.pdf ", 
                          plot.fig$path[1])
        system(command = command)
      } else {
        
        multiple.plot.data <- plot.data
        multiple.order.x <- MultiPlotSortX()
        plot.x.limit <- length(multiple.order.x)
        plot.x.max <- as.numeric(plot.param[1, kColumnPlotMultipleMax])
        plot.x.min <- as.numeric(plot.param[1, kColumnPlotMultipleMin])
        plot.multiple <- ceiling(plot.x.limit / plot.x.max)
        plot.x.size <- ceiling(plot.x.limit / plot.multiple)
        if (plot.x.size < plot.x.min && plot.x.min < plot.x.limit) {
          plot.x.size <- plot.x.min
          plot.multiple <- ceiling(plot.x.limit / plot.x.size)
        }
        # LogWarn(paste(as.character(plot.x.limit), as.character(plot.multiple),
        #               as.character(plot.x.size)))
        
        for (i in plot.x.limit:(plot.x.size * plot.multiple)) {
          if (i > plot.x.limit) {
            multiple.order.x[i] <- ""
            for (j in 1:(i-plot.x.limit)) {
              multiple.order.x[i] <- paste0(multiple.order.x[i], " ")
            }
            
          }
        }
        
        multiple.plot.title  <- plot.param[1, kColumnPlotTitle]
        
        for (i in 1:plot.multiple) {
          plot.code.name <- gsub("\\.+", "_", plot.code)
          if (i > 1) {
            plot.file <- paste0(plot.dir, plot.code.name, "_", 
                                as.character(i), ".pdf")
          }
          plot.fig$path[i] <- paste0(getwd(), "/", plot.file)
          
          start.x <- (i-1) * plot.x.size + 1
          end.x <- i * plot.x.size
          plot.param[1, kColumnSortX] <<- kStringNone
          plot.param[1, kColumnOrderX] <<- ""
          plot.data <<- data.frame()
          for (j in start.x:end.x) {
            if (j == start.x) {
              plot.param[1, kColumnOrderX] <<- multiple.order.x[j]
            } else {
              plot.param[1, kColumnOrderX] <<- 
                paste0(plot.param[1, kColumnOrderX], 
                       kSeparator, multiple.order.x[j])
            }
            plot.data <<- 
              rbind(plot.data, 
                    multiple.plot.data[
                      multiple.plot.data[, kColumnAxisX] == 
                        multiple.order.x[j], ])
          }
          
          if(i > 1){
            plot.param[1, kColumnPlotTitle] <<- 
              paste(multiple.plot.title, as.character(i))
          }
          plot.fig$name[i] <- plot.param[1, kColumnPlotTitle]
          LogInfo(paste(plot.param[1, kColumnPlotGeom], plot.fig$name[i]))
          
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
            figure <- PlotGeomBoxWhisker()
          } else if (plot.param[1, kColumnPlotGeom] == kPlotGeomWindRose) {
            figure <- PlotGeomWindRose()
          }
          
          print(figure)
          showtext_end()
          dev.off()
              
        }

      }
      return(plot.fig)
      
    },
    
    GetMultiPlotFigure = function(plot.dir, plot.code) {
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
