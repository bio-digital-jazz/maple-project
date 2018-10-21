###Cycle8####
#1_BRS_1a#
---
title: "1_BRS_1a"
output: html_document
---
  
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(extrafont)
library(stringr)
library(ggpubr)
library(Cairo)


######SET CORRECT WD###########
setwd("G:/LHD/LHD_1-14_BRS Charts - Cycle 8")
options(scipen = 999)  ###no scientific notation in charts
data2 <- read.csv("Breast_NSW_Bipart_all_LGA.csv") # reading the data file
unique_lhds <- data2 %>%  ###creating a data frame with unique LHD/PHN names
  distinct(LHD.Name)

LHDs <- unique_lhds$LHD.Name[unique_lhds$LHD.Name!= "NSW"] ###creating a vector with unique LHD/PHN names

for (Highlight in LHDs) { #for loop open
  i = 1 #set the number of the chart here as in the footnotes files
  x <- read.csv("G:/Footnotes/Breast RBCO footnotes 2018.csv") #reading the footnotes files
  table1 <- read.csv("Breast_NSW_Bipart_all_LGA.csv") # reading the data file
  sno <- "01"   #for the file name
  program <- "BRS" #CVS, BWS, BRS, HLP, SKP
  type <- "1a" #Type of chart
  pdf_name <- paste0(Highlight,"_", sno,"_",program,"_", type,"_","Cycle8.pdf") #for the file name
  pdf_title <- paste0(x$Chart...Table.title[x$Chart..Table.ID ==i][1],", ", x$LHD[x$Chart..Table.ID ==i][1],", ", x$Year[x$Chart..Table.ID ==i][1]) #PDF title
  pdf_title <- str_replace_all(string = pdf_title, pattern = "-", repl ="\u2013") #adding the en dash to titles
  pdf_title <- sapply(lapply(pdf_title, strwrap, width=75), paste, collapse="\n") #text wrapping the title
  
  #for footnotes
  foot_dataset <- subset(x, x$Chart..Table.ID ==i) #subsetting the footnotes for chart 1
  foot_dataset2 <- data.frame(foot_dataset$Symbol...Index,"  ", foot_dataset$Footnotes, "\n\n") #converting the footnotes into a dataframe
  foot_dataset$Footnotes
  new2 <- lapply(foot_dataset$Footnotes, str_replace_all, pattern = "-", replacement ="\u2013")
  new <- sapply(lapply(new2, strwrap, width=118), paste, collapse="\n")
  new <- data.frame(new)
  new1 <- data.frame(foot_dataset2$foot_dataset.Symbol...Index, new)
  new1$foot_dataset2.foot_dataset.Symbol...Index <- as.character(new1$foot_dataset2.foot_dataset.Symbol...Index)
  for (i in 1:nrow(new1)){
    if(grepl("[0-9]",new1[i,1])){
      new1[i,1] <- paste0(new1[i,1],".")    #add '.' to the footnote id
    }
  }
  tt <- ttheme_minimal(core = list(fg_params = list(fontsize = 7, fontfamily = "verdana", just = c("top", "left"), hjust = 0, x = 0.01, y = 1, vjust = 1, col = "#58595B", lineheight = 1)), padding = unit(c(0,1.5),"mm")) #defining the theme for the footnotes table
  g2 <- tableGrob(new1[1:nrow(new1), 1:ncol(new1)], rows = NULL, cols = NULL, theme = tt)
  g2$widths <- unit(c(5,165), c("mm", "mm"))
  g2$layout$clip <- "off"
  grid.newpage()
  grid.draw(g2)
  #####for footnotes#####
  
  #####the plot#####
  
  table1 <- subset(table1, as.character(LHD.Name) == as.character(LGA.Name)) 
  table1$Women.Population <- format(round(as.numeric(table1$Women.Population), digits = 0), big.mark = ",")
  table1$Rate <- as.numeric(table1$Rate)
  table1$Rate <- round(table1$Rate, digits = 3)
  table1$Women.Population[table1$LHD.Name=="NSW" & table1$Year == "2016-2017"]
  table1$Women.Population[table1$LHD.Name=="NSW" & table1$Year == "2015-2016"]
  
  labels <- table1 %>%
    filter(Year %in% c("2016-2017")) 
  
  x <- as.character(labels$Women.Population)
  y <- as.character(labels$LHD.Name)
  x <- str_replace_all(string=x, pattern=" ", repl="")
  
  labels$labels <- paste(y, '\n', "(N=", x ,")", sep = "")
  labels <- labels[,c(1,9)]
  
  table1 <- merge(table1, labels, by = "LHD.Name") 

  table1 <- arrange(table1, desc(table1$Rate))
  
  table1$Year <- factor(table1$Year)
  
  CurrYear <- "2016-2017"
  PrevYear <- "2015-2016"
  
  CurrYearData <- table1 %>%
    filter(Year %in% CurrYear)%>%
    arrange(desc(Rate)) %>%
    filter(LHD.Name != "NSW") %>%
    mutate(Rate2 = row_number())
  
  PrevYearData <- table1 %>%
    filter(Year %in% PrevYear)%>%
    arrange(desc(Rate)) %>%
    filter(LHD.Name != "NSW")
  
  #New code added to create new variable Rate2 for ordering the bars in geom_bar 
  PrevYearData <- merge(PrevYearData, CurrYearData[, c("LHD.Name","Rate2")], by = "LHD.Name")
  
  #being used later in the code to specify the vale for y intercept in geom_hline(aes(yintercept = yy,color="NSW"), size = 1) 
  NSWLineCurrYear <- table1$Rate[table1$LHD.Name=="NSW" & table1$Year == CurrYear]
  NSWLinePrevYear <- table1$Rate[table1$LHD.Name=="NSW" & table1$Year == PrevYear]
  
  BarCols <- c(CurrYear = "#66CCF0", PrevYear = "#B3B3B3" , Highlight = "#005AAD")
  LineCcols <- c("NSWLineCurrYear" = "black", "NSWLinePrevYear" = "#999999", "ErrorBars" = "black")
  
  CurrYearLabel <- str_replace_all(string=paste0(CurrYear, "  "), pattern="-", repl="\u2013")
  PrevYearLabel <- str_replace_all(string=paste0(PrevYear, "  "), pattern="-", repl="\u2013")
  NSWLabelCurrYear <- str_replace_all(string= paste0("NSW ", CurrYear,"\n", "(", NSWLineCurrYear,"%", ", N=",table1$Women.Population[table1$LHD.Name =="NSW" & table1$Year ==CurrYear], ")"), pattern="-", repl="\u2013")
  NSWLabelPrevYear <- str_replace_all(string= paste0("NSW ", PrevYear,"\n", "(", NSWLinePrevYear,"%", ", N=",table1$Women.Population[table1$LHD.Name =="NSW" & table1$Year ==PrevYear], ")"), pattern="-", repl="\u2013")
  
  g1 <- ggplot() +
    #data was changed on this line from table2.1 to table2.1.1
    geom_bar(data = PrevYearData, aes(x = reorder(LHD.Name, -Rate2), y = Rate, fill = "PrevYear"), stat = "identity") +
    geom_bar(data = CurrYearData, width = 0.6, aes(x = factor(LHD.Name), y = Rate, fill = "CurrYear"), stat = "identity") +
    geom_bar(data=subset(CurrYearData, LHD.Name == Highlight), aes(x = LHD.Name, y = Rate, fill = "Highlight"), width = 0.6, stat = "identity", alpha = 0.8) +
    geom_text(data = CurrYearData, aes(x = factor(LHD.Name), label = format(Rate, digits = 3), y = 4), family = "Verdana", color = "white", size = (2.47), fontface = "bold") +
    geom_errorbar(data = CurrYearData, aes(x = LHD.Name, ymin = Lower.CI, ymax=Upper.CI, colour = "ErrorBars"), width = 0, size = 0.469177215) +
    geom_segment(aes(y = NSWLineCurrYear, yend = NSWLineCurrYear, x = CurrYearData$LHD.Name, xend = (nrow(CurrYearData) - 0.5 ), colour = "NSWLineCurrYear"), size = 1.0633)  +
    geom_segment(aes(y = NSWLinePrevYear, yend = NSWLinePrevYear, x = CurrYearData$LHD.Name, xend = (nrow(CurrYearData) - 0.5 ), colour = "NSWLinePrevYear"), size = 1.0633)  +
    xlab("Local Health District") +
    ylab("Participation rate (%)") +
    geom_point(data = CurrYearData, aes(x = LHD.Name, y = Lower.CI),shape=23,fill="black") +
    geom_point(data = CurrYearData, aes(x = LHD.Name, y = Upper.CI),shape=23,fill="black") +
    theme_bw() +
    coord_flip()+
    theme(text = element_text(family = "Verdana"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          strip.background  = element_blank(),
          axis.title.x  = element_text(colour = "#58595B", size = 8),
          axis.title.y = element_blank(),
          axis.line = element_line(colour = "#58595B", size = .176),
          axis.ticks = element_line(colour = "#58595B", size = .176),
          axis.text  = element_text(size = 7, colour = "#58595B", lineheight = 1.1),
          axis.ticks.length =unit(1.25, "mm"),
          legend.position = c(0.5, -0.175),
          legend.text = element_text(colour = "#58595B", size = 7, lineheight = 1),
          legend.title=element_blank(),
          legend.direction = "horizontal",
          legend.justification = "bottom",
          legend.spacing.y = unit(-1, "mm"),
          legend.spacing.x = unit(1, "mm"),
          legend.key.size = unit(6, "mm"),
          legend.background = element_rect(fill = "NA", colour = "NA" )) +
    scale_y_continuous(breaks = seq(0, 100, 10), limits =c(0, 100), expand = c(0,0)) +
    scale_x_discrete(breaks = CurrYearData$LHD.Name, labels = CurrYearData$labels) +
    guides(fill=guide_legend(order = 1, label.hjust = 0.2), colour = guide_legend(override.aes = list(size = 0.469177215))) +
    scale_fill_manual(values = BarCols, breaks = c("CurrYear", "PrevYear", "Highlight"), 
                      labels = c(CurrYearLabel, PrevYearLabel, Highlight)) +
    scale_colour_manual(values = LineCcols, breaks= c("NSWLineCurrYear", "NSWLinePrevYear", "ErrorBars"), 
                       labels = c(NSWLabelCurrYear, NSWLabelPrevYear, "Confidence interval" ))
  g1
  
  #####the plot#####
  
  #####grobs for grid changes#####
  p2 = ggplotGrob(g1)
  # Get names of axis-l- grobs
  names_list <- grid.ls(grid.force(p2))$name
  names_list2 <- as.data.frame(names_list)
  names_list2 <- names_list2 %>%
    filter(str_detect(names_list,"-bg."))
  #####grobs for grid changes#####
  
  grid.newpage()
  cairo_pdf(pdf_name, height = 11.6929, width = 8.26772,  family = "Verdana")
  top.vp <- viewport(layout=grid.layout(7,3,
                                        widths=unit(c(20, 170, 20), c("mm", "mm", "mm")),
                                        heights=unit(c(20, 10, 4, 170, 10, 59, 20), c("mm", "mm", "mm", "mm", "mm","mm", "mm"))))  
  
  top_margin <- viewport(layout.pos.row=1, layout.pos.col=2, name="top_margin")                         	#row 1, column 2 Top Margin
  title <- viewport(layout.pos.row=2, layout.pos.col=2, name="title")                                   	#row 2, column 2 Plot title
  padding <- viewport(layout.pos.row=3, layout.pos.col = 2,name="padding")                                #row 3, column 2 padding bw title and plot
  plot <- viewport(layout.pos.row=4, layout.pos.col = 2,name="plot")                                    	#row 4, column 2 Actual Plot/graph
  legend <- viewport(layout.pos.row=5, layout.pos.col = 2,name="legend")                                  #row 5, column 2 Actual Plot/graph
  footnotes <- viewport(layout.pos.row=6, layout.pos.col=2, name="footnotes")                             #row 6, column 2 Bottom section for Footnotes
  bottom_margin <- viewport(layout.pos.row=7, layout.pos.col=2, name="bottom_margin")                   	#row 7, column 2 Bottom Margin
  
  #Define viewport tree by adding above viewport and then push these viewports to the grid using pushViewport command
  splot <- vpTree(top.vp, vpList(top_margin,title, padding, plot, legend, footnotes,bottom_margin)) # Defining the hierarchy of the viewports
  pushViewport(splot) # Creating viewports for plotting with the definitions of splot
  
  upViewport()
  downViewport("top_margin")
  #grid.text("22top margin 20 mm",gp = gpar(col="grey"))
  
  upViewport()
  downViewport("title")
  grid.text(pdf_title,x=unit(1,"mm"),y=unit(1,"npc") - unit(1,"mm"),just = c("left","bottom"), vjust = 1, gp = gpar(col="#8C0080", fontsize=11, fontface = "bold", fontfamily = "verdana", lineheight = 1))  
  
  upViewport()
  downViewport("padding")

  upViewport()
  downViewport("plot")
  print(g1,newpage = FALSE, vjust = 1, just = c("left","bottom"))
  downViewport("guides.4-2-4-2")
  downViewport("key-1-3-bg.2-4-2-4")
  grid.rect(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
            width = unit(1, "npc"), height = unit(1, "npc"),
            just = "centre", hjust = NULL, vjust = NULL,
            default.units = "npc", name = NULL,
            gp=gpar(col="NA",fill="white"), draw = TRUE, vp = viewport(angle = 90))
  
  grid.rect(x = unit(0, "npc"), y = unit(0.5, "npc"),
            width = unit(6, "mm"), height = unit(0.8, "mm"),
            just = c("left","top"),gp=gpar(col="NA", fill="black"), vp = viewport(angle = 90))
  
  upViewport()
  downViewport("key-1-7-bg.2-8-2-8")
  grid.rect(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
            width = unit(1, "npc"), height = unit(1, "npc"),
            just = "centre", hjust = NULL, vjust = NULL,
            default.units = "npc", name = NULL,
            gp=gpar(col="NA",fill="white"), draw = TRUE, vp = viewport(angle = 90))
  
  grid.rect(x = unit(0, "npc"), y = unit(0.5, "npc"),
            width = unit(6, "mm"), height = unit(0.8, "mm"),
            just = c("left","top"),gp=gpar(col="NA", fill="#B3B3B3"), vp = viewport(angle = 90))
  
  upViewport(0)
  downViewport("legend")
  
  upViewport(0)
  downViewport("footnotes")
  grid.rect(x=seq(0, 1, length = 131), y=0.86, width = unit(0.65, "mm"), height = unit(0.65, "mm"), default.units="npc", name=NULL,gp=gpar(fill = "#E2E3E4", col = "NA"), draw=TRUE, vp=NULL)
  grid.draw(g2)
  
  upViewport()
  downViewport("bottom_margin")
  
  dev.off()
}
