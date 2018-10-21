###Cycle7####
###24_BWS_1b_3###
#---
#title: "24_BWS_1b_3"
#output: html_document
#---

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(extrafont)
library(stringr)
library(ggpubr)
library(Cairo)




options(scipen = 999)
table2 <- read.csv("./Data/bowel_LGA_LHD.csv")
######SET CORRECT WD###########

#####for titles#####

# general pdf setup stuff
i = 3 #set the number of the chart here
Highlight <- "Hunter New England LHD" #specify the loop code here
#####for pdf name####
sno <- "24"
program <- "BWS" #CVS, BWS, BRS, HLP, SKP
type <- "1b" #Type of chart
pdf_name <- paste0(sno,"_", program,"_", i,"_", type,"_",Highlight,"_", "Cycle8.pdf")
#####for pdf name####


## footnote here
x <- read.csv("./Data/Bowel RBCO footnotes 2018.csv") #reading the footnotes files
pdf_title <- paste0(x$Chart...Table.title[x$Chart..Table.ID == i][1],", ", x$LHD[x$Chart..Table.ID == i][1],", ",Highlight,", ", x$Year[x$Chart..Table.ID == i][1])
pdf_title <- str_replace_all(string = pdf_title, pattern = "-", repl ="\u2013")
pdf_title <- sapply(lapply(pdf_title, strwrap, width=70), paste, collapse="\n")
#####for titles#####

#####for footnotes#####
foot_dataset <- subset(x, x$Chart..Table.ID ==i) #subsetting the footnotes for chart 1
foot_dataset2 <- data.frame(foot_dataset$Symbol...Index,"  ", foot_dataset$Footnotes, "\n\n") #converting the footnotes into a dataframe

new2 <- lapply(foot_dataset$Foonotes, str_replace_all, pattern = "-", replacement ="\u2013")
new2
new <- sapply(lapply(new2, strwrap, width=120), paste, collapse="\n")
new
new <- data.frame(new)
new1 <- data.frame(foot_dataset2$foot_dataset.Symbol...Index, new)

new1 <- new1[-6,]
new1
new1$foot_dataset2.foot_dataset.Symbol...Index <- as.character(new1$foot_dataset2.foot_dataset.Symbol...Index)
for (i in 1:nrow(new1)){
  if(grepl("[0-9]",new1[i,1])){
    new1[i,1] <- paste0(new1[i,1],".")    #add '.' to the footnote id
  }
}

tt <- ttheme_minimal(core = list(fg_params = list(fontsize = 7, fontfamily = "verdana", just = c("bottom", "left"), hjust = 0, x = 0.01, y = 1, vjust = 1, col = "#58595B", lineheight = .95)), padding = unit(c(0,1.5),"mm")) #defining the theme for the footnotes table
g2 <- tableGrob(new1[1:nrow(new1), 1:ncol(new1)], rows = NULL, cols = NULL, theme = tt)
g2$widths <- unit(c(5,165), c("mm", "mm"))
#g2[[2]]$widths <- unit(100, "mm")
g2$layout$clip <- "off"
grid.newpage()
grid.draw(g2)
#####for footnotes#####

