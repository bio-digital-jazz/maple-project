###Cycle8####
#Bowel screening
#23_BWS_1a_2#
---
title: #23_BWS_1a_2#
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
options(scipen = 999)

unique_lhds2 <- data2 %>%
  distinct(LHD.Name)
######SET CORRECT WD###########

#####for titles#####
i = 2 #set the number of the chart here
x <- read.csv("./Data/Bowel RBCO footnotes 2018.csv") #reading the footnotes files
table2 <- read.csv("./Data//bowel_LGA_LHD.csv")

#####for pdf name####
sno <- "23"
program <- "BWS" #CVS, BWS, BRS, HLP, SKP
type <- "1a" #Type of chart
pdf_name <- paste0(sno,"_", program,"_", i,"_", type,"_",Highlight,"_", "Cycle8.pdf")
#####for pdf name####

#pdf_title <- paste0(x$Chart...Table.title[i][1],", ", x$LHD[i][1],", ", x$Year[i][1])
pdf_title <- paste0(x$Chart...Table.title[x$Chart..Table.ID == i][1],", ", x$LHD[x$Chart..Table.ID== i][1],", ", x$Year[x$Chart..Table.ID == i][1])
#pdf_title <- paste0(x$Chart...Table.title[i][1],", ", x$LHD[i][1],", ", "2015 and 2016")
pdf_title <- str_replace_all(string = pdf_title, pattern = "-", repl ="\u2013")
pdf_title <- sapply(lapply(pdf_title, strwrap, width=75), paste, collapse="\n")
#####for titles#####

#####for footnotes#####
foot_dataset <- subset(x, x$Chart..Table.ID ==i) #subsetting the footnotes for chart 1
foot_dataset2 <- data.frame(foot_dataset$Symbol...Index,"  ", foot_dataset$Footnotes, "\n\n") #converting the footnotes into a dataframe

new2 <- lapply(foot_dataset$Foonotes, str_replace_all, pattern = "-", replacement ="\u2013")
new2
new <- sapply(lapply(new2, strwrap, width=130), paste, collapse="\n")
new
new <- data.frame(new)
new1 <- data.frame(foot_dataset2$foot_dataset.Symbol...Index, new)

new1
new1$foot_dataset2.foot_dataset.Symbol...Index <- as.character(new1$foot_dataset2.foot_dataset.Symbol...Index)
for (i in 1:nrow(new1)){
  if(grepl("[0-9]",new1[i,1])){
    new1[i,1] <- paste0(new1[i,1],".")    #add '.' to the footnote id
  }
}
tt <- ttheme_minimal(core = list(fg_params = list(fontsize = 7, fontfamily = "verdana", just = c("top", "left"), hjust = 0, x = 0.01, y = 1, vjust = 1, col = "#58595B", lineheight = 1)), padding = unit(c(0,1.5),"mm")) #defining the theme for the footnotes table
g2 <- tableGrob(new1[1:nrow(new1), 1:ncol(new1)], rows = NULL, cols = NULL, theme = tt)
g2$widths <- unit(c(5,165), c("mm", "mm"))
#g2[[2]]$widths <- unit(100, "mm")
g2$layout$clip <- "off"
grid.newpage()
grid.draw(g2)
#####for footnotes#####

#####the plot#####
#head(table2)
#table2$intact_pop <- format((as.integer(table2$intact_pop, length = 0)), big.mark = ",")
#table2$intact_pop
#table2$Respondents[table2$LHD.Name=="NSW" & table2$Year == "2016"]
#unique(table2$year)

colnames(table2)[1] <- "LHD.Name"
colnames(table2)[2] <- "LGA.Name"
colnames(table2)[3] <- "Year"
colnames(table2)[4] <- "Rate"
colnames(table2)[5] <- "Lower.CI"
colnames(table2)[6] <- "Upper.CI"
colnames(table2)[7] <- "Respondents"
head(table2)


table2$Respondents <-  format((table2$Respondents), big.mark = ",")
table2$Rate <- round(as.numeric(table2$Rate)*100, digits = 3)
table2$Lower.CI <- round(as.numeric(table2$Lower.CI)*100, digits = 3)
table2$Upper.CI <- round(as.numeric(table2$Upper.CI)*100, digits = 3)


x <- as.character(table2$Respondents)
y <- as.character(table2$LHD.Name)
x <- str_replace_all(string=x, pattern=" ", repl="")


#var1 <- paste0(table2$LHD.Name, '\n', "(N=", as.character(table2$Women.Screened),")", sep = "")
var1 <- paste(y, '\n', "(N=", x ,")", sep = "")

table2 <- cbind(table2, var1) 

#table2 <- subset(table2, as.character(LHD.Name) == as.character(LGA.Name))


Highlight <- "Hunter New England LHD" #specify the loop code here

#cols2 = c("2014-2015" = "#B3B3B3", "2015-2016" = "#66CCF0", "Highlight" = "#005AAD", "NSW" = "black", "ErrorBars" = "black")
cols2 = c("2016" = "#B3B3B3", "2017" = "#66CCF0", "LHD" = "#005AAD", "NSW2" = "black", "NSW1" = "#999999", "ErrorBars" = "black")

table2 <- arrange(table2, desc(table2$Rate))

table2 <- subset(table2, as.character(LHD.Name) == as.character(LGA.Name))

table2 %>%
  arrange(Rate)



table2$Year <- factor(table2$Year)

table2.1 <- table2 %>%
  filter(Year %in% c("2016"))%>%
  arrange(Rate) %>%
  filter(LHD.Name != "NSW")
table2.2 <- table2 %>%
  filter(Year %in% c("2017"))%>%
  arrange(Rate) %>%
  filter(LHD.Name != "NSW") 

#being used later in the code to specify the vale for y intercept in geom_hline(aes(yintercept = yy,color="NSW"), size = 1) 
y2 <- (table2$Rate[table2$LHD.Name=="NSW" & table2$Year == "2017"])
y1 <- (table2$Rate[table2$LHD.Name=="NSW" & table2$Year == "2016"])


#New code added to create new variable Rate2 for ordering the bars in geom_bar 
table2.2$Rate2 <- seq(1,length.out = nrow(table2.2), by =1)
table2.2.2 <- table2.2[,c("LHD.Name","Rate2")]

table2.1.1 <- merge(table2.2.2,table2.1, by="LHD.Name")
#New code added to create new variable Rate2 for ordering the bars in geom_bar 

g1 <- ggplot() +
  #data was changed on this line from table2.1 to table2.1.1
  geom_bar(data = table2.1.1, aes(x = reorder(LHD.Name, Rate2), y = Rate, fill = Year), stat = "identity") +
  geom_bar(data = table2.2, width = 0.6, aes(x = factor(LHD.Name), y = Rate, fill = Year), stat = "identity") +
  geom_bar(data = subset(table2.2, LHD.Name == Highlight), aes(x = LHD.Name, y = Rate, fill = "LHD"), width = 0.6, stat = "identity", alpha = 0.8) +
  geom_text(data = table2.2, aes(x = factor(LHD.Name), label = format(Rate, digits = 3), y = 4), family = "Verdana", color = "white", size = (2.47), fontface = "bold") +
  
  geom_segment(aes(y = y1, yend = y1, x = table2.2$LHD.Name, xend = (nrow(table2.2) - 0.5 ), color = "NSW1"), size = 1.0633)  +
  geom_segment(aes(y = y2, yend = y2, x = table2.2$LHD.Name, xend = (nrow(table2.2) - 0.5 ), color = "NSW2"), size = 1.0633)  +geom_errorbar(data = table2.2, aes(x = factor(LHD.Name),ymin = Lower.CI, ymax=Upper.CI, colour = "ErrorBars"), width = 0, size = 0.469177215) +
  xlab("Local Health District") +
  ylab("Participation rate (%)") +
  #  geom_point(data = table2.2, aes(x = LHD.Name, y = Lower.CI,color = "ErrorBar"),shape=23,fill="black") +
  geom_point(data = table2.2, aes(x = LHD.Name, y = Lower.CI),shape=23,fill="black") +
  geom_point(data = table2.2, aes(x = LHD.Name, y = Upper.CI),shape=23,fill="black") +
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
        #axis.ticks.margin = unit(3, "mm"),
        legend.position = c(0.5, -0.18),
        legend.text = element_text(colour = "#58595B", size = 7, lineheight = 1),
        legend.title=element_blank(),
        legend.direction = "horizontal",
        legend.justification = "bottom",
        legend.spacing.y = unit(-2.3, "mm"),
        legend.background = element_rect(fill = "NA", colour = "NA"),
        legend.key.size = unit(6, "mm"),
        legend.spacing.x = unit(1, "mm")
  ) +
  
  scale_y_continuous(breaks = seq(0, 100, 10), limits =c(0, 100), expand = c(0,0)) +
  scale_x_discrete(breaks = table2.2$LHD.Name, labels = table2.2$var1) +
  guides(fill=guide_legend(order = 1), colour=guide_legend(
    override.aes = list(size = 0.469177215))) +
  #  scale_fill_manual(values = cols2, breaks = c("Jul2015-Jun2017", "Jul2014-Jun2016", "LHD"), labels = c("Jul 2015\u2013Jun 2017  ","Jul 2014\u2013Jun 2016  ",  Highlight)) +
  scale_fill_manual(values = cols2, breaks = c("2017","2016","LHD" ), labels = c(  "2017  ","2016   ", Highlight)) +
  scale_color_manual(name = "", values = c("NSW2" = "black","NSW1" = "#999999" , "ErrorBars" = "black"), breaks= c("NSW2", "NSW1", "ErrorBars"), labels = c("NSW 2017 (36.8% \nN=686,325)","NSW 2016 (37.8% \nN=573,924)", "Confidence interval"))
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
                                      heights=unit(c(20, 10, 4, 170, 4, 59, 30), c("mm", "mm", "mm", "mm", "mm","mm", "mm"))))  

top_margin <- viewport(layout.pos.row=1, layout.pos.col=2, name="top_margin")                         	#row 1, column 2 Top Margin
title <- viewport(layout.pos.row=2, layout.pos.col=2, name="title")                                   	#row 2, column 2 Plot title
padding <- viewport(layout.pos.row=3, layout.pos.col = 2,name="padding")                                #row 3, column 2 padding bw title and plot
plot <- viewport(layout.pos.row=4, layout.pos.col = 2,name="plot")                                    	#row 4, column 2 Actual Plot/graph
legend <- viewport(layout.pos.row=5, layout.pos.col = 2,name="legend")                                  #row 5, column 2 Actual Plot/graph
footnotes <- viewport(layout.pos.row=6, layout.pos.col=2, name="footnotes")                             #row 6, column 2 Bottom section for Footnotes
bottom_margin <- viewport(layout.pos.row=7, layout.pos.col=2, name="bottom_margin")                   	#row 7, column 2 Bottom Margin

#Define viewport tree by adding above viewport and then push these viewports to the grid using pushViewport command
splot <- vpTree(top.vp, vpList(top_margin,title, padding, plot,footnotes,bottom_margin)) # Defining the hierarchy of the viewports
pushViewport(splot) # Creating viewports for plotting with the definitions of splot

upViewport()
downViewport("top_margin")
#grid.text("22top margin 20 mm",gp = gpar(col="grey"))

upViewport()
downViewport("title")
grid.text(pdf_title,x=unit(1,"mm"),y=unit(1,"npc") - unit(1,"mm"),just = c("left","bottom"), vjust = 1, gp = gpar(col="#8C0080", fontsize=11, fontface = "bold", fontfamily = "verdana", lineheight = 1))  
#grid.text(splitString(pdf_title),x=0, y=1, just=c("left", "top"),gp = gpar(col="#8C0080", fontsize=11, fontface = "bold"))

upViewport()
downViewport("padding")
#grid.text("padding 8 mm",gp = gpar(col="grey",fontsize=8))

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
downViewport("footnotes")
grid.rect(x=seq(0, 1, length = 131), y=0.76, width = unit(0.65, "mm"), height = unit(0.65, "mm"), default.units="npc", name=NULL,gp=gpar(fill = "#E2E3E4", col = "NA"), draw=TRUE, vp=NULL)
grid.draw(g2)


upViewport()
downViewport("bottom_margin")
dev.off()


