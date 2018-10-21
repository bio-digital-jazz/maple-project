library(dplyr)
library(ggplot2)
library(grid)
library(extrafont)


createPlot = function(chart5) {
 
  ggplot(data = chart5, 
         aes(x=reorder(AgeGroup,Order), 
             y = calc_rate)) +
    geom_bar(stat = "identity", 
             position = position_dodge(width = -1), 
             aes(fill = xyz, 
                 group = Gender),
             width =  0.8)  +
    scale_fill_manual(values = c("1 Males" = "#66CCF0", 
                                 "1 Females" = "#C680C0", 
                                 "2 Females" = "#A33399", 
                                 "2 Males" = "#00ABE6", 
                                 "Error Bar" = "black"), 
                      breaks = c("2 Females","2 Males"),
                      labels=c("Female", "Male")) +
    xlab ("Age Group") +
    geom_errorbar(data = chart5, 
                  aes(x = AgeGroup, 
                      ymin = calc_LCI, 
                      ymax = calc_UCI, 
                      group = Gender), 
                  fill = chart5$Gender, 
                  width = 0, 
                  position = position_dodge(width = -1)) +
    geom_point(data = chart5, 
               aes(x = AgeGroup, 
                   y = calc_LCI, 
                   group = Gender, 
                   color = "Error Bar"), 
               fill = "black",
               position = position_dodge(width = -1), 
               shape = 23) +
    geom_point(data = chart5, 
               aes(x = AgeGroup, 
                   y = calc_UCI, 
                   group = Gender), 
               fill = "black",
               position = position_dodge(width = -1), 
               shape = 23) 
  
  
  
}

