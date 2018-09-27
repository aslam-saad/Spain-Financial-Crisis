
##
## Required libraries
library(caret)
library(tidyverse)
library(readxl)
library(knitr)
library(extrafont)
loadfonts(device = "win")


## Importing datasets 
spainOECD <- read.csv("data/spain OECD.csv") #the first dataset
spainOECD <- spainOECD %>%
    select(Time, Variable, Value) %>%
    spread(key = Variable, value = Value)

spainIMF <- read.csv("data/spain IMF.csv")  #the second dataset
spainIMF <- spainIMF %>%
    gather(key = "Time", value = "value", starts_with("X")) %>%
    spread(key = Subject.Descriptor, value = value) %>%
    select(-Units) %>%
    mutate(Time = str_extract(Time, "[0-9]{4}"))

spainData <- merge(spainOECD, spainIMF)  #merging the both datasets 



## The rest lines of the code represent the plots used in the document
## Every chunk represents a plot in the order of their appearance in the document
##
## The first figure
spainData %>%
    mutate(GAP = ifelse(`Output gap as a percentage of potential GDP` <= 0,                 
                        "Recessionary", "Inflationary"), Time = as.factor(Time)) %>%
    ggplot()+
    geom_bar(aes(x = Time, y = `Output gap as a percentage of potential GDP`, fill = GAP), stat = "identity") + 
    scale_fill_manual(name = "GAP", values = c("skyblue", "#ee4057"))+
    scale_y_continuous(limits = c(-12, 12), breaks = seq(-12, 12, by=2))+
    labs(x = NULL, title = "Spain Output Gaps From 2000 To 2018", caption = "Data Source: OECD.org")+
    guides(fill = "none")+
    theme(axis.title.y = element_text(size = 11, family = "Courier New"),
          plot.title = element_text(size = 14, color = "#112367", hjust = .5,
                                    family = "Courier New", face = "bold"),
          plot.caption = element_text(color = "grey50"),
          axis.text.x = element_text(angle = 70, vjust = .4, hjust = .5), 
          axis.ticks = element_line(color = "grey50"),
          axis.ticks.length = unit(.1, "cm"),
          plot.margin  = margin(.15, .15, .15, .15, "inches"),
          panel.border = element_rect(size = 1.2, fill = NA, color = "grey50")) 


## The second figure
spainData %>%
    select(Time, `Unemployment rate`, `Gross domestic product, volume, growth`)%>%
    gather(`Unemployment rate`, `Gross domestic product, volume, growth`, 
           key = "indicator", value = "value") %>%
    ggplot()+
    geom_line(aes(x = Time, y = value, color = indicator), size = 1) +
    scale_x_continuous(limits = c(2000, 2018), breaks = seq(2000, 2018, by=1))+
    scale_y_continuous(limits = c(-5, 29), breaks = seq(-5, 29, by=2))+
    scale_color_manual(name = NULL, values = c("#ee4057", "#23B133"), labels = c("Unemploymet", "GDP"),
                       limits = c("Unemployment rate", "Gross domestic product, volume, growth"))+
    labs(x = NULL, y = NULL, caption = "Data Source: OECD.org")+
    theme(
        plot.caption = element_text(color = "grey50"),
        axis.text.x = element_text(angle = 70, vjust = .4, hjust = .5), 
        axis.ticks = element_line(color = "grey50"),
        axis.ticks.length = unit(.1, "cm"),
        plot.margin  = margin(.15, .15, .15, .15, "inches"),
        panel.border = element_rect(size = 1.2, fill = NA, color = "grey50"),
        legend.position = c(.1,.88))


## The third figure
spainData %>%
    select(Time, `Net general government interest payments as a percentage of GDP`, 
           `Gross public debt, Maastricht criterion as a percentage of GDP`)%>%
    gather(`Net general government interest payments as a percentage of GDP`, 
           `Gross public debt, Maastricht criterion as a percentage of GDP`, 
           key = "indicator", value = "value") %>%
    mutate(Time = as.factor(Time)) %>%
    ggplot(aes(x = Time, y = value, fill = indicator), alpha = .5)+
    geom_bar(stat = "identity") +
    geom_label(aes(label = ifelse(value <= 5, round(value, 1), round(value, 0))), 
               color = "#ffffff", size = 3.5)+
    facet_wrap(~indicator, ncol = 1, scales = "free_y")+
    scale_fill_manual(name = "indicator", values = c( rgb(188/360,143/360,143/360, alpha = .8), 
                                                      rgb(49/360,79/360,79/360, alpha = .7)))+
    guides(fill = "none")+
    labs(y = NULL, x = NULL, caption = "Data Source: OECD.org")+
    theme(
        panel.grid = element_blank(), 
        plot.caption = element_text(color = "grey50"),
        axis.text.x = element_text(angle = 70, vjust = .4, hjust = .5), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "grey50"),
        axis.ticks.length = unit(.1, "cm"),
        plot.margin = margin(.15, .15, .15, .15, "inches"),
        panel.border = element_rect(size = 1.1, fill = NA, color = "grey50"), 
        strip.background = element_rect(fill = "grey50", color = "grey50"),
        strip.text = element_text(size = 10, color = "white"))


## The forth figure
spainData %>%
    select(Time, `Total investment`, `Gross national savings`, `Current account balance`)%>%
    gather(`Total investment`, `Gross national savings`, `Current account balance`,
           key = "indicator", value = "value") %>%
    mutate(Time = as.factor(Time), indicator = fct_rev(indicator)) %>%
    ggplot()+
    geom_bar(aes(x = Time, y = value, fill = indicator),  
             stat = "identity", position = position_dodge(.8)) + 
    scale_fill_manual(name = NULL, values = c("#ee4057", "skyblue", "orange"))+
    scale_y_continuous(limits = c(-10, 50), breaks = seq(-10, 59, by=5))+
    labs(x = NULL, y = NULL, caption = "Data Source: IMF.org")+
    theme(
        plot.caption = element_text(color = "grey50", size = 9),
        axis.text.x = element_text(angle = 70, vjust = .4, hjust = .5), 
        axis.ticks = element_line(color = "grey50"),
        axis.ticks.length = unit(.1, "cm"),
        plot.margin  = margin(.15, .15, .15, .15, "inches"),
        panel.border = element_rect(size = 1.2, fill = NA, color = "grey50"),
        legend.text = element_text(size = 9),
        legend.position = c(.85,.80))


## The fifth figure
spainData %>%
    ggplot()+
    geom_hline(yintercept = 0, color = "grey")+
    geom_smooth(aes(x = Time, y = `Output gap as a percentage of potential GDP`), 
                color = "grey50", se = F, size = 1)+
    geom_histogram(aes(x = Time, y = `General government net lending as a percentage of GDP` ),
                   stat = "identity", fill = "cornsilk", color = "grey", alpha = .7, bins = 19) + 
    scale_x_continuous(breaks = seq(2000, 2018, by=1))+
    scale_y_continuous(limits = c(-15, 8), breaks = seq(-15, 8, by=2))+
    labs(x = NULL, y = "Budget Balance As A Percentage Of GDP", 
         caption = "Data Source: OECD.org", 
         title = "Budget Balance During The Phases Of Business Cycle")+
    theme(
        plot.title = element_text(size = 9, color = "#112367", hjust = .5,
                                  family = "Courier New", face = "bold"),  
        axis.title.y = element_text(size = 11, family = "Courier New"),
        plot.caption = element_text(color = "grey50", size = 8),
        axis.text.x = element_text(angle = 70, vjust = .4, hjust = .5), 
        axis.ticks = element_line(color = "grey50"),
        axis.ticks.length = unit(.1, "cm"),
        panel.grid.minor.x = element_blank(),
        plot.margin  = margin(.15, .15, .15, .15, "inches"),
        panel.border = element_rect(size = 1.2, fill = NA, color = "grey50"))


## The sixth figure
spainData %>%
    ggplot()+
    geom_hline(yintercept = 0, color = "grey")+
    annotate("rect", xmin = 1999.5, xmax = 2007.5, ymin = -3, ymax = 3, fill = "red", alpha = .15)+
    geom_bar(aes(x = Time, y = `Short-term interest rate` - `Harmonised headline inflation`), 
             stat = "identity", color = "grey50", size = 1)+
    scale_x_continuous(breaks = seq(2000, 2018, by=1))+
    scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by=1))+
    labs(x = NULL, y = "Real Interest Rate As A Percentage Of GDP", 
         caption = "Data Source: OECD.org")+
    theme(
        axis.title.y = element_text(size = 11, family = "Courier New"),
        plot.caption = element_text(color = "grey50", size = 9),
        axis.text.x = element_text(angle = 70, vjust = .4, hjust = .5), 
        axis.ticks = element_line(color = "grey50"),
        axis.ticks.length = unit(.1, "cm"),
        panel.grid.minor.x = element_blank(),
        plot.margin  = margin(.15, .15, .15, .15, "inches"),
        panel.border = element_rect(size = 1.2, fill = NA, color = "grey50"))

