## MN State Fair Data
## 2024 

## packages 
library(tidyverse)
library(scales)
library(grid)


##data 
totalCount <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/CurrentSchool/TidyTuesday/mnStateFairData/totalCounts.csv")


plotTotal <- totalCount %>%
  mutate(color = case_when(year == 2024 ~ "A",
                           year != 2024 ~ "B"))%>%
  ggplot(., aes(year, totalAttend, fill = color))+
  
  #add background shading covering fair length 
  annotate("rect", xmin = 1885, xmax = 1918, ymin = -60000, ymax = 2350000, 
           alpha = 0.3, fill = "grey80")+
  annotate("rect", xmin = 1939, xmax = 1971, ymin = -60000, ymax = 2350000, 
           alpha = 0.3, fill = "grey80")+
  annotate("rect", xmin = 1975, xmax = 2026, ymin = -60000, ymax = 2350000, 
           alpha = 0.3, fill = "grey80")+
 
  #label shading with day length
  annotate("text", x = 1902, y = 2450000, label = "6 DAYS",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 3,
           color = "grey30")+
  annotate("text", x = 1928, y = 2450000, label = "8 DAYS",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 3,
           color = "grey30")+
  annotate("text", x = 1955, y = 2450000, label = "10 DAYS",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 3,
           color = "grey30")+
  annotate("text", x = 2000, y = 2450000, label = "12 DAYS",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 3,
           color = "grey30")+
  geom_col(alpha=0.8,
           width = 0.8)+
  
 scale_fill_manual(values = c("#8ab4d1", "#466485"))+
  
   #plot aesthetics
  theme_minimal()+
  coord_cartesian(clip = "off",
                  ylim = c(80000, 2300000))+
  scale_x_continuous(breaks = seq(1885, 2024, by = 15))+
  scale_y_continuous(breaks = function(year) pretty(year)[pretty(year) != 0],
                     labels = label_number(scale = 1e-6, suffix = " M"))+
  theme(plot.margin = unit(c(1.5,0.1,0.75,0.1), "in"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(margin = margin(t = 8),
                                   size = 7))


  #ADD CHART LABELS: 
    plotTotal + labs(x = "",
       y = "")+
  annotate("text", x = 1885, y = 3050000, label = "Minnesota State Fair Attendance",
           hjust = 0,
           family = "Open Sans",
           fontface = "bold",
           size = 7,
           color = "grey20")+
  annotate("text", x = 1885, y = 2800000, label = "Annual State Fair attendance (in millions) since the fairgrounds were established in Saint Paul in 1885.\n1,925,904 people visited the fair in 2024: the 5th highest attendence ever recorded.",
           hjust = 0,
           family = "Open Sans",
           size = 3.5,
           color = "grey30")+
  annotate("text", x = 2025, y = -630000, label = "Data Source: Minnesota State Fair Foundation 2024 - based on reporting from MPR",
           hjust = 1,
           family = "Open Sans",
           size = 2.5,
           color = "grey60")+

  #cancelation annotations 
  annotate("text", x = 1893, y = -320000, label = "Cancelled 1893",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "grey30")+
  annotate("text", x = 1893, y = -470000, label = "confict with the\nWorld Exposition",
           hjust = 0.5,
           family = "Open Sans",
           size = 2.5,
           color = "grey30")+
  annotate("text", x = 1943, y = -320000, label = "Cancelled 1945",
           hjust = 1,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "grey30")+
  annotate("text", x = 1943, y = -470000, label = "WWII transit\nrestrictions",
           hjust = 1,
           family = "Open Sans",
           size = 2.5,
           color = "grey30")+
  annotate("text", x = 1947, y = -320000, label = "Cancelled 1946",
           hjust = 0,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "grey30")+
  annotate("text", x = 1947, y = -420000, label = "Polio Epidemic",
           hjust = 0,
           family = "Open Sans",
           size = 2.5,
           color = "grey30")+
  annotate("text", x = 2020, y = -320000, label = "Canceled 2020",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "grey30")+
  annotate("text", x = 2020, y = -420000, label = "Covid-19 Pandemic",
           hjust = 0.5,
           family = "Open Sans",
           size = 2.5,
           color = "grey30")+
  
  #notable annotations 
  annotate("text", x = 1897, y = 600000, label = "1899",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "#466485")+
  annotate("text", x = 1897, y = 500000, label = "first fair\nwith electricity",
           hjust = 0.5,
           family = "Open Sans",
           size = 2.5,
           color = "#466485",
           lineheight = 0.9)+
  annotate("text", x = 1915, y = 800000, label = "1915",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "#466485")+
  annotate("text", x = 1915, y = 700000, label = "Ye Old Mill\nopens",
           hjust = 0.5,
           family = "Open Sans",
           size = 2.5,
           color = "#466485",
           lineheight = 0.9)+
  annotate("text", x = 1941, y = 1120000, label = "1947",
           hjust = 1,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "#466485")+
  annotate("text", x = 1941, y = 1010000, label = "Pronto Pup\ndebut",
           hjust = 1,
           family = "Open Sans",
           size = 2.5,
           color = "#466485",
           lineheight = 0.9)+
  annotate("text", x = 1952, y = 1580000, label = "1954",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "#466485")+
  annotate("text", x = 1952, y = 1480000, label = "First crowned Princess\nKay of the Milky Way",
           hjust = 0.5,
           family = "Open Sans",
           size = 2.5,
           color = "#466485",
           lineheight = 0.9)+
  annotate("text", x = 1981, y = 1980000, label = "1979",
           hjust = 0.5,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "#466485")+
  annotate("text", x = 1981, y = 1870000, label = "Sweet Martha's\ndebut",
           hjust = 0.5,
           family = "Open Sans",
           size = 2.5,
           color = "#466485",
           lineheight = 0.9)+
  annotate("text", x = 2014, y = 2230000, label = "2019",
           hjust = 1,
           family = "Open Sans",
           fontface = "bold",
           size = 2.5,
           color = "#466485")+
  annotate("text", x = 2014, y = 2130000, label = "all-time attendance\nrecord (2,126,551)",
           hjust = 1,
           family = "Open Sans",
           size = 2.5,
           color = "#466485",
           lineheight = 1)+

##arrows
  geom_curve(aes(x = 1897, y = 400000, xend = 1899, yend = 200000),
             curvature = 0.2, 
             arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
             color = "#718ba8", size = 0.005)+
  geom_curve(aes(x = 1915, y = 600000, xend = 1915, yend = 400000),
             curvature = 0, 
             arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
             color = "#718ba8", size = 0.005)+
  geom_curve(aes(x = 1942, y = 1070000, xend = 1946.5, yend = 950000),
             curvature = -0.3, 
             arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
             color = "#718ba8", size = 0.005)+
  geom_curve(aes(x = 1952, y = 1350000, xend = 1954, yend = 1010000),
             curvature = 0.2, 
             arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
             color = "#718ba8", size = 0.005)+
  geom_curve(aes(x = 1981, y = 1750000, xend = 1979, yend = 1450000),
             curvature = -0.2, 
             arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
             color = "#718ba8", size = 0.005)+
  geom_curve(aes(x = 2015, y = 2200000, xend = 2019, yend = 2160000),
             curvature = -0.3, 
             arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
             color = "#718ba8", size = 0.005)

## FINAL 
ggsave("totalAttendance.jpeg", width = 12.5, height = 6.5, units = "in")



