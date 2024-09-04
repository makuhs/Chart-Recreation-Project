## Animated Temperature Data 

## Packages 
library(tidyverse)
library(ggtext)
library(patchwork)
library(gganimate)
library(gifski)



## Data Set-Up 
data_source <- "\n\n2023 - NASA Earth Observatory Goddard Institude for Space Studies GISS Surface Temperature Analysis V4"

global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')


## Plot

scale <- c(-0.5, 0, 0.25, 0.75)

title <- "\nSUMMER 2023: THE WARMEST ON RECORD"
subtitle <- "Global Surface Temperature Anomaly (°C) for June, July, and August\nData shows departure from the global surface temperature average from 1951-1980\n"

tempAni <- global_temps

tempAni[144,18] = 1.2

p1<- ggplot(tempAni, aes(Year, JJA, fill = JJA))+
  geom_col(aes(group = seq_along(Year)),
           show.legend = F, color = "white", linewidth = 0.08)+
  scale_x_continuous(breaks=seq(1880,2020,by=20))+
  geom_hline(yintercept = 0, lty = 3, color = "grey50")+
  
  annotate("rect", xmin = 1920, xmax = 1965, ymin = 0.51, ymax = 0.7,
           fill = "white")+
  geom_text(aes(x = 1951, y = 0.6, label = sprintf("%5.2f°C", JJA), color = JJA), 
            hjust = 0.5,
            show.legend = F,
            fontface='bold',
            size = 12) + 
  geom_text(aes(x = 1951, y = 0.75, label = sprintf("%5.0f", Year)), 
            hjust = 0.5,
            show.legend = F,
            color = "grey45",
            size = 8) +
  geom_text(aes(x = 1951, y = 1.215, label = subtitle), 
            family = "Open Sans",
            hjust = 0.5,
            show.legend = F,
            color = "grey30",
            size = 3.3)+
  scale_fill_gradientn(colors = c("#3384C6", "white","#e9ba74", "#BF2210", "#731409"),
                       breaks=scale,
                       limits = c(-0.5, 1.3))+
  scale_color_gradientn(colors = c("#3384C6", "white","#e9ba74", "#BF2210", "#731409"),
                        breaks=scale,
                        limits = c(-0.5, 1.3))+
  
  theme_minimal()+
  coord_cartesian(clip = "off")+
  labs(x = "",
       y = "",
       title = title,
       caption = data_source)+
  
  theme(plot.title = element_text(
    face = "bold",
    family = "DIN Alternate",
    color = "grey25",
    size = 25,
    hjust  = 0.5),
    plot.caption = element_text(size = 6.5,
                                color = "grey40",
                                hjust = 1),
    plot.margin = unit(c(0.4, 0.2, 0.2, 0.2), "in"))+
  transition_reveal(Year)


animate(p1, rewind = FALSE, nframes = 143, fps = 8, end_pause = 20,
        width =9, height = 6, units = "in", res = 300,
        renderer = gifski_renderer("time_series.gif"))