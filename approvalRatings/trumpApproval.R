## Trump Approval Ratings 

## Data Sourced from 'Strength in Numbers' and 'Margin of Error'
## Averages computed by G. Elliott Morris (overall) and Adam Carlson (issues)


## packages
library(tidyverse)
library(lubridate)
library(plotly)

## data
trumpData <- read.csv("approvalRatings/trumpApprovalRatings.csv")%>%
  mutate(displayDate = as.Date(displayDate, format = "%d-%b"),
         dayMonth = paste(day(displayDate), month(displayDate, label = TRUE, abbr = TRUE), 
                          sep = " "))

## text annotations
title <- "Trump's Job Approval Rating 2025"
subtitle <- "Average approval and disapproval rating for Donald Trump's second term. 
All adults, all polls, rolling 10-day average."
caption <- "Data and chart inspo from G. Elliott Morris (gelliotmorris.com) Strength in Numbers"

## plot
ggplot(trumpData)+
  
  # mexico/canada/china tariffs into effect: 
  geom_vline(xintercept = as.Date("2025-03-04"),
             lty = 2,
             color = "grey70",
             alpha=0.5)+
  annotate("text", x = as.Date("2025-03-02"), y = 56.75, label = "Tariffs begin",
           hjust = 1,
           color = "grey65",
           alpha=0.8,
           family = "Open Sans Bold Italic",
           size = 3)+
  annotate("text", x = as.Date("2025-03-02"), y = 55.7, label = "Mexico, Canada, & China",
           hjust = 1,
           color = "grey65",
           alpha=0.8,
           family = "Open Sans Italic",
           size = 2.3)+
  
  # add rating lines: 
  geom_line(aes(displayDate, (approve*100)),
            color = "#3b71a3",
            size = 1.5,
            alpha=0.8)+
  geom_line(aes(displayDate, (disapprove*100)),
            color = "#c7665f",
            size = 1.5,
            alpha=0.8)+
  
  # set plotting coords
  coord_cartesian(
    clip = "off",
    xlim = as.Date(c("2025-01-30", "2025-05-01")),
    ylim = c(35, 60))+
  scale_x_date(date_labels = "%d\n%b",
               date_breaks = "7 days")+
  
 # add line annotations: 
   annotate("text", x = as.Date("2025-05-4"), y = 41.7, label = "Approve\n41.6%",
           hjust = 0,
           color = "#3b71a3",
           alpha=0.8,
           family = "Open Sans Bold",
           size = 3.25,
           lineheight = 1)+
  annotate("text", x = as.Date("2025-05-4"), y = 54.7, label = "Disapprove\n54.7%",
           hjust = 0,
           color = "#c7665f",
           alpha=0.8,
           family = "Open Sans Bold",
           size = 3.25,
           lineheight = 1)+
  
  # theming: 
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(2.2, 2.5, 0.5 , 0.2), "cm"),
        
        axis.text = element_text(color = "grey65"),
        axis.text.x = element_text(size= 8))+
  
  labs(x = "",
       y = "")+ 
  
  # add titles/captions: 
  annotate("text", x = as.Date("2025-01-22"), y = 66.5, label = title,
           hjust = 0,
           color = "grey10",
           alpha=0.8,
           family = "Open Sans Bold",
           size = 5.9)+
  
  annotate("text", x = as.Date("2025-01-22"), y = 63.4, label = subtitle,
           hjust = 0,
           color = "grey10",
           alpha=0.8,
           family = "Open Sans Italic",
           size = 3,
           lineheight = 1.1)+
  
  annotate("text", x = as.Date("2025-05-20"), y = 28, label = caption,
           hjust = 1,
           color = "grey65",
           alpha=0.8,
           family = "Open Sans Italic",
           size = 2,
           lineheight = 1.1)
  
  
ggplotly(p)
             