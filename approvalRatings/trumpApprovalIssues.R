## Trump Net Approval Ratings by Issue

## Data Sourced from 'Strength in Numbers' and 'Margin of Error'
## Averages computed by G. Elliott Morris (overall) and Adam Carlson (issues)
## gelliotmorris.com


## packages ------------------------------
library(tidyverse)
library(lubridate)
library(grid)
library(ggiraph)
library(htmlwidgets)




## data ------------------------------
trumpDataIssue <- read.csv("approvalRatings/trumpApprovalRatings_byIssue.csv")%>%
  mutate(displayDate = as.Date(displayDate, format = "%d-%b"),
         dayMonth = paste(day(displayDate), month(displayDate, label = TRUE, abbr = TRUE), 
                          sep = " "))%>% # for dates
  
  pivot_longer(cols = economy:trade,
               names_to = "issue",
               values_to = "rating")%>%
  mutate(tooltip_bg = case_when(issue == "economy" ~ "#85D1D1",
                                issue == "immigration" ~ "#5499C7",
                                issue == "inflation" ~ "#EA725D",
                                issue == "trade" ~ "#F5BB62"),
         tooltip = paste0(
           '<div style="background:', tooltip_bg, 
           '; padding:4px; border-radius:4px; font-size:11px; color:white; border:none; font-family:Open Sans;">',
           format(displayDate, "%b %d"), "<br/>", round(rating * 100, 1), "%",'</div>'),
         
         data_id = issue)%>%
  na.omit()

lineLabs <- trumpDataIssue %>%
  filter(displayDate == "2025-05-01")%>%
  mutate(labX = "2025-05-05",
         lab = paste0(str_to_title(issue), "\n", (rating*100), "%"))


## text annotations
title <- "Trump's Net Job Approval Rating by Issue"
subtitle <- "Average net job approval rating (% approve - % disapprove) for Trump on issue-specific poll questions. 
Lines show a 10-day moving average for issue polls."
caption <- "Data and chart inspo from G. Elliott Morris (gelliotmorris.com) and Adam Carlson"


## plot ------------------------------
p<- ggplot(trumpDataIssue)+
  
  geom_hline(yintercept = 0,
             size = 3, color = "white")+
  geom_hline(yintercept = 0,
             lty = 2, color = "grey65")+
  
  # add rating lines: 
  geom_line_interactive(
    aes(x = displayDate, y = rating * 100, color = issue, group = issue, data_id = issue),
    size = 1.2,
    alpha = 0.8) +
  
  geom_point_interactive(
    aes(
      x = displayDate,
      y = rating * 100,
      tooltip = tooltip,
      data_id = issue,
      color = issue
    ),
    size = 3,
    alpha = 0
  )+
  
  scale_color_manual(values = c("#85D1D1","#5499C7","#EA725D","#F5BB62"))+
  
  # set plotting coords
  coord_cartesian(
    clip = "off",
    xlim = as.Date(c("2025-01-30", "2025-05-01")),
    ylim = c(-27, 15))+
  scale_x_date(date_labels = "%d\n%b",
               date_breaks = "7 days")+
  
  # annotations
  geom_text_interactive(data=lineLabs, aes(as.Date(labX), (rating*100), color = issue, data_id = issue, label= lab),
            hjust = 0,
            family = "Open Sans",
            fontface = "bold",
            size = 2.5,
            lineheight = 1)+
  
  # theming 
  theme_minimal()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'white', color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(2.2, 1.75, 0.5 , 1.75), "cm"),
        
        axis.text = element_text(color = "grey65", 
                                 family = "Open Sans",
                                 size = 7),
        axis.text.x = element_text(size= 6))+
  
  labs(x = "",
       y = "")+
  
  
  # add titles/captions: 
  annotate("text", x = as.Date("2025-01-10"), y = 26, label = title,
           hjust = 0,
           color = "grey10",
           alpha=0.8,
           family = "Open Sans",
           fontface = "bold",
           size = 5.5)+
  
  annotate("text", x = as.Date("2025-01-11"), y = 21, label = subtitle,
           hjust = 0,
           color = "grey20",
           alpha=0.8,
           family = "Open Sans",
           size = 2.5,
           lineheight = 1.3)+
  
  annotate("text", x = as.Date("2025-05-15"), y = -35.5, label = caption,
           hjust = 1,
           color = "grey65",
           alpha=0.8,
           family = "Open Sans",
           fontface = "italic",
           size = 1.8)+

# add interpretation tags 
  annotate("text", 
           x = as.Date("2025-01-16"), y = 8, label = "greater\napproval",
           hjust = 1,
           color = "grey60",
           alpha=0.8,
           family = "Open Sans",
           fontface = "italic",
           size = 2,
           lineheight = 1.1)+
  annotate("segment",
           x = as.Date("2025-01-18"), xend = as.Date("2025-01-18"),
           y = 2, yend = 13,  # from y=0 up to y=10
           color = "grey65",
           size = 0.2,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"))+
  
  annotate("text", x = as.Date("2025-01-16"), y = -8, label = "greater\ndisapproval",
           hjust = 1,
           color = "grey60",
           alpha=0.8,
           family = "Open Sans",
           fontface = "italic",
           size = 2,
           lineheight = 1.1)+
  annotate("segment",
           x = as.Date("2025-01-18"), xend = as.Date("2025-01-18"),
           y = -2, yend = -15,  # from y=0 up to y=10
           color = "grey65",
           size = 0.2,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"))


g<- girafe(ggobj = p,
  width_svg = 6,
  height_svg = 4.5,
  options = list(
    opts_tooltip(css = "padding:4px; 
                 font-size:11px; 
                 border-radius:4px; 
                 color:white; 
                 border:none; 
                 font-family:Open Sans;"),
    opts_hover(css = "stroke-width:3; 
               opacity:1; 
               border:none;"),
    opts_hover_inv(css = "opacity:0.2;")))


#final ------------------------------
saveWidget(g, "approvalRatings/trumpApprovalRatingsIssue.html", selfcontained = TRUE)

p
ggsave("approvalRatings/trumpApprovalRatingsIssue.png", width = 7.75, height = 5, units = "in") 

