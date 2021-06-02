rm(list = ls())
library(tidyverse)
library(readxl)
library(gridExtra)
library(cartogram) 
library(sf)  
library(broom)        
library(tweenr)       
library(gganimate)    
library(rgdal) 
library(ggforce)
library(data.table)
#setwd("C:/Users/ConditionKim/Desktop/성균관대학교/2021-1/대용량자료관리및시각화/프로젝트/shiny")
seoul <- readOGR("data/seoul_gu_polygon.shp",
                 encoding = "UTF-8", 
                 use_iconv = T)

data <- fread("data/map.csv",
              header = T,
              data.table = F,
              encoding = "UTF-8")

data <- data %>% mutate(시가총액 = 시가총액/1000000000000)

seoul@data <- seoul@data %>% 
  left_join(data, by = c("NM" = "자치구명")) %>% 
  mutate(시가총액 = as.numeric(시가총액),
             id = as.character(0 : (nrow(seoul@data) - 1 )), 
             id_carto = as.character(1:nrow(seoul@data))) #%>% 
#  select(-c(AVG_PUB_PR, GU_AREA, FULL_NM), -starts_with("TOTAL"))

###############
## Cartogram ## 
###############
## 서부 원점, 중부 원점, 동부 원점, 동해(울릉) 원점
crs_vector <- c(
  "+proj=tmerc +lat_0=38 +lon_0=125 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs",
  "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs",
  "+proj=tmerc +lat_0=38 +lon_0=129 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs",
  "+proj=tmerc +lat_0=38 +lon_0=131 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs"
)
options(warn = -1)
seoul_sptrans <- spTransform(seoul, crs_vector[1]) ## Projection : polar coordinate -> Cartesian

seoul_carto <- cartogram_cont(seoul_sptrans, "시가총액", itermax = 10)

seoul_carto_dt <- tidy(seoul_carto) %>% 
  left_join(seoul_carto@data, by = c("id" = "id_carto")) 

seoul_dt <- tidy(seoul_sptrans) %>% left_join(seoul_sptrans@data, by = "id")

seoul_dt$id = seq(1, nrow(seoul_dt))
seoul_carto_dt <- seoul_carto_dt %>% 
  mutate(id = seq(1, nrow(seoul_carto_dt))) %>% 
  rename(id_carto = id.y)

rm(list =  ls()[! ls() %in% c("seoul_dt")])

draw_map <- function(DF){
  DF %>% 
    ggplot() + 
    geom_polygon(aes(fill = 시가총액, 
                     x = long, 
                     y = lat, 
                     group = group,
                     color = 시가총액), 
                 size = 1.1, 
                 alpha = 0.8) +
    scale_fill_gradient2(name = "시가총액(조)",
                         breaks = c(10, 20, 30, 40, 50, 60),
                         midpoint = 30,
                         low = "skyblue",
                         mid = "#FFEBF5",
                         high = "#FF6565",
                         guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                              keywidth=unit(12, units = "mm"), 
                                              label.position = "bottom", 
                                              title.position = 'top', 
                                              nrow = 1)) +
    scale_color_gradient2(name = "시가총액(조)",
                          breaks = c(10, 20, 30, 40, 50, 60),
                          midpoint = 30,
                          low = "skyblue",
                          mid = "#FFEBF5",
                          high = "#FF6565",
                          guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                               keywidth=unit(12, units = "mm"), 
                                               label.position = "bottom", 
                                               title.position = 'top', 
                                               nrow = 1)) +
    labs( title = "서울시 시가총액", subtitle="서울시 건물 시가총액", x =NULL, y = NULL) +
    theme(
      text = element_text(color = "#22211d", face = "bold"), 
      plot.background = element_rect(fill = "#f5f5f4", color = "black", size = 1.5), 
      panel.background = element_rect(fill = "#f5f5f4", color = NA), 
      legend.background = element_rect(fill = "#f5f5f4", color = NA),
      plot.title = element_text(size= 22, 
                                hjust=0.5, 
                                color = "#4e4d47", 
                                margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      plot.subtitle = element_text(size= 13, 
                                   hjust=0.5, 
                                   color = "#4e4d47", 
                                   margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position = c(0.2, 0.86))
}

save.image("data/map.RData")
