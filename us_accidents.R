library(tidyverse)
library(usmap)
library(plotly)
library(lubridate)
library(data.table)
library(zoo)
us_accidents <- fread("US_Accidents_Dec19.csv")

head(us_accidents) %>% View

#which day, hour do most accidents happen
#which state do most accidents occur, yearly average
#weather condition that most accidents occur
setnames(us_accidents, "State", "abbr") # to work with maps data

sum_dat <- us_accidents[, .(freq = .N), by = abbr] %>%
    .[order(freq, decreasing = T)]



## us map
#just a general view

us_df <- usmap::us_map()


us_sum_acc <- merge(sum_dat, us_df, by ="abbr" )


ggplotly(ggplot(us_sum_acc, aes(x, y, group = group, 
                                fill = freq, text = full))+
    geom_polygon()+
    theme_void())


time_cols <- c("Start_Time", "End_Time")

us_accidents[, (time_cols) := lapply(.SD, ymd_hms), .SDcols = time_cols]

#extract date
us_accidents[, Start_Date := as.Date(Start_Time)]

## tred in numbers

#on average how many deaths occur per day
daily_dat <- us_accidents[, .(freq = .N),
                          by = .(abbr,Start_Date)] %>%
    .[, .(daily_average = mean(freq)), by = abbr]


us_daily_acc <- merge(daily_dat, us_df, by ="abbr" )


ggplotly(ggplot(us_daily_acc, aes(x, y, group = group, 
                                fill = daily_average, text = full))+
             geom_polygon()+
             theme_void())

## time of the day

## extract which counties from california and texas
##

library(sf)
counties <- map_data("county") %>% setDT()
ca_counties <- counties[region == "california"]

ca_counties <- st_read("CA_Counties")

ca_counties  <- st_transform(ca_counties , crs = "+proj=longlat")



ca_counties_sp <- as(ca_counties, Class = "Spatial")

library(spatialEco)
ca_acc <- us_accidents[abbr == "CA"]

proj4string = CRS("+proj=longlat")
ca_acc_shp <- SpatialPointsDataFrame(ca_acc[,.(Start_Lng,Start_Lat)], 
                                      ca_acc ,    #the R object to convert
                                      proj4string = proj4string)   # assign a CRS 


pts.poly <- point.in.poly(ca_acc_shp, ca_counties_sp)

ca_acc_comb <- st_as_sf(pts.poly)

head(ca_acc_comb) %>% View

## quick map
## an ovelar map 

ggplot(ca_counties)+
    geom_sf()

ggplot(ca_counties)+
    geom_sf()+
    geom_point(data = ca_acc,
               aes(Start_Lng,Start_Lat), 
               color = "blue") +
    theme_void()

df_sum <- ca_acc_comb %>% group_by(NAME, Start_Date) %>%
    summarise(freq = n()) %>%
    group_by(NAME) %>%
    summarise(mean_daily = mean(freq))%>%
    st_drop_geometry() %>%
    left_join(ca_counties)

df_sum <- st_set_geometry(df_sum, "geometry")

pp <- ggplot(df_sum, aes(fill = freq))+
    geom_sf() +
    theme_void()

plotly:: ggplotly(pp)

library(tmap)

ttm()
tm_shape(df_sum)+
    tm_polygons(col= "mean_daily", n = 5, palette = "BuPu")+
    tm_borders(col = "black")
