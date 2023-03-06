
library(tidyverse)
library(tidyverse)
library(here)
library(XML)
library(lubridate)
library(ggmap)
library(geosphere)
options(digits.secs = 3)
options(scipen = 999)

# parse GPX file
getwd()
lund_gpx_file_names <- list.files("Running_gpx")
lund_gpx_dfs <- sapply(lund_gpx_file_names, function(file_name){
      path_tmp <- paste0("Running_gpx/", file_name)
      parsed <- htmlTreeParse(file = path_tmp, useInternalNodes = TRUE)
      
      # get values via via the respective xpath
      coords <- xpathSApply(parsed, path = "//trkpt", xmlAttrs)
      elev   <- xpathSApply(parsed, path = "//trkpt/ele", xmlValue)
      ts_chr <- xpathSApply(parsed, path = "//trkpt/time", xmlValue)
      
      # combine into df 
      dat_df <- data.frame(
        ts_POSIXct = ymd_hms(ts_chr, tz = "EST"),
        lat = as.numeric(coords["lat",]), 
        lon = as.numeric(coords["lon",]), 
        elev = as.numeric(elev)
      )
      # head(dat_df)
      
      dat_df <- 
        dat_df %>%
        mutate(lat_lead = lead(lat)) %>%
        mutate(lon_lead = lead(lon)) %>%
        rowwise() %>%
        mutate(dist_to_lead_m = distm(c(lon, lat), c(lon_lead, lat_lead), fun = distHaversine)[1,1]) %>%
        ungroup()
      
      # compute time elapsed (in seconds) between subsequent GPS points
      dat_df <- 
        dat_df %>%
        mutate(ts_POSIXct_lead = lead(ts_POSIXct)) %>%
        mutate(ts_diff_s = as.numeric(difftime(ts_POSIXct_lead, ts_POSIXct, units = "secs"))) 
      
      # compute metres per seconds, kilometres per hour 
      dat_df <- 
        dat_df %>%
        mutate(speed_m_per_sec = dist_to_lead_m / ts_diff_s) %>%
        mutate(speed_km_per_h = speed_m_per_sec * 3.6)
      
      # remove some columns we won't use anymore 
      dat_df <- 
        dat_df %>% 
        select(-c(lat_lead, lon_lead, ts_POSIXct_lead, ts_diff_s)) %>% 
        mutate(runid = file_name)
      # head(dat_df) %>% as.data.frame()
      return(dat_df) }, simplify = F, USE.NAMES = T)

register_google(key = "AIzaSyD-ABPuJHfqz6jluiJEQeN_aqwcu0DBzBM")

lund_gpx <- lund_gpx_dfs %>% do.call("rbind", .) %>% filter(runid != "Lund Running (3).gpx")
      
lund_lon_range <- lund_gpx %>% .$lon %>% range
lund_lat_range <- lund_gpx %>% .$lat %>% range


# get the map background 
bbox <- make_bbox(lund_lon_range,lund_lat_range)
dat_df_map <- get_googlemap(center = c(mean(lund_lon_range), mean(lund_lat_range)), zoom = 13, maptype = "satellite" )
# no Google token alternative: 
# dat_df_map <- get_map(bbox, maptype = "toner-lite", source = "stamen")

# generate plot

ggmap(dat_df_map) + 
  # geom_point(data = dat_df, aes(lon, lat), size = 1, alpha = 0.5) +
  geom_path(data = lund_gpx, aes(lon, lat, group = runid), alpha = .3, color = "orange", size = 2.5) +
  # geom_label(data = dat_df_dist_marks, aes(lon, lat, label = dist_m_cumsum_km_floor),size = 3) +
  # labs(x = "Longitude", y = "Latitude", # color = "Elev. [m]", title = "Jan's runs in Lund in 2022") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

# Warsaw marathon ---------------------------------------------------------

marathon_gpx_dfs <- sapply(c("Gdansk_marathon.gpx"), function(file_name){
  path_tmp <- paste0("", file_name)
  parsed <- htmlTreeParse(file = path_tmp, useInternalNodes = TRUE)
  
  # get values via via the respective xpath
  coords <- xpathSApply(parsed, path = "//trkpt", xmlAttrs)
  elev   <- xpathSApply(parsed, path = "//trkpt/ele", xmlValue)
  ts_chr <- xpathSApply(parsed, path = "//trkpt/time", xmlValue)
  
  # combine into df 
  dat_df <- data.frame(
    ts_POSIXct = ymd_hms(ts_chr, tz = "EST"),
    lat = as.numeric(coords["lat",]), 
    lon = as.numeric(coords["lon",]), 
    elev = as.numeric(elev)
  )
  # head(dat_df)
  
  dat_df <- 
    dat_df %>%
    mutate(lat_lead = lead(lat)) %>%
    mutate(lon_lead = lead(lon)) %>%
    rowwise() %>%
    mutate(dist_to_lead_m = distm(c(lon, lat), c(lon_lead, lat_lead), fun = distHaversine)[1,1]) %>%
    ungroup()
  
  # compute time elapsed (in seconds) between subsequent GPS points
  dat_df <- 
    dat_df %>%
    mutate(ts_POSIXct_lead = lead(ts_POSIXct)) %>%
    mutate(ts_diff_s = as.numeric(difftime(ts_POSIXct_lead, ts_POSIXct, units = "secs"))) 
  
  # compute metres per seconds, kilometres per hour 
  dat_df <- 
    dat_df %>%
    mutate(speed_m_per_sec = dist_to_lead_m / ts_diff_s) %>%
    mutate(speed_km_per_h = speed_m_per_sec * 3.6)
  
  # remove some columns we won't use anymore 
  dat_df <- 
    dat_df %>% 
    select(-c(lat_lead, lon_lead, ts_POSIXct_lead, ts_diff_s)) %>% 
    mutate(runid = file_name)
  # head(dat_df) %>% as.data.frame()
  return(dat_df) }, simplify = F, USE.NAMES = T)



marathon_gpx <- marathon_gpx_dfs %>% do.call("rbind", .) %>% filter(runid != "marathon Running (3).gpx") %>% filter(speed_km_per_h < 15 & speed_km_per_h > 10)

marathon_lon_range <- marathon_gpx %>% .$lon %>% range
marathon_lat_range <- marathon_gpx %>% .$lat %>% range


# get the map background 
bbox <- make_bbox(marathon_lon_range,marathon_lat_range)
dat_df_map <- get_googlemap(center = c(mean(marathon_lon_range), mean(marathon_lat_range)), zoom = 12, maptype = "satellite" )
# no Google token alternative: 
# dat_df_map <- get_map(bbox, maptype = "toner-lite", source = "stamen")

# generate plot
library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")
library(zoo)
ggmap(dat_df_map) + 
  # geom_point(data = dat_df, aes(lon, lat), size = 1, alpha = 0.5) +
  geom_path(data = marathon_gpx, aes(lon, lat, color = speed_km_per_h),  size = 2.5) +
  # geom_label(data = dat_df_dist_marks, aes(lon, lat, label = dist_m_cumsum_km_floor),size = 3) +
  # labs(x = "Longitude", y = "Latitude", # color = "Elev. [m]", title = "Jan's runs in marathon in 2022") +
  scale_color_gradientn(colours = pal) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

