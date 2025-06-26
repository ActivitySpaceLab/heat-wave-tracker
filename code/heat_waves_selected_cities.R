# Title ####
# For detecting and plotting heatwaves. 

rm(list=ls())

# Dependencies ####
library(tidyverse)
library(lubridate)
library(data.table)
library(tidyverse)
library(weathermetrics)
library(sf)

# Selected cities ####
these_munis = c("Barcelona", "Madrid", "València", "Zaragoza", "Sevilla")

# Spatial data on stations ####
SPAIN_CRS = 25830

station_points = st_read("~/research/SpainTiger/data/cartography/1da1315b/Estaciones_Completas.shp") %>% bind_rows(st_read("~/research/SpainTiger/data/cartography/b29c8d56/Estaciones_Termometricas.shp")) %>% bind_rows(st_read("~/research/SpainTiger/data/cartography/2aa58725/Estaciones_Automaticas.shp")) %>% bind_rows(st_read("~/research/SpainTiger/data/cartography/8892d9c9/Estaciones_Pluviometricas.shp")) %>% st_transform(SPAIN_CRS)

spain_munis = st_read("~/research/SpainTiger/data/cartography/SIGLIM_Publico_INSPIRE/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp") %>% st_transform(SPAIN_CRS) %>% filter(NATLEVNAME == "Municipio") %>% dplyr::select(municipality_name = NAMEUNIT, NATCODE)

station_points = st_join(station_points, spain_munis)

station_info = station_points %>% st_drop_geometry() %>% dplyr::select(indicativo = INDICATIVO, station_name = NOMBRE, municipality_name, PROVINCIA) %>% distinct()

these_stations = station_info %>% filter(municipality_name %in% these_munis) %>% pull(indicativo)

# Loading weather data ####

weather_daily = fread("~/research/realtime-weather-spain/data/spain_weather_daily_historical.csv.gz") %>% filter(indicativo %in% these_stations) %>% mutate(date = as_date(date))

# adding latest realtime weather
weather_realtime = fread("~/research/realtime-weather-spain/data/spain_weather.csv.gz")  %>% filter(idema %in% these_stations) %>% pivot_wider(id_cols = c(fint, idema), names_from = measure, values_from = value) %>% mutate(date = as_date(fint)) %>% group_by(date, indicativo = idema) %>% summarise(TX = max(tamax), TN = min(tamin), HRX = max(hr), HRN = min(hr), .groups = "drop") %>% filter(!date %in% weather_daily$date) %>% mutate(date = as_date(date))

weather_daily = rbindlist(list(weather_daily, weather_realtime))

# Cleaning ####
weather_daily %>% filter(!is.na(HRN)) %>% pull(HRN) %>% range() # HRN looks fine.

weather_daily %>% filter(!is.na(HRX)) %>% pull(HRX) %>% range() # HRX looks fine.

# in case it goes over:
# weather_daily$HRX[which(weather_daily$HRX>100)] = NA

# Heat index ####
weather_daily = weather_daily %>% mutate(Hi_max = heat.index(t = TX, rh = HRX, temperature.metric	= "celsius", output.metric = "celsius"), Hi_min = heat.index(t = TN, rh = HRN, temperature.metric	= "celsius", output.metric = "celsius"))

weather_daily = weather_daily %>% left_join(station_info, by = "indicativo")


# renoving stations that don't have full time series (or close to it)
weather_daily = weather_daily %>% filter(!station_name %in% c("MADRID, C. UNIVERSITARIA", "MADRID/CUATRO VIENTOS")) 

# checking how many stations in each muni
weather_daily %>% dplyr::select(municipality_name, indicativo) %>% distinct() %>% group_by(municipality_name) %>% summarize(n())

stations = unique(weather_daily$indicativo)

# for threshholds, using previous 10 years (2015-2024), June, July and August
weather_daily_thresholds = weather_daily %>% filter(year(date) < 2025, year(date)>2014) %>% mutate(month = month(date)) %>% filter(month %in% c(6, 7, 8)) 

weather_daily_year_counts = weather_daily_thresholds %>% filter(!is.na(Hi_min), !is.na(Hi_max)) %>% group_by(year(date), indicativo, station_name, municipality_name) %>% summarise(n = n(), .groups = "drop") %>% pivot_wider(id_cols = c(indicativo, station_name, municipality_name), names_from = `year(date)`, values_from = n)
# based on this, I am removing stations 9434 and 0201X because they each have some years missing and/or with very low numbers of readings. The others looks mostly fine in terms of close to the expected readings each year

bad_stations = c('9434', '0201X')

# also two of the weather_daily stations do not even appear in the threshold table because they have no readings from those months (or maybe none at all)
weather_daily_all_stations = unique(weather_daily$indicativo)

other_bad_stations = weather_daily_all_stations[which(!weather_daily_all_stations %in% unique(weather_daily_year_counts$indicativo))]

bad_stations = c(bad_stations, other_bad_stations)

weather_daily_thresholds = weather_daily_thresholds %>% filter(!indicativo %in% bad_stations)

weather_daily = weather_daily %>% filter(!indicativo %in% bad_stations)

unique(weather_daily$indicativo)

Hi_min_85p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_min_85p = as.numeric(quantile(Hi_min, probs = 0.85, na.rm=TRUE)), n = sum(!is.na(Hi_min)), .groups = "drop")

Hi_min_90p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_min_90p = as.numeric(quantile(Hi_min, probs = 0.90, na.rm=TRUE)), n = sum(!is.na(Hi_min)), .groups = "drop")

Hi_min_95p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_min_95p = as.numeric(quantile(Hi_min, probs = 0.95, na.rm=TRUE)), n = sum(!is.na(Hi_min)), .groups = "drop")

Hi_min_99p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_min_99p = as.numeric(quantile(Hi_min, probs = 0.99, na.rm=TRUE)), n = sum(!is.na(Hi_min)), .groups = "drop")

Hi_min_100p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_min_100p = as.numeric(quantile(Hi_min, probs = 1, na.rm=TRUE)), n = sum(!is.na(Hi_min)), .groups = "drop")

Hi_max_85p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_max_85p = as.numeric(quantile(Hi_max, probs = 0.85, na.rm=TRUE)), n = sum(!is.na(Hi_max)), .groups = "drop")

Hi_max_90p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_max_90p = as.numeric(quantile(Hi_max, probs = 0.90, na.rm=TRUE)), n = sum(!is.na(Hi_max)), .groups = "drop")

Hi_max_95p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_max_95p = as.numeric(quantile(Hi_max, probs = 0.95, na.rm=TRUE)), n = sum(!is.na(Hi_max)), .groups = "drop")

Hi_max_99p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_max_99p = as.numeric(quantile(Hi_max, probs = 0.99, na.rm=TRUE)), n = sum(!is.na(Hi_max)), .groups = "drop")

Hi_max_100p_table = weather_daily_thresholds %>% group_by(indicativo, station_name) %>% summarise(Hi_max_100p = as.numeric(quantile(Hi_max, probs = 1, na.rm=TRUE)), n = sum(!is.na(Hi_max)), .groups = "drop")

Hi_min_thresholds_table = Hi_min_85p_table %>% dplyr::select(-n) %>% left_join(Hi_min_90p_table %>% dplyr::select(-n)) %>% left_join(Hi_min_95p_table %>% dplyr::select(-n)) %>% left_join(Hi_min_99p_table %>% dplyr::select(-n)) %>% left_join(Hi_min_100p_table %>% dplyr::select(-n)) %>% as.data.table()

fwrite(Hi_min_thresholds_table, "data/Hi_min_thresholds_table.csv.gz")

Hi_max_thresholds_table = Hi_max_85p_table %>% dplyr::select(-n) %>% left_join(Hi_max_90p_table %>% dplyr::select(-n)) %>% left_join(Hi_max_95p_table %>% dplyr::select(-n)) %>% left_join(Hi_max_99p_table %>% dplyr::select(-n)) %>% left_join(Hi_max_100p_table %>% dplyr::select(-n)) %>% as.data.table()

fwrite(Hi_max_thresholds_table, "data/Hi_max_thresholds_table.csv.gz")

weather_daily = weather_daily %>%
  left_join(Hi_min_thresholds_table) %>%
  left_join(Hi_max_thresholds_table) 

heat_waves_85p = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_min >= Hi_min_85p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()


heat_waves_90p = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_min >= Hi_min_90p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()

heat_waves_95p = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_min >= Hi_min_95p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()

heat_waves_99p = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_min >= Hi_min_99p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()

heat_waves_100p = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_min >= Hi_min_100p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()


heat_waves_85p_max = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_max >= Hi_max_85p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()


heat_waves_90p_max = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_max >= Hi_max_90p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()

heat_waves_95p_max = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_max >= Hi_max_95p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()

heat_waves_99p_max = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_max >= Hi_max_99p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()

heat_waves_100p_max = weather_daily %>% group_by(indicativo, station_name) %>% filter(Hi_max >= Hi_max_100p) %>% arrange(date) %>% mutate(date_diff_back = c(0, diff(date)), date_diff_forward = c(diff(date), 0)) %>% filter(date_diff_back == 1 | date_diff_forward == 1) %>% ungroup()


heat_wave_summary = bind_rows(
  heat_waves_85p %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 85),
  heat_waves_90p %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 90),
  heat_waves_95p %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 95),
  heat_waves_100p %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 100)
  ) %>% as.data.table()

fwrite(heat_wave_summary, "data/heat_wave_summary.csv.gz")


# quick check of this year's heat waves
heat_waves_85p %>% filter(date >= as_date("2025-01-01"))%>% pull(municipality_name) %>% unique()

heat_waves_90p %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_95p %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_99p %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_100p %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()


heat_waves_85p_max %>% filter(date >= as_date("2025-01-01"))%>% pull(municipality_name) %>% unique()

heat_waves_90p_max %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_95p_max %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_99p_max %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_100p_max %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()


# writing summary of dates ####
weather_daily %>% filter(date>=today()-7) %>% group_by(date, municipality_name) %>% summarize(n = n(), .groups = "drop") %>% pivot_wider(id_cols = date, names_from = municipality_name, values_from = n) %>% arrange(date) %>% fwrite("data/summary_readings_past_week.csv.gz")

this_muni = "Sevilla"

for(this_muni in these_munis){
  
  this_weather_daily = weather_daily %>% filter(municipality_name == this_muni)
  
  this_heat_waves_85p = heat_waves_85p %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_90p = heat_waves_90p %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_95p = heat_waves_95p %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_99p = heat_waves_99p %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_100p = heat_waves_100p %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
  this_heat_waves_85p_max = heat_waves_85p_max %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_90p_max = heat_waves_90p_max %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_95p_max = heat_waves_95p_max %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_99p_max = heat_waves_99p_max %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_100p_max = heat_waves_100p_max %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
  this_Hi_min_85p_table = Hi_min_85p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_Hi_min_90p_table = Hi_min_90p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_Hi_min_95p_table = Hi_min_95p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_Hi_min_99p_table = Hi_min_99p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_Hi_min_100p_table = Hi_min_100p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
  
  this_Hi_max_85p_table = Hi_max_85p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_Hi_max_90p_table = Hi_max_90p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_Hi_max_95p_table = Hi_max_95p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_Hi_max_99p_table = Hi_max_99p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_Hi_max_100p_table = Hi_max_100p_table %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
  ggplot(this_weather_daily, aes(x=date, y=Hi_min)) + 
    geom_line() + 
    geom_point(data=this_heat_waves_85p, aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") +
    geom_point(data=this_heat_waves_90p, aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p, aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p, aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(paste0("plots/full_timeseries_", this_muni, ".png"), height = 6, width = 8)
  
  
  ggplot(this_weather_daily %>% filter(date >= today()-120, date < today()), aes(x=date, y=Hi_min))+ geom_line() + geom_line(aes(x=date, y=Hi_max), color="pink") +
    geom_point(data=this_heat_waves_85p %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") + 
    geom_point(data=this_heat_waves_90p %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") + ylim(c(10, 45)) + 
    geom_point(data=this_heat_waves_95p %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(paste0("plots/recent_timeseries_", this_muni, ".png"), height = 6, width = 8)
  
  
  # now maximums
  
  ggplot(this_weather_daily, aes(x=date, y=Hi_max)) + 
    geom_line() + 
    geom_point(data=this_heat_waves_85p, aes(x=date, y=Hi_max), color="#ff0000aa") + 
    geom_hline(data = this_Hi_max_85p_table, aes(yintercept = Hi_max_85p), color = "red") +
    geom_point(data=this_heat_waves_90p_max, aes(x=date, y=Hi_max), color="#9900aaaa") + 
    geom_hline(data = this_Hi_max_90p_table, aes(yintercept = Hi_max_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_max, aes(x=date, y=Hi_max), color="yellow") + 
    geom_hline(data = this_Hi_max_95p_table, aes(yintercept = Hi_max_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_max, aes(x=date, y=Hi_max), color="green") + 
    geom_hline(data = this_Hi_max_99p_table, aes(yintercept = Hi_max_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(paste0("plots/full_timeseries_maximums_", this_muni, ".png"), height = 6, width = 8)
  
  
  ggplot(this_weather_daily %>% filter(date >= today()-120, date < today()), aes(x=date, y=Hi_min))+ geom_line() + geom_line(aes(x=date, y=Hi_max), color="pink") +
    geom_point(data=this_heat_waves_85p_max %>% filter(year(date) == year(today())), aes(x=date, y=Hi_max), color="#ff0000aa") + 
    geom_hline(data = this_Hi_max_85p_table, aes(yintercept = Hi_max_85p), color = "red") + 
    geom_point(data=this_heat_waves_90p_max %>% filter(year(date) == year(today())), aes(x=date, y=Hi_max), color="#9900aaaa") + 
    geom_hline(data = this_Hi_max_90p_table, aes(yintercept = Hi_max_90p), color = "purple") + 
    geom_point(data=this_heat_waves_95p_max %>% filter(year(date) == year(today())), aes(x=date, y=Hi_max), color="yellow") + 
    geom_hline(data = this_Hi_max_95p_table, aes(yintercept = Hi_max_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_max %>% filter(year(date) == year(today())), aes(x=date, y=Hi_max), color="green") + 
    geom_hline(data = this_Hi_max_99p_table, aes(yintercept = Hi_max_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(paste0("plots/recent_timeseries_maximums_", this_muni, ".png"), height = 6, width = 8)
  
  
}


