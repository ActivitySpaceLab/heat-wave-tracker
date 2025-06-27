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
library(gridExtra)
library(RcppRoll)


# Selected cities ####
these_munis = c("Barcelona", "Madrid", "València", "Zaragoza", "Sevilla")

# Spatial data on stations ####
SPAIN_CRS = 25830

station_points = st_read("~/research/SpainTiger/data/cartography/1da1315b/Estaciones_Completas.shp") %>% bind_rows(st_read("~/research/SpainTiger/data/cartography/b29c8d56/Estaciones_Termometricas.shp")) %>% bind_rows(st_read("~/research/SpainTiger/data/cartography/2aa58725/Estaciones_Automaticas.shp")) %>% bind_rows(st_read("~/research/SpainTiger/data/cartography/8892d9c9/Estaciones_Pluviometricas.shp")) %>% st_transform(SPAIN_CRS)

spain_munis = st_read("~/research/SpainTiger/data/cartography/SIGLIM_Publico_INSPIRE/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89/recintos_municipales_inspire_peninbal_etrs89.shp") %>% st_transform(SPAIN_CRS) %>% filter(NATLEVNAME == "Municipio") %>% dplyr::select(municipality_name = NAMEUNIT, NATCODE)

station_points = st_join(station_points, spain_munis)

station_info = station_points %>% st_drop_geometry() %>% dplyr::select(indicativo = INDICATIVO, station_name = NOMBRE, municipality_name, PROVINCIA) %>% distinct()

write_rds(station_info, file = "data/station_info.Rds")

these_stations = station_info %>% filter(municipality_name %in% these_munis) %>% pull(indicativo)

# Loading weather data ####

weather_daily = fread("~/research/realtime-weather-spain/data/spain_weather_daily_historical.csv.gz") %>% filter(indicativo %in% these_stations) %>% mutate(date = as_date(date))

# adding latest realtime weather
weather_realtime = fread("~/research/realtime-weather-spain/data/spain_weather.csv.gz")  %>% filter(idema %in% these_stations) %>% mutate(fint = with_tz(fint, "CET")) %>% pivot_wider(id_cols = c(fint, idema), names_from = measure, values_from = value) %>% mutate(date = as_date(fint)) %>% rename(indicativo = idema)

n_per_day = weather_realtime %>% group_by(date, indicativo) %>% summarise(n = n(), .groups = "drop")

weather_realtime = weather_realtime %>% left_join(n_per_day) %>% filter(n ==24) 

if(nrow(weather_realtime)>0){
  weather_realtime = weather_realtime %>% group_by(date, indicativo) %>% summarise(TX = max(tamax), TN = min(tamin), HRX = max(hr), HRN = min(hr), .groups = "drop") %>% filter(!date %in% weather_daily$date) %>% mutate(date = as_date(date))

  weather_daily = rbindlist(list(weather_daily, weather_realtime))
}

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


# making heat waves by length ####
these_indicativos = unique(weather_daily$indicativo)

this_indicativo = these_indicativos[1]

this_minormax = "min"
this_p = 85

heat_wave_dates_table = bind_rows(lapply(these_indicativos, function(this_indicativo){

  bind_rows(lapply(c("min", "max"), function(this_minormax){
  
  bind_rows(lapply(c(85, 90, 95, 99, 100), function(this_p){
  
    this_threshold_variable = paste0("Hi_", this_minormax, "_", this_p, "p")
    
    this_comparison_variable = paste0("Hi_", this_minormax)
    
  this_weather_daily = weather_daily %>% filter(indicativo == this_indicativo) %>% mutate(test_var = !!sym(this_comparison_variable) >= !!sym(this_threshold_variable)) %>% replace_na(list(test_var = FALSE)) %>% arrange(date) 
  
  this_hw_dates = this_weather_daily %>% filter(test_var) %>% arrange(date) %>% pull(date)
  
  hw_chunks = split(this_hw_dates, cumsum(c(TRUE, diff(this_hw_dates) != 1)))
  
  hw_chunk_lengths = unlist(lapply(hw_chunks, function(this_chunk) rep(length(this_chunk), length(this_chunk))))
  
  return(
    tibble(indicativo = this_indicativo, threshold = this_threshold_variable, n_days = hw_chunk_lengths, date = do.call(c, hw_chunks))
  )
  }))
    
  }))
}))

## 2-day ####

heat_waves_85p_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_min_85p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_90p_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_min_90p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_95p_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_min_95p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_99p_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_min_99p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_100p_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_min_100p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)


## 3-day ####

heat_waves_85p_3d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 3, threshold == "Hi_min_85p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_90p_3d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 3, threshold == "Hi_min_90p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_95p_3d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 3, threshold == "Hi_min_95p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_99p_3d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 3, threshold == "Hi_min_99p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_100p_3d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 3, threshold == "Hi_min_100p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)


## 4-day ####

heat_waves_85p_4d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 4, threshold == "Hi_min_85p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_90p_4d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 4, threshold == "Hi_min_90p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_95p_4d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 4, threshold == "Hi_min_95p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_99p_4d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 4, threshold == "Hi_min_99p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_100p_4d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 4, threshold == "Hi_min_100p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)


## 5-day ####

heat_waves_85p_5d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 5, threshold == "Hi_min_85p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_90p_5d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 5, threshold == "Hi_min_90p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_95p_5d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 5, threshold == "Hi_min_95p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_99p_5d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 5, threshold == "Hi_min_99p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)


heat_waves_100p_5d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 5, threshold == "Hi_min_100p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)



heat_waves_85p_max_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_max_85p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_90p_max_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_max_90p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_95p_max_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_max_95p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_99p_max_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_max_99p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

heat_waves_100p_max_2d = weather_daily %>% left_join(heat_wave_dates_table %>% filter(n_days >= 2, threshold == "Hi_max_100p") %>% dplyr::select(indicativo, date) %>% mutate(heat_wave = TRUE)) %>% filter(heat_wave)

# writing heat wave summary ####
heat_wave_summary = bind_rows(
  heat_waves_85p_2d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 85, days = 2),
  heat_waves_90p_2d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 90, days = 2),
  heat_waves_95p_2d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 95, days = 2),
  heat_waves_100p_2d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 100, days = 2),
  heat_waves_85p_3d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 85, days = 3),
  heat_waves_90p_3d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 90, days = 3),
  heat_waves_95p_3d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 95, days = 3),
  heat_waves_100p_3d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 100, days = 3),
  
  heat_waves_85p_4d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 85, days = 4),
  heat_waves_90p_4d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 90, days = 4),
  heat_waves_95p_4d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 95, days = 4),
  heat_waves_100p_4d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 100, days = 4),
  
  heat_waves_85p_5d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 85, days = 5),
  heat_waves_90p_5d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 90, days = 5),
  heat_waves_95p_5d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 95, days = 5),
  heat_waves_100p_5d %>% dplyr::select(date, indicativo, station_name, municipality_name) %>% mutate(threshold = 100, days = 5)
  ) %>% as.data.table()

fwrite(heat_wave_summary, "data/heat_wave_summary.csv.gz")


# quick check of this year's heat waves
heat_waves_85p_2d %>% filter(date >= as_date("2025-01-01"))%>% pull(municipality_name) %>% unique()

heat_waves_90p_2d %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_95p_2d %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_99p_2d %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_100p_2d %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()


heat_waves_85p_5d %>% filter(date >= as_date("2025-01-01"))%>% pull(municipality_name) %>% unique()

heat_waves_90p_5d %>% filter(date >= as_date("2025-01-01"))%>% pull(municipality_name) %>% unique()

heat_waves_95p_5d %>% filter(date >= as_date("2025-01-01"))%>% pull(municipality_name) %>% unique()

heat_waves_99p_5d %>% filter(date >= as_date("2025-01-01"))%>% pull(municipality_name) %>% unique()



heat_waves_85p_max_2d %>% filter(date >= as_date("2025-01-01"))%>% pull(municipality_name) %>% unique()

heat_waves_90p_max_2d %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_95p_max_2d %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_99p_max_2d %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()

heat_waves_100p_max_2d %>% filter(date >= as_date("2025-01-01")) %>% pull(municipality_name) %>% unique()


# writing summary of dates ####
summary_readings_past_week = weather_daily %>% filter(date>=today()-7) %>% group_by(date, municipality_name) %>% summarize(n = n(), .groups = "drop") %>% pivot_wider(id_cols = date, names_from = municipality_name, values_from = n) %>% arrange(date) 


summary_readings_past_week %>% fwrite("data/summary_readings_past_week.csv.gz")

this_muni = "Sevilla"

# loop through munis ####

for(this_muni in these_munis){
  
  this_weather_daily = weather_daily %>% filter(municipality_name == this_muni)
  
  this_heat_waves_85p_2d = heat_waves_85p_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_90p_2d = heat_waves_90p_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_95p_2d = heat_waves_95p_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_99p_2d = heat_waves_99p_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_100p_2d = heat_waves_100p_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
  
  
  this_heat_waves_85p_3d = heat_waves_85p_3d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_90p_3d = heat_waves_90p_3d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_95p_3d = heat_waves_95p_3d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_99p_3d = heat_waves_99p_3d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_100p_3d = heat_waves_100p_3d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
  
  this_heat_waves_85p_4d = heat_waves_85p_4d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_90p_4d = heat_waves_90p_4d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_95p_4d = heat_waves_95p_4d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_99p_4d = heat_waves_99p_4d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_100p_4d = heat_waves_100p_4d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
  
  this_heat_waves_85p_5d = heat_waves_85p_5d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_90p_5d = heat_waves_90p_5d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_95p_5d = heat_waves_95p_5d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_99p_5d = heat_waves_99p_5d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
  this_heat_waves_85p_max_2d = heat_waves_85p_max_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_90p_max_2d = heat_waves_90p_max_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_95p_max_2d = heat_waves_95p_max_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_99p_max_2d = heat_waves_99p_max_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  this_heat_waves_100p_max_2d = heat_waves_100p_max_2d %>% filter(indicativo %in% unique(this_weather_daily$indicativo))
  
  
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
  
  ## 2-day spells ####
  
  this_p = ggplot(this_weather_daily, aes(x=date, y=Hi_min)) + 
    geom_line() + 
    geom_point(data=this_heat_waves_85p_2d, aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") +
    geom_point(data=this_heat_waves_90p_2d, aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_2d, aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_2d, aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/full_timeseries_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/full_timeseries_", this_muni, ".Rds"))
  
  this_p = ggplot(this_weather_daily %>% filter(date >= today()-120, date < today()), aes(x=date, y=Hi_min))+ geom_line() + geom_line(aes(x=date, y=Hi_max), color="pink") +
    geom_point(data=this_heat_waves_85p_2d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") + 
    geom_point(data=this_heat_waves_90p_2d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_2d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_2d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/recent_timeseries_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/recent_timeseries_", this_muni, ".Rds"))
  
  ## 3-say spells ####
  
 this_p = ggplot(this_weather_daily, aes(x=date, y=Hi_min)) + 
    geom_line() + 
    geom_point(data=this_heat_waves_85p_3d, aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") +
    geom_point(data=this_heat_waves_90p_3d, aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_3d, aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_3d, aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/full_timeseries_3d_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/full_timeseries_3d_", this_muni, ".Rds"))
  
  this_p = ggplot(this_weather_daily %>% filter(date >= today()-120, date < today()), aes(x=date, y=Hi_min))+ geom_line() + geom_line(aes(x=date, y=Hi_max), color="pink") +
    geom_point(data=this_heat_waves_85p_3d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") + 
    geom_point(data=this_heat_waves_90p_3d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_3d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_3d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/recent_timeseries_3d_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/recent_timeseries_3d_", this_muni, ".Rds"))
  
  
  ## 4-say spells ####
  
  this_p = ggplot(this_weather_daily, aes(x=date, y=Hi_min)) + 
    geom_line() + 
    geom_point(data=this_heat_waves_85p_4d, aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") +
    geom_point(data=this_heat_waves_90p_4d, aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_4d, aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_4d, aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/full_timeseries_4d_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/full_timeseries_4d_", this_muni, ".Rds"))
  
  this_p = ggplot(this_weather_daily %>% filter(date >= today()-120, date < today()), aes(x=date, y=Hi_min))+ geom_line() + geom_line(aes(x=date, y=Hi_max), color="pink") +
    geom_point(data=this_heat_waves_85p_4d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") + 
    geom_point(data=this_heat_waves_90p_4d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_4d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_4d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/recent_timeseries_4d_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/recent_timeseries_4d_", this_muni, ".Rds"))
  
  ## 5-day spells ####
  
  
  this_p = ggplot(this_weather_daily, aes(x=date, y=Hi_min)) + 
    geom_line() + 
    geom_point(data=this_heat_waves_85p_5d, aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") +
    geom_point(data=this_heat_waves_90p_5d, aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_5d, aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_5d, aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/full_timeseries_5d_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/full_timeseries_5d_", this_muni, ".Rds"))  
  
  this_p = ggplot(this_weather_daily %>% filter(date >= today()-120, date < today()), aes(x=date, y=Hi_min))+ geom_line() + geom_line(aes(x=date, y=Hi_max), color="pink") +
    geom_point(data=this_heat_waves_85p_5d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#ff0000aa") + 
    geom_hline(data = this_Hi_min_85p_table, aes(yintercept = Hi_min_85p), color = "red") + 
    geom_point(data=this_heat_waves_90p_5d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="#9900aaaa") + 
    geom_hline(data = this_Hi_min_90p_table, aes(yintercept = Hi_min_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_5d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="yellow") + 
    geom_hline(data = this_Hi_min_95p_table, aes(yintercept = Hi_min_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_5d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_min), color="green") + 
    geom_hline(data = this_Hi_min_99p_table, aes(yintercept = Hi_min_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/recent_timeseries_5d_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/recent_timeseries_5d_", this_muni, ".Rds"))
  
  
  # now maximums
  
  this_p = ggplot(this_weather_daily, aes(x=date, y=Hi_max)) + 
    geom_line() + 
    geom_point(data=this_heat_waves_85p_max_2d, aes(x=date, y=Hi_max), color="#ff0000aa") + 
    geom_hline(data = this_Hi_max_85p_table, aes(yintercept = Hi_max_85p), color = "red") +
    geom_point(data=this_heat_waves_90p_max_2d, aes(x=date, y=Hi_max), color="#9900aaaa") + 
    geom_hline(data = this_Hi_max_90p_table, aes(yintercept = Hi_max_90p), color = "purple") +
    geom_point(data=this_heat_waves_95p_max_2d, aes(x=date, y=Hi_max), color="yellow") + 
    geom_hline(data = this_Hi_max_95p_table, aes(yintercept = Hi_max_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_max_2d, aes(x=date, y=Hi_max), color="green") + 
    geom_hline(data = this_Hi_max_99p_table, aes(yintercept = Hi_max_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/full_timeseries_maximums_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/full_timeseries_maximums_", this_muni, ".Rds"))
  
  this_p = ggplot(this_weather_daily %>% filter(date >= today()-120, date < today()), aes(x=date, y=Hi_min))+ geom_line() + geom_line(aes(x=date, y=Hi_max), color="pink") +
    geom_point(data=this_heat_waves_85p_max_2d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_max), color="#ff0000aa") + 
    geom_hline(data = this_Hi_max_85p_table, aes(yintercept = Hi_max_85p), color = "red") + 
    geom_point(data=this_heat_waves_90p_max_2d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_max), color="#9900aaaa") + 
    geom_hline(data = this_Hi_max_90p_table, aes(yintercept = Hi_max_90p), color = "purple") + 
    geom_point(data=this_heat_waves_95p_max_2d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_max), color="yellow") + 
    geom_hline(data = this_Hi_max_95p_table, aes(yintercept = Hi_max_95p), color = "yellow") +
    geom_point(data=this_heat_waves_99p_max_2d %>% filter(year(date) == year(today())), aes(x=date, y=Hi_max), color="green") + 
    geom_hline(data = this_Hi_max_99p_table, aes(yintercept = Hi_max_99p), color = "green") +
    facet_wrap(.~station_name)
  
  ggsave(this_p, file = paste0("plots/recent_timeseries_maximums_", this_muni, ".png"), height = 6, width = 8)
  write_rds(this_p, file = paste0("plots/recent_timeseries_maximums_", this_muni, ".Rds"))
  
}
