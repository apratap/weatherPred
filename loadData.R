rm(list = ls())
library(install.load)
install_load("tidyverse", "data.table", "hydroTSM")

dataFolder = "data/historical-hourly-weather-data/"
SELECTED_CITY = "New York"

humidity = fread( paste0(dataFolder,"humidity.csv"), data.table = F)
humidity = humidity %>% select(datetime, humidity=all_of(SELECTED_CITY))

pressure = fread( paste0(dataFolder,"pressure.csv"), data.table = F)
pressure = pressure %>% select(datetime, pressure=all_of(SELECTED_CITY))

temperature = fread( paste0(dataFolder,"temperature.csv"), data.table = F)
temperature = temperature %>% select(datetime, temperature=all_of(SELECTED_CITY)) %>%
  dplyr::mutate(temperatureF = ((9/5)*temperature) - 459.67) %>%
  select(-temperature)

weatherDesc = fread( paste0(dataFolder,"weather_description.csv"), data.table = F)
weatherDesc = weatherDesc %>% select(datetime, weatherDesc=all_of(SELECTED_CITY))

windDir = fread( paste0(dataFolder,"wind_direction.csv"), data.table = F)
windDir = windDir %>% select(datetime, windDir=all_of(SELECTED_CITY))

windSp = fread( paste0(dataFolder,"wind_speed.csv"), data.table = F)
windSp = windSp %>% select(datetime, windSp=all_of(SELECTED_CITY))

### Binary Var that is T if there was any sort of rain
weatherDesc <- weatherDesc %>% 
  filter(! ( is.na(weatherDesc) | weatherDesc == '')) %>%
  mutate(rain = ifelse(stringr::str_detect(weatherDesc, 'rain|thunder|drizzle'), "Yes", "False"),
         rain = factor(rain),
         weatherType = case_when(
           stringr::str_detect(weatherDesc, 'rain|thunder|drizzle') ~ 'rain',
           stringr::str_detect(weatherDesc, 'cloud|fog|mist') ~ 'cloudy',
           stringr::str_detect(weatherDesc, 'snow') ~ 'snow',
           stringr::str_detect(weatherDesc, 'clear') ~ 'clear',
           TRUE ~ 'other')
         ) %>%
  select(-weatherDesc)

##Final data
data.by.hour <- Reduce(function(x,y) merge(x,y, all=T), list(humidity, pressure, temperature, weatherDesc, windDir, windSp ))


#Annotate other vars
data.by.hour <- data.by.hour %>% 
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime),
                date = as.Date(datetime),
                hour = lubridate::hour(datetime),
                month = lubridate::month(datetime, label=T, abbr=T),
                week = lubridate::week(datetime),
                year = lubridate::year(datetime),
                season = hydroTSM::time2season(datetime, out.fmt = "seasons") ) %>%
  as.data.frame() %>%
  dplyr::mutate(rain = as.factor(rain),
                tod = case_when(
                  hour %in% c(0,1,2,3) ~ "Late-night\n(12-4 AM)",
                  hour %in% c(4,5,6,7) ~ "Early-Morning\n(4-8 AM)",
                  hour %in% c(8,9,10,11) ~ "Morning\n(8-12 PM)",
                  hour %in% c(12,13,14,15) ~ "Afternoon\n(12-4 PM)",
                  hour %in% c(16,17,18,19) ~ "Afternoon\n(4-8 PM)",
                  hour %in% c(20,21,22,23) ~ "Afternoon\n(8-12 PM)"),
                month = factor(as.character(month)),
                week = factor(as.character(week)),
                hour = factor(as.character(hour)),
                season = factor(as.character(season)))

##Agg by day
data.by.day <- data.by.hour %>% 
  dplyr::group_by(date, year, month, season, week) %>%
  dplyr::arrange(hour) %>%
  dplyr::summarise(rain = ifelse( sum(rain == 'Yes', na.rm = T) > 0, 'Yes', 'No'),
                   tempF.mean = mean(temperatureF, na.rm = T),
                   tempF.median = median(temperatureF, na.rm = T),
                   tempF.IQR = IQR(temperatureF, na.rm = T),
                   tempF.max = max(temperatureF, na.rm=T),
                   tempF.min = min(temperatureF, na.rm=T),
                   tempF.SD = sd(temperatureF, na.rm=T),
                   tempF.change.sd = sd(diff(temperatureF, lag=1), na.rm = T),
                   tempF.change.avg = mean(diff(temperatureF, lag=1), na.rm = T),
                   humidity.mean = mean(humidity, na.rm = T),
                   humidity.median = median(humidity, na.rm = T),
                   humidity.IQR = IQR(humidity, na.rm = T),
                   humidity.max = max(humidity, na.rm=T),
                   humidity.min = min(humidity, na.rm=T),
                   humidity.SD = sd(humidity, na.rm=T),
                   tempF.change.sd = sd(diff(temperatureF, lag=1), na.rm = T),
                   tempF.change.avg = mean(diff(temperatureF, lag=1), na.rm = T),
                   pressure.mean = mean(pressure, na.rm = T),
                   pressure.median = median(pressure, na.rm = T),
                   pressure.IQR = IQR(pressure, na.rm = T),
                   pressure.max = max(pressure, na.rm=T),
                   pressure.min = min(pressure, na.rm=T),
                   pressure.SD = sd(pressure, na.rm=T),
                   pressure.change.sd = sd(diff(pressure, lag=1), na.rm = T),
                   pressure.change.avg = mean(diff(pressure, lag=1), na.rm = T),
                   windDir.mean = mean(windDir, na.rm = T),
                   windDir.median = median(windDir, na.rm = T),
                   windDir.IQR = IQR(windDir, na.rm = T),
                   windDir.max = max(windDir, na.rm=T),
                   windDir.min = min(windDir, na.rm=T),
                   windDir.SD = sd(windDir, na.rm=T),
                   windDir.change.sd = sd(diff(windDir, lag=1), na.rm = T),
                   windDir.change.avg = mean(diff(windDir, lag=1), na.rm = T),
                   windSp.mean = mean(windSp, na.rm = T),
                   windSp.median = median(windSp, na.rm = T),
                   windSp.IQR = IQR(windSp, na.rm = T),
                   windSp.max = max(windSp, na.rm=T),
                   windSp.min = min(windSp, na.rm=T),
                   windSp.SD = sd(windSp, na.rm=T),
                   windSp.change.sd = sd(diff(windSp, lag=1), na.rm = T),
                   windSp.change.avg = mean(diff(windSp, lag=1), na.rm = T)) %>%
  as.data.frame() %>%
  dplyr::mutate(rain = as.factor(rain),
                month = factor(as.character(month)),
                week = factor(as.character(week)),
                season = factor(as.character(season)))





#### Data by Time of Day
tmp_weatherType_by_TOD <- function(x){
  #return the most common weather in the TOD bucket
  x <- sort(table(x))  
  tod_weatherType_tod <- names(x)[length(x)]
  ifelse(is.na(tod_weatherType_tod), NA, tod_weatherType_tod)
}
tmp_weatherType_by_TOD <- purrr::possibly(tmp_weatherType_by_TOD, otherwise = NA)

data.by.tod <- data.by.hour %>% 
  dplyr::group_by(date, year, month, season, week, tod) %>%
  dplyr::arrange(hour) %>%
  dplyr::summarise(rain = ifelse( sum(rain == 'Yes', na.rm = T) > 0, 'Yes', 'No'),
                   tempF.mean = mean(temperatureF, na.rm = T),
                   tempF.median = median(temperatureF, na.rm = T),
                   tempF.IQR = IQR(temperatureF, na.rm = T),
                   tempF.max = max(temperatureF, na.rm=T),
                   tempF.min = min(temperatureF, na.rm=T),
                   tempF.SD = sd(temperatureF, na.rm=T),
                   tempF.change.sd = sd(diff(temperatureF, lag=1), na.rm = T),
                   tempF.change.avg = mean(diff(temperatureF, lag=1), na.rm = T),
                   humidity.mean = mean(humidity, na.rm = T),
                   humidity.median = median(humidity, na.rm = T),
                   humidity.IQR = IQR(humidity, na.rm = T),
                   humidity.max = max(humidity, na.rm=T),
                   humidity.min = min(humidity, na.rm=T),
                   humidity.SD = sd(humidity, na.rm=T),
                   tempF.change.sd = sd(diff(temperatureF, lag=1), na.rm = T),
                   tempF.change.avg = mean(diff(temperatureF, lag=1), na.rm = T),
                   pressure.mean = mean(pressure, na.rm = T),
                   pressure.median = median(pressure, na.rm = T),
                   pressure.IQR = IQR(pressure, na.rm = T),
                   pressure.max = max(pressure, na.rm=T),
                   pressure.min = min(pressure, na.rm=T),
                   pressure.SD = sd(pressure, na.rm=T),
                   pressure.change.sd = sd(diff(pressure, lag=1), na.rm = T),
                   pressure.change.avg = mean(diff(pressure, lag=1), na.rm = T),
                   windDir.mean = mean(windDir, na.rm = T),
                   windDir.median = median(windDir, na.rm = T),
                   windDir.IQR = IQR(windDir, na.rm = T),
                   windDir.max = max(windDir, na.rm=T),
                   windDir.min = min(windDir, na.rm=T),
                   windDir.SD = sd(windDir, na.rm=T),
                   windDir.change.sd = sd(diff(windDir, lag=1), na.rm = T),
                   windDir.change.avg = mean(diff(windDir, lag=1), na.rm = T),
                   windSp.mean = mean(windSp, na.rm = T),
                   windSp.median = median(windSp, na.rm = T),
                   windSp.IQR = IQR(windSp, na.rm = T),
                   windSp.max = max(windSp, na.rm=T),
                   windSp.min = min(windSp, na.rm=T),
                   windSp.SD = sd(windSp, na.rm=T),
                   windSp.change.sd = sd(diff(windSp, lag=1), na.rm = T),
                   windSp.change.avg = mean(diff(windSp, lag=1), na.rm = T)) %>%
  as.data.frame() %>%
  dplyr::mutate(rain = as.factor(rain),
                month = factor(as.character(month)),
                week = factor(as.character(week)),
                season = factor(as.character(season)))

#### KEEP OUT 10% of the data for testing 
totalDays <- unique(data.by.hour$date)

set.seed(13434)
testDays <- sample(totalDays, size = length(totalDays) * .10)

##### NEVER TO BE SEEN TEST DATA
TEST.data.by.day <- data.by.day %>% filter(date %in% testDays)
TEST.data.by.hour <- data.by.hour %>% filter(date %in% testDays)
TEST.data.by.tod  <- data.by.tod %>% filter(date %in% testDays)

### Remaining Training and Validation
data.by.day  <- data.by.day %>% filter(!date %in% testDays)
data.by.hour <- data.by.hour %>% filter(!date %in% testDays)
data.by.tod  <- data.by.tod %>% filter(!date %in% testDays)


nrow(data.by.day)
nrow(data.by.hour)
nrow(data.by.tod)
