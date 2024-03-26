# First Set ----

## Preparation ----

# Load libraries ----
library(dplyr)
library(lubridate)
library(tidyr)
library(hms)

# import multiple data frames into one list with original names
path_to_data <- "Fitabase Data 4.12.16-5.12.16/"
list_of_files <- list.files(path_to_data, full.names = TRUE)
fitbit_data <- lapply(list_of_files, read.csv)
fitbit_data <- setNames(fitbit_data, tools::file_path_sans_ext(basename(list_of_files)))

# substitute names in fitbit_data to remove "_merged"
names(fitbit_data) <- sub("_merged$", "", names(fitbit_data))

# make all titles lowercase and add underscores
names(fitbit_data) <- tolower(names(fitbit_data))
fitbit_data$daily_activity <- fitbit_data$dailyactivity
fitbit_data$minute_sleep <- fitbit_data$minutesleep
fitbit_data$sleep_day <- fitbit_data$sleepday
fitbit_data$weight_log_info <- fitbit_data$weightloginfo

# removes data frames with repeated data and data frames with old titles
fitbit_data$dailycalories <- NULL
fitbit_data$dailyintensities <- NULL
fitbit_data$dailysteps <- NULL
fitbit_data$dailyactivity <- NULL
fitbit_data$minutesleep <- NULL
fitbit_data$sleepday <- NULL
fitbit_data$weightloginfo <- NULL

# combines all hourly data frames into one data frame
hourly_activity <- merge(fitbit_data$hourlycalories, fitbit_data$hourlyintensities)
hourly_activity <- merge(hourly_activity, fitbit_data$hourlysteps)
fitbit_data[["hourly_activity"]] <- hourly_activity
fitbit_data$hourlycalories <- NULL
fitbit_data$hourlyintensities <- NULL
fitbit_data$hourlysteps <- NULL
hourly_activity <- NULL

# combines all narrow minute data frames into one data frame
minute_activity_narrow <- merge(fitbit_data$minutecaloriesnarrow, fitbit_data$minuteintensitiesnarrow)
minute_activity_narrow <- merge(minute_activity_narrow, fitbit_data$minutemetsnarrow)
minute_activity_narrow <- merge(minute_activity_narrow, fitbit_data$minutestepsnarrow)
fitbit_data[["minute_activity_narrow"]] <- minute_activity_narrow
fitbit_data$minutecaloriesnarrow <- NULL
fitbit_data$minuteintensitiesnarrow <- NULL
fitbit_data$minutemetsnarrow <- NULL
fitbit_data$minutestepsnarrow <- NULL
minute_activity_narrow <- NULL

# combines all wide minute data frames into one data frame
minute_activity_wide <- merge(fitbit_data$minutecalorieswide, fitbit_data$minuteintensitieswide)
minute_activity_wide <- merge(minute_activity_wide, fitbit_data$minutestepswide)
fitbit_data[["minute_activity_wide"]] <- minute_activity_wide
fitbit_data$minutecalorieswide <- NULL
fitbit_data$minuteintensitieswide <- NULL
fitbit_data$minutestepswide <- NULL
minute_activity_wide <- NULL

# rename columns with lowercase and underscores
names(fitbit_data$daily_activity) <- c("id", "date", "total_steps", "total_distance",
                                      "tracker_distance", "logged_activities_distance",
                                      "very_active_distance", "moderately_active_distance",
                                      "light_active_distance", "sedentary_active_distance",
                                      "very_active_minutes", "fairly_active_minutes",
                                      "lightly_active_minutes", "sedentary_minutes",
                                      "calories")

names(fitbit_data$heartrate_seconds) <- c("id", "date", "value")

names(fitbit_data$minute_sleep) <- c("id", "date", "value", "log_id")

names(fitbit_data$sleep_day) <- c("id", "date", "total_sleep_records",
                                 "total_minutes_asleep", "total_time_in_bed")

names(fitbit_data$weight_log_info) <- c("id", "date", "weight_kg", "weight_pounds", 
                                        "fat", "bmi", "is_manual_report", "log_id")

names(fitbit_data$hourly_activity) <- c("id", "date", "calories", "total_intensity", 
                                        "average_intensity", "step_total")

names(fitbit_data$minute_activity_narrow) <- c("id", "date", "calories", "intensity",
                                               "mets", "steps")

names(fitbit_data$minute_activity_wide) <- c("id", "date", "calories_00", "calories_01", 
                                             "calories_02", "calories_03", "calories_04", 
                                             "calories_05", "calories_06", "calories_07", 
                                             "calories_08", "calories_09", "calories_10", 
                                             "calories_11", "calories_12", "calories_13", 
                                             "calories_14", "calories_15", "calories_16", 
                                             "calories_17", "calories_18", "calories_19", 
                                             "calories_20", "calories_21", "calories_22", 
                                             "calories_23", "calories_24", "calories_25", 
                                             "calories_26", "calories_27", "calories_28", 
                                             "calories_29", "calories_30", "calories_31", 
                                             "calories_32", "calories_33", "calories_34", 
                                             "calories_35", "calories_36", "calories_37", 
                                             "calories_38", "calories_39", "calories_40", 
                                             "calories_41", "calories_42", "calories_43", 
                                             "calories_44", "calories_45", "calories_46", 
                                             "calories_47", "calories_48", "calories_49", 
                                             "calories_50", "calories_51", "calories_52", 
                                             "calories_53", "calories_54", "calories_55", 
                                             "calories_56", "calories_57", "calories_58", 
                                             "calories_59", "intensity_00", "intensity_01", 
                                             "intensity_02", "intensity_03", "intensity_04", 
                                             "intensity_05", "intensity_06", "intensity_07", 
                                             "intensity_08", "intensity_09", "intensity_10", 
                                             "intensity_11", "intensity_12", "intensity_13", 
                                             "intensity_14", "intensity_15", "intensity_16", 
                                             "intensity_17", "intensity_18", "intensity_19", 
                                             "intensity_20", "intensity_21", "intensity_22", 
                                             "intensity_23", "intensity_24", "intensity_25", 
                                             "intensity_26", "intensity_27", "intensity_28", 
                                             "intensity_29", "intensity_30", "intensity_31", 
                                             "intensity_32", "intensity_33", "intensity_34", 
                                             "intensity_35", "intensity_36", "intensity_37", 
                                             "intensity_38", "intensity_39", "intensity_40", 
                                             "intensity_41", "intensity_42", "intensity_43", 
                                             "intensity_44", "intensity_45", "intensity_46", 
                                             "intensity_47", "intensity_48", "intensity_49", 
                                             "intensity_50", "intensity_51", "intensity_52", 
                                             "intensity_53", "intensity_54", "intensity_55", 
                                             "intensity_56", "intensity_57", "intensity_58", 
                                             "intensity_59", "steps_00", "steps_01", 
                                             "steps_02", "steps_03", "steps_04", 
                                             "steps_05", "steps_06", "steps_07", 
                                             "steps_08", "steps_09", "steps_10", 
                                             "steps_11", "steps_12", "steps_13", 
                                             "steps_14", "steps_15", "steps_16", 
                                             "steps_17", "steps_18", "steps_19", 
                                             "steps_20", "steps_21", "steps_22", 
                                             "steps_23", "steps_24", "steps_25", 
                                             "steps_26", "steps_27", "steps_28", 
                                             "steps_29", "steps_30", "steps_31", 
                                             "steps_32", "steps_33", "steps_34", 
                                             "steps_35", "steps_36", "steps_37", 
                                             "steps_38", "steps_39", "steps_40", 
                                             "steps_41", "steps_42", "steps_43", 
                                             "steps_44", "steps_45", "steps_46", 
                                             "steps_47", "steps_48", "steps_49", 
                                             "steps_50", "steps_51", "steps_52", 
                                             "steps_53", "steps_54", "steps_55", 
                                             "steps_56", "steps_57", "steps_58", 
                                             "steps_59")

# remove values from global environment
rm(hourly_activity)
rm(minute_activity_narrow)
rm(minute_activity_wide)

# create a function to replace ids to ease readability
id_replace <- function(column_name) {
  sub(1503960366, 1, sub(1624580081, 2, sub(1644430081, 3, sub(1844505072, 4, 
  sub(1927972279, 5, sub(2022484408, 6, sub(2026352035, 7, sub(2320127002, 8, 
  sub(2347167796, 9, sub(2873212765, 10, sub(3372868164, 11, sub(3977333714, 12, 
  sub(4020332650, 13, sub(4057192912, 14, sub(4319703577, 15, sub(4388161847, 16, 
  sub(4445114986, 17, sub(4558609924, 18, sub(4702921684, 19, sub(5553957443, 20, 
  sub(5577150313, 21, sub(6117666160, 22, sub(6290855005, 23, sub(6775888955, 24, 
  sub(6962181067, 25, sub(7007744171, 26, sub(7086361926, 27, sub(8053475328, 28, 
  sub(8253242879, 29, sub(8378563200, 30, sub(8583815059, 31, sub(8792009665, 32, 
  sub(8877689391, 33, column_name)))))))))))))))))))))))))))))))))
}

# execute id_replace function on each data frame
fitbit_data$daily_activity$id <- id_replace(fitbit_data$daily_activity$id)
fitbit_data$heartrate_seconds$id <- id_replace(fitbit_data$heartrate_seconds$id)
fitbit_data$minute_sleep$id <- id_replace(fitbit_data$minute_sleep$id)
fitbit_data$sleep_day$id <- id_replace(fitbit_data$sleep_day$id)
fitbit_data$weight_log_info$id <- id_replace(fitbit_data$weight_log_info$id)
fitbit_data$hourly_activity$id <- id_replace(fitbit_data$hourly_activity$id)
fitbit_data$minute_activity_narrow$id <- id_replace(fitbit_data$minute_activity_narrow$id)
fitbit_data$minute_activity_wide$id <- id_replace(fitbit_data$minute_activity_wide$id)

## Organization ----

# separates dates and times into two columns
fitbit_data$minute_sleep <- 
  mutate(separate(fitbit_data$minute_sleep, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data$heartrate_seconds <- 
  mutate(separate(fitbit_data$heartrate_seconds, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data$sleep_day <- 
  mutate(separate(fitbit_data$sleep_day, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data$weight_log_info <- 
  mutate(separate(fitbit_data$weight_log_info, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data$hourly_activity <- 
  mutate(separate(fitbit_data$hourly_activity, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data$minute_activity_narrow <- 
  mutate(separate(fitbit_data$minute_activity_narrow, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data$minute_activity_wide <- 
  mutate(separate(fitbit_data$minute_activity_wide, date, c("date", "time", "am_pm"), sep = " "))

# convert dates to weekdays, then add as a separate column to each data frame
fitbit_data$daily_activity <- 
  mutate(fitbit_data$daily_activity, dotw = weekdays(
    as.Date(fitbit_data$daily_activity$date, '%m/%d/%Y')))

fitbit_data$daily_activity <- fitbit_data$daily_activity %>% 
  relocate(dotw, .after = date)

fitbit_data$heartrate_seconds <- 
  mutate(fitbit_data$heartrate_seconds, dotw = weekdays(
    as.Date(fitbit_data$heartrate_seconds$date, '%m/%d/%Y')))

fitbit_data$heartrate_seconds <- fitbit_data$heartrate_seconds %>% 
  relocate(dotw, .after = date)

fitbit_data$minute_sleep <- 
  mutate(fitbit_data$minute_sleep, dotw = weekdays(
    as.Date(fitbit_data$minute_sleep$date, '%m/%d/%Y')))

fitbit_data$minute_sleep <- fitbit_data$minute_sleep %>% 
  relocate(dotw, .after = date)

fitbit_data$sleep_day <- 
  mutate(fitbit_data$sleep_day, dotw = weekdays(
    as.Date(fitbit_data$sleep_day$date, '%m/%d/%Y')))

fitbit_data$sleep_day <- fitbit_data$sleep_day %>% 
  relocate(dotw, .after = date)

fitbit_data$weight_log_info <- 
  mutate(fitbit_data$weight_log_info, dotw = weekdays(
    as.Date(fitbit_data$weight_log_info$date, '%m/%d/%Y')))

fitbit_data$weight_log_info <- fitbit_data$weight_log_info %>% 
  relocate(dotw, .after = date)

fitbit_data$hourly_activity <- 
  mutate(fitbit_data$hourly_activity, dotw = weekdays(
    as.Date(fitbit_data$hourly_activity$date, '%m/%d/%Y')))

fitbit_data$hourly_activity <- fitbit_data$hourly_activity %>% 
  relocate(dotw, .after = date)

fitbit_data$minute_activity_narrow <- 
  mutate(fitbit_data$minute_activity_narrow, dotw = weekdays(
    as.Date(fitbit_data$minute_activity_narrow$date, '%m/%d/%Y')))

fitbit_data$minute_activity_narrow <- fitbit_data$minute_activity_narrow %>% 
  relocate(dotw, .after = date)

fitbit_data$minute_activity_wide <- 
  mutate(fitbit_data$minute_activity_wide, dotw = weekdays(
    as.Date(fitbit_data$minute_activity_wide$date, '%m/%d/%Y')))

fitbit_data$minute_activity_wide <- fitbit_data$minute_activity_wide %>% 
  relocate(dotw, .after = date)

# fix data types
fitbit_data$heartrate_seconds$id <- as.integer(fitbit_data$heartrate_seconds$id)
fitbit_data$heartrate_seconds$date <- as.Date(fitbit_data$heartrate_seconds$date, '%m/%d/%Y')
fitbit_data$heartrate_seconds$time <- as_hms(fitbit_data$heartrate_seconds$time)

fitbit_data$daily_activity$id <- as.integer(fitbit_data$daily_activity$id)
fitbit_data$daily_activity$date <- as.Date(fitbit_data$daily_activity$date, '%m/%d/%Y')

fitbit_data$minute_sleep$id <- as.integer(fitbit_data$minute_sleep$id)
fitbit_data$minute_sleep$date <- as.Date(fitbit_data$minute_sleep$date, '%m/%d/%Y')
fitbit_data$minute_sleep$time <- as_hms(fitbit_data$minute_sleep$time)

fitbit_data$sleep_day$id <- as.integer(fitbit_data$sleep_day$id)
fitbit_data$sleep_day$date <- as.Date(fitbit_data$sleep_day$date, '%m/%d/%Y')
fitbit_data$sleep_day$time <- as_hms(fitbit_data$sleep_day$time)

fitbit_data$weight_log_info$id <- as.integer(fitbit_data$weight_log_info$id)
fitbit_data$weight_log_info$date <- as.Date(fitbit_data$weight_log_info$date, '%m/%d/%Y')
fitbit_data$weight_log_info$time <- as_hms(fitbit_data$weight_log_info$time)
fitbit_data$weight_log_info$is_manual_report <- as.logical(fitbit_data$weight_log_info$is_manual_report)

fitbit_data$hourly_activity$id <- as.integer(fitbit_data$hourly_activity$id)
fitbit_data$hourly_activity$date <- as.Date(fitbit_data$hourly_activity$date, '%m/%d/%Y')
fitbit_data$hourly_activity$time <- as_hms(fitbit_data$hourly_activity$time)

fitbit_data$minute_activity_narrow$id <- as.integer(fitbit_data$minute_activity_narrow$id)
fitbit_data$minute_activity_narrow$date <- as.Date(fitbit_data$minute_activity_narrow$date, '%m/%d/%Y')
fitbit_data$minute_activity_narrow$time <- as_hms(fitbit_data$minute_activity_narrow$time)

fitbit_data$minute_activity_wide$id <- as.integer(fitbit_data$minute_activity_wide$id)
fitbit_data$minute_activity_wide$date <- as.Date(fitbit_data$minute_activity_wide$date, '%m/%d/%Y')
fitbit_data$minute_activity_wide$time <- as_hms(fitbit_data$minute_activity_wide$time)

# remove unnecessary columns
fitbit_data$minute_sleep$log_id <- NULL
fitbit_data$weight_log_info$log_id <- NULL

# create folder for files to export to
dir.create("Fitabase_Data_4.12.16-5.12.16_Cleaned")

# export data frames to csv files
for(framename in names(fitbit_data)) {
  write.csv(fitbit_data[[framename]], paste0("Fitabase_Data_4.12.16-5.12.16_Cleaned/", 
                                             framename, ".csv"), row.names = FALSE)
}


# Second Set ----

## Preparation ----

# import multiple data frames into one list with original names
path_to_data2 <- "Fitabase Data 3.12.16-4.11.16/"
list_of_files2 <- list.files(path_to_data2, full.names = TRUE)
fitbit_data2 <- lapply(list_of_files2, read.csv)
fitbit_data2 <- setNames(fitbit_data2, tools::file_path_sans_ext(basename(list_of_files2)))

# substitute names in fitbit_data to remove "_merged"
names(fitbit_data2) <- sub("_merged$", "", names(fitbit_data2))

# make all titles lowercase and add underscores
names(fitbit_data2) <- tolower(names(fitbit_data2))
fitbit_data2$daily_activity <- fitbit_data2$dailyactivity
fitbit_data2$minute_sleep <- fitbit_data2$minutesleep
fitbit_data2$weight_log_info <- fitbit_data2$weightloginfo

# removes data frames with repeated data and data frames with old titles
fitbit_data2$dailycalories <- NULL
fitbit_data2$dailyintensities <- NULL
fitbit_data2$dailysteps <- NULL
fitbit_data2$dailyactivity <- NULL
fitbit_data2$minutesleep <- NULL
fitbit_data2$weightloginfo <- NULL

# combines all hourly data frames into one data frame
hourly_activity <- merge(fitbit_data2$hourlycalories, fitbit_data2$hourlyintensities)
hourly_activity <- merge(hourly_activity, fitbit_data2$hourlysteps)
fitbit_data2[["hourly_activity"]] <- hourly_activity
fitbit_data2$hourlycalories <- NULL
fitbit_data2$hourlyintensities <- NULL
fitbit_data2$hourlysteps <- NULL
hourly_activity <- NULL

# combines all narrow minute data frames into one data frame
minute_activity_narrow <- merge(fitbit_data2$minutecaloriesnarrow, fitbit_data2$minuteintensitiesnarrow)
minute_activity_narrow <- merge(minute_activity_narrow, fitbit_data2$minutemetsnarrow)
minute_activity_narrow <- merge(minute_activity_narrow, fitbit_data2$minutestepsnarrow)
fitbit_data2[["minute_activity_narrow"]] <- minute_activity_narrow
fitbit_data2$minutecaloriesnarrow <- NULL
fitbit_data2$minuteintensitiesnarrow <- NULL
fitbit_data2$minutemetsnarrow <- NULL
fitbit_data2$minutestepsnarrow <- NULL
minute_activity_narrow <- NULL

# rename columns with lowercase and underscores
names(fitbit_data2$daily_activity) <- c("id", "date", "total_steps", "total_distance", 
                                        "tracker_distance", "logged_activities_distance", 
                                        "very_active_distance", "moderately_active_distance", 
                                        "light_active_distance", "sedentary_active_distance", 
                                        "very_active_minutes", "fairly_active_minutes", 
                                        "lightly_active_minutes", "sedentary_minutes", 
                                        "calories")

names(fitbit_data2$heartrate_seconds) <- c("id", "date", "value")

names(fitbit_data2$minute_sleep) <- c("id", "date", "value", "log_id")

names(fitbit_data2$weight_log_info) <- c("id", "date", "weight_kg", "weight_pounds", 
                                         "fat", "bmi", "is_manual_report", "log_id")

names(fitbit_data2$hourly_activity) <- c("id", "date", "calories", "total_intensity", 
                                         "average_intensity", "step_total")

names(fitbit_data2$minute_activity_narrow) <- c("id", "date", "calories", "intensity", 
                                                "mets", "steps")

# remove values from global environment
rm(hourly_activity)
rm(minute_activity_narrow)

# create a function to replace ids to ease readability
id_replace <- function(column_name) {
  sub(1503960366, 1, sub(1624580081, 2, sub(1644430081, 3, sub(1844505072, 4,  
  sub(1927972279, 5, sub(2022484408, 6, sub(2026352035, 7, sub(2320127002, 8, 
  sub(2347167796, 9, sub(2873212765, 10, sub(3372868164, 11, sub(3977333714, 12, 
  sub(4020332650, 13, sub(4057192912, 14, sub(4319703577, 15, sub(4388161847, 16, 
  sub(4445114986, 17, sub(4558609924, 18, sub(4702921684, 19, sub(5553957443, 20, 
  sub(5577150313, 21, sub(6117666160, 22, sub(6290855005, 23, sub(6775888955, 24, 
  sub(6962181067, 25, sub(7007744171, 26, sub(7086361926, 27, sub(8053475328, 28, 
  sub(8253242879, 29, sub(8378563200, 30, sub(8583815059, 31, sub(8792009665, 32, 
  sub(8877689391, 33, sub(2891001357, 34, sub(6391747486, 35, column_name)))))))))))))))))))))))))))))))))))
}

# execute id_replace function on each data frame
fitbit_data2$daily_activity$id <- id_replace(fitbit_data2$daily_activity$id)
fitbit_data2$heartrate_seconds$id <- id_replace(fitbit_data2$heartrate_seconds$id)
fitbit_data2$minute_sleep$id <- id_replace(fitbit_data2$minute_sleep$id)
fitbit_data2$weight_log_info$id <- id_replace(fitbit_data2$weight_log_info$id)
fitbit_data2$hourly_activity$id <- id_replace(fitbit_data2$hourly_activity$id)
fitbit_data2$minute_activity_narrow$id <- id_replace(fitbit_data2$minute_activity_narrow$id)

## Organization ----

# separates dates and times into two columns
fitbit_data2$minute_sleep <- 
  mutate(separate(fitbit_data2$minute_sleep, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data2$heartrate_seconds <- 
  mutate(separate(fitbit_data2$heartrate_seconds, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data2$weight_log_info <- 
  mutate(separate(fitbit_data2$weight_log_info, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data2$hourly_activity <- 
  mutate(separate(fitbit_data2$hourly_activity, date, c("date", "time", "am_pm"), sep = " "))

fitbit_data2$minute_activity_narrow <- 
  mutate(separate(fitbit_data2$minute_activity_narrow, date, c("date", "time", "am_pm"), sep = " "))

# convert dates to weekdays, then add as a separate column to each data frame
fitbit_data2$daily_activity <- 
  mutate(fitbit_data2$daily_activity, dotw = weekdays(as.Date(fitbit_data2$daily_activity$date, '%m/%d/%Y')))

fitbit_data2$daily_activity <- fitbit_data2$daily_activity %>% 
  relocate(dotw, .after = date)

fitbit_data2$heartrate_seconds <- 
  mutate(fitbit_data2$heartrate_seconds, dotw = weekdays(as.Date(fitbit_data2$heartrate_seconds$date, '%m/%d/%Y')))

fitbit_data2$heartrate_seconds <- fitbit_data2$heartrate_seconds %>% 
  relocate(dotw, .after = date)

fitbit_data2$minute_sleep <- 
  mutate(fitbit_data2$minute_sleep, dotw = weekdays(as.Date(fitbit_data2$minute_sleep$date, '%m/%d/%Y')))

fitbit_data2$minute_sleep <- fitbit_data2$minute_sleep %>% 
  relocate(dotw, .after = date)

fitbit_data2$weight_log_info <- 
  mutate(fitbit_data2$weight_log_info, dotw = weekdays(as.Date(fitbit_data2$weight_log_info$date, '%m/%d/%Y')))

fitbit_data2$weight_log_info <- fitbit_data2$weight_log_info %>% 
  relocate(dotw, .after = date)

fitbit_data2$hourly_activity <- 
  mutate(fitbit_data2$hourly_activity, dotw = weekdays(as.Date(fitbit_data2$hourly_activity$date, '%m/%d/%Y')))

fitbit_data2$hourly_activity <- fitbit_data2$hourly_activity %>% 
  relocate(dotw, .after = date)

fitbit_data2$minute_activity_narrow <- 
  mutate(fitbit_data2$minute_activity_narrow, dotw = weekdays(as.Date(fitbit_data2$minute_activity_narrow$date, '%m/%d/%Y')))

fitbit_data2$minute_activity_narrow <- fitbit_data2$minute_activity_narrow %>% 
  relocate(dotw, .after = date)

# fix data types
fitbit_data2$heartrate_seconds$id <- as.integer(fitbit_data2$heartrate_seconds$id)
fitbit_data2$heartrate_seconds$date <- as.Date(fitbit_data2$heartrate_seconds$date, '%m/%d/%Y')
fitbit_data2$heartrate_seconds$time <- as_hms(fitbit_data2$heartrate_seconds$time)

fitbit_data2$daily_activity$id <- as.integer(fitbit_data2$daily_activity$id)
fitbit_data2$daily_activity$date <- as.Date(fitbit_data2$daily_activity$date, '%m/%d/%Y')

fitbit_data2$minute_sleep$id <- as.integer(fitbit_data2$minute_sleep$id)
fitbit_data2$minute_sleep$date <- as.Date(fitbit_data2$minute_sleep$date, '%m/%d/%Y')
fitbit_data2$minute_sleep$time <- as_hms(fitbit_data2$minute_sleep$time)

fitbit_data2$weight_log_info$id <- as.integer(fitbit_data2$weight_log_info$id)
fitbit_data2$weight_log_info$date <- as.Date(fitbit_data2$weight_log_info$date, '%m/%d/%Y')
fitbit_data2$weight_log_info$time <- as_hms(fitbit_data2$weight_log_info$time)
fitbit_data2$weight_log_info$is_manual_report <- as.logical(fitbit_data2$weight_log_info$is_manual_report)

fitbit_data2$hourly_activity$id <- as.integer(fitbit_data2$hourly_activity$id)
fitbit_data2$hourly_activity$date <- as.Date(fitbit_data2$hourly_activity$date, '%m/%d/%Y')
fitbit_data2$hourly_activity$time <- as_hms(fitbit_data2$hourly_activity$time)

fitbit_data2$minute_activity_narrow$id <- as.integer(fitbit_data2$minute_activity_narrow$id)
fitbit_data2$minute_activity_narrow$date <- as.Date(fitbit_data2$minute_activity_narrow$date, '%m/%d/%Y')
fitbit_data2$minute_activity_narrow$time <- as_hms(fitbit_data2$minute_activity_narrow$time)

# remove unnecessary columns
fitbit_data2$minute_sleep$log_id <- NULL
fitbit_data2$weight_log_info$log_id <- NULL

# create folder for files
dir.create("Fitabase_Data_3.12.16-4.11.16_Cleaned")

# export data frames to csv files
for(framename in names(fitbit_data2)) {
  write.csv(fitbit_data2[[framename]], paste0("Fitabase_Data_3.12.16-4.11.16_Cleaned/", framename, ".csv"), row.names = FALSE)
}

# Merging Sets ----

# merges data frames on full join, and converts single frames to variables
daily_activity <- full_join(fitbit_data2$daily_activity, fitbit_data$daily_activity)
heartrate_seconds <- full_join(fitbit_data2$heartrate_seconds, fitbit_data$heartrate_seconds)
hourly_activity <- full_join(fitbit_data2$hourly_activity, fitbit_data$hourly_activity)
minute_activity_narrow <- full_join(fitbit_data2$minute_activity_narrow, fitbit_data$minute_activity_narrow)
minute_sleep <- full_join(fitbit_data2$minute_sleep, fitbit_data$minute_sleep)
sleep_day <- fitbit_data$sleep_day
weight_log_info <- full_join(fitbit_data2$weight_log_info, fitbit_data$weight_log_info)
minute_activity_wide <- fitbit_data$minute_activity_wide
sleep_day <- fitbit_data$sleep_day

# creates new list from new variables
fitbit_data_merged <- list(daily_activity, heartrate_seconds, hourly_activity, 
                           minute_activity_narrow, minute_activity_wide, minute_sleep, 
                           sleep_day, weight_log_info)
fitbit_data_merged <- setNames(fitbit_data_merged, 
                               c("daily_activity", "heartrate_seconds", "hourly_activity", 
                                 "minute_activity_narrow", "minute_activity_wide", 
                                 "minute_sleep", "sleep_day", "weight_log_info"))

# removes clutter from global environment
rm(daily_activity)
rm(heartrate_seconds)
rm(hourly_activity)
rm(minute_activity_narrow)
rm(minute_activity_wide)
rm(minute_sleep)
rm(sleep_day)
rm(weight_log_info)

# creates folder for new files
dir.create("Fitabase_Data_3.12.16-5.12.16_Cleaned")

# exports new csv files into the new folder
for(framename in names(fitbit_data_merged)) {
  write.csv(fitbit_data_merged[[framename]], paste0("Fitabase_Data_3.12.16-5.12.16_Cleaned/", 
                                                    framename, ".csv"), row.names = FALSE)
}