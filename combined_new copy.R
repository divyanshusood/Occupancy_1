library(eplusr)
library(purrr)
library(data.table)
library(lubridate)
library(hms)

read_sch_data <- function(path, file_name, na = 0.0) {
  # Extract month and weekday from the file name
  parts <- strsplit(file_name, "_")[[1]]
  month <- as.integer(gsub("month", "", parts[3]))
  weekday <- gsub("day", "", parts[4])
  
  # Check the total line count of the file
  total_lines <- length(readLines(path))
  
  # Proceed only if there are more than 4 lines
  if (total_lines > 4) {
    data <- fread(path, skip = 4)
    data[, DiaryDay_Act := as_hms(DiaryDay_Act)]
    setnafill(data, type = "const", fill = na, cols = 2L)
    interval <- diff(data$DiaryDay_Act[1:2]) / 60
    
    list(month = month, num = NA, type = NA, weekday = weekday, interval = interval, data = data)
  } else {
    # Handle the case where the file does not have enough lines
    # This could involve skipping the file, logging a warning, or another appropriate action
    warning(sprintf("File '%s' skipped: not enough lines.", path))
    NULL  # Return NULL or appropriate default structure
  }
}

create_sch_weekly <- function(idf, name, list_days) {
  WEEKDAY <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" )
  # stop if any weekday data is missing
  if (any(mis_wd <- !WEEKDAY %in% names(list_days))) {
    stop("Missing schedule for weekday [", paste(WEEKDAY[mis_wd], collapse = ", "), "]")
  }
  
  # create a "AlwaysOff" for weekends, design days and other days
  # adjust this as you need
  if (!idf$is_valid_class("Schedule:Day:Interval") ||
      !idf$is_valid_name("AlwaysOff", "Schedule:Day:Interval")) {
    idf$add(Schedule_Day_Interval = list("AlwaysOff", "On/Off", "No", "Until: 24:00", 0))
    
  }
  # create a daily schedule
  idf$add(
    Schedule_Week_Daily = list(
      name,
      list_days$Sunday$name(),
      list_days$Monday$name(),
      list_days$Tuesday$name(),
      list_days$Wednesday$name(),
      list_days$Thursday$name(),
      list_days$Friday$name(),
      list_days$Saturday$name(),
      "AlwaysOff",
      "AlwaysOff",
      "AlwaysOff",
      "AlwaysOff",
      "AlwaysOff"
    )
  )[[1L]]
}

create_sch_yearly <- function(idf, name, list_months,  type_limits = "Any number") {
  # get the start and end day for each month
  start <- lubridate::ymd("2022-01-01") + months(0:11)
  end <- lubridate::rollforward(start)
  
  # combine start day, end day and month schedules
  data <- data.table::data.table(
    index = seq(12),
    start = start, end = end,
    sch = purrr::map(list_months, ~.$name())
  )
  
  # get the start and end day field value
  data[, `:=`(
    start_month = as.list(lubridate::month(start)),
    start_day = as.list(lubridate::mday(start)),
    end_month = as.list(lubridate::month(end)),
    end_day = as.list(lubridate::mday(end))
  )]
  
  # remove original start and end column since not needed
  data[, `:=`(start = NULL, end = NULL)]
  
  # melt the data based on index column
  m <- data.table::melt.data.table(data, "index")
  data.table::setorderv(m, "index")
  
  idf$add(
    Schedule_Year = as.list(c(name, type_limits, m$value))
  )[[1L]]
}


create_schedules <- function(idf, room_type, file_path, new_names) {
  # Correct pattern for file names including month and day
  path_csv <- list.files(file_path, pattern = "accom_apt_month(1[0-2]|[1-9])_day(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)\\.csv$", full.names = TRUE, recursive = TRUE)
  
  # Read and process each CSV file, discarding any that result in NULL (i.e., have insufficient lines)
  sch_data <- map(path_csv, function(path) {
    file_name <- basename(path)
    read_sch_data(path, file_name, na = 0)
  }) %>% discard(is.null)
  
  # Process each day's schedule data
  sch_daily <- map(sch_data, function(data) {
    sch_name <- sprintf("%s_%s_%s_%s", data$type, month.abb[data$month], data$weekday, room_type)
    sch <- idf$add(Schedule_Day_List = list(sch_name, "On/Off", "No", data$interval))[[1]]
    sch$set(c(sch$value(1:4), data$data[[data$weekday]]))
    sch
  })
  
  # Organize data for monthly and weekly schedules
  d <- data.table(
    index = seq_along(path_csv),
    month_num = map_int(sch_data, `[[`, "month"),
    new_name = new_names[map_int(sch_data, `[[`, "month")],
    month = month.name[map_int(sch_data, `[[`, "month")],
    weekday = map_chr(sch_data, `[[`, "weekday"),
    sch_daily = sch_daily
  )
  
  # Aggregate daily schedules into monthly schedules
  d_month <- d[, .(sch_monthly = list(create_sch_weekly(idf, new_name, sch_daily))), by = .(new_name)]
  
  # Debug: Check if all weekdays are present before calling create_sch_weekly
  d[, .(weekdays = list(unique(weekday))), by = .(new_name)][, print(.SD), by = .(new_name)]
  
  # Proceed with creating yearly schedule
  create_sch_yearly(idf, paste("Yearly", room_type, sep = "_"), d_month$sch_monthly)
}


# Initialize IDF
idf <- read_idf("Dublin_Apartment_4occ.idf")

# Names for different room types
new_names_household <- c("jan_house", "feb_house", "mar_house", "april_house", "may_house", "june_house", 
                         "july_house", "aug_house","sept_house", "oct_house", "nov_house", "dec_house")

# Paths for different room types
path_household <- "~/Documents/RQ-3_journal/DeterministicAnalysis/newtrial/1occ/house"

# Create schedules for each room type
create_schedules(idf, "house", path_household, new_names_household)


# Save the final IDF
idf_save_path <- "~/Documents/RQ-3_journal/DeterministicAnalysis/EnergyPlus_Models/Apartment/Apartment_all_idf/Dublin_Apartment_4occ_final.idf"
idf$save(path = file.path(idf_save_path), copy_external = TRUE)
