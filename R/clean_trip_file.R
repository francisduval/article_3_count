clean_trip_file <- function(trip_file) {
  read_csv(trip_file, col_type = cols()) %>% 
    lazy_dt() %>%
    transmute(
      trip_number = as.character(TRIP_NUMBER),
      vin = as.character(ENROLLED_VIN),
      datetime_start = parse_date_time2(LOCAL_TRIP_START_TIMESTAMP, "%d%b%Y:%H:%M:%OS"),
      datetime_end = parse_date_time2(LOCAL_TRIP_END_TIMESTAMP, "%d%b%Y:%H:%M:%OS"),
      duration = as.numeric(datetime_end - datetime_start),
      date_start = date(datetime_start),
      date_end = date(datetime_end),
      time_start = as_hms(datetime_start),
      time_end = as_hms(datetime_end),
      distance = as.numeric(VSS_DISTANCE),
      avg_speed = as.numeric(VSS_AVG_SPEED),
      max_speed = as.numeric(VSS_MAX_SPEED)
    ) %>%
    filter(
      distance > 0,
      avg_speed > 0,
      max_speed > 0,
      duration > 0
    ) %>% 
    arrange(vin, datetime_start) %>% 
    as_tibble() %>% 
    na.omit()
}