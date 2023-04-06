compute_tele_data <- function(aug_trip_data) {
  aug_trip_data %>% 
    mutate(
      weekday = weekdays(date_start, abbreviate = T),
      is_weekend = weekday %in% c("Sat", "Sun"),
      distance_night_trip = (time_start >= as_hms("00:00:00") & time_start < as_hms("06:00:00")) * distance,
      distance_noon_trip = (time_start >= as_hms("11:00:00") & time_start < as_hms("14:00:00")) * distance,
      distance_evening_trip = (time_start >= as_hms("20:00:00") & time_start <= as_hms("23:59:59")) * distance,
      distance_peak_morning_trip = ((time_start >= as_hms("07:00:00") & time_start < as_hms("09:00:00")) & !is_weekend) * distance,
      distance_peak_evening_trip = ((time_start >= as_hms("17:00:00") & time_start < as_hms("20:00:00")) & !is_weekend) * distance,
      distance_mon_to_thu = (weekday %in% c("Mon", "Tue", "Wed", "Thu")) * distance,
      distance_fri_sat = (weekday %in% c("Fri", "Sat")) * distance,
      distance_sun = (weekday == "Sun") * distance
    ) %>% 
    rename(
      trip_avg_speed = avg_speed,
      trip_distance = distance,
      trip_duration = duration,
      trip_max_speed = max_speed
    ) %>% 
    group_by(vin, contract_start_date) %>% 
    summarise(
      nb_trips                  = n(),
      avg_daily_distance        = sum(trip_distance) / 365.25,
      avg_daily_nb_trips        = n() / 365.25,
      med_trip_avg_speed        = median(trip_avg_speed),
      med_trip_distance         = median(trip_distance),
      med_trip_max_speed        = median(trip_max_speed),
      max_trip_max_speed        = max(trip_max_speed),
      prop_long_trip            = sum(trip_distance > 100) / n(),
      frac_expo_night           = sum(distance_night_trip) / sum(trip_distance),
      frac_expo_noon            = sum(distance_noon_trip) / sum(trip_distance),
      frac_expo_evening         = sum(distance_evening_trip) / sum(trip_distance),
      frac_expo_peak_morning    = sum(distance_peak_morning_trip) / sum(trip_distance),
      frac_expo_peak_evening    = sum(distance_peak_evening_trip) / sum(trip_distance),
      frac_expo_mon_to_thu      = sum(distance_mon_to_thu) / sum(trip_distance),
      frac_expo_fri_sat         = sum(distance_fri_sat) / sum(trip_distance)
    ) %>% 
    ungroup()
}
