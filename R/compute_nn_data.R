compute_nn_data <- function(aug_trip_data) {
  
  # -----
  
  compute_h_cols <- function(data) {
    compute_hour_vec <- function(time_start_vec, time_end_vec) {
      seconds_vec <- vector(mode = "numeric", length = 86400)
      
      for(i in seq_along(time_start_vec)) {
        ind <- seq(as.numeric(time_start_vec[i]), as.numeric(time_end_vec[i]))
        seconds_vec[ind] <- seconds_vec[ind] + 1
      }
      
      hours_vec <- map_dbl(1:24, ~ sum(seconds_vec[(3600 * (.x - 1) + 1):(3600 * .x)]))
      
      return(hours_vec / sum(hours_vec))
    }
    
    h_cols <- 
      data %>%
      group_by(vin, contract_start_date) %>%
      summarise(time_cols = list(compute_hour_vec(time_start, time_end))) %>%
      ungroup() %>% 
      unnest(cols = time_cols) %>%
      mutate(names = rep(glue("h_{1:24}"), nrow(.) / 24)) %>%
      pivot_wider(names_from = names, values_from = time_cols)
    
    return(h_cols)
  }
  
  # -----
  
  compute_p_cols <- function(data) {
    compute_days_vec <- function(datetime_start_vec, datetime_end_vec) {
      weekday_vec_start <- wday(datetime_start_vec, week_start = 1)
      weekday_vec_end <- wday(datetime_end_vec, week_start = 1)
      
      secs_since_midnight_vec_start <- as.numeric(as_hms(datetime_start_vec))
      secs_since_midnight_vec_end <- as.numeric(as_hms(datetime_end_vec))
      
      sec_since_monday_vec_start <- (weekday_vec_start - 1) * 86400 + secs_since_midnight_vec_start
      sec_since_monday_vec_end <- (weekday_vec_end - 1) * 86400 + secs_since_midnight_vec_end
      
      seconds_vec <- vector(mode = "numeric", length = 604800)
      
      for(i in seq_along(datetime_start_vec)) {
        ind <- seq(sec_since_monday_vec_start[i], sec_since_monday_vec_end[i])
        seconds_vec[ind] <- seconds_vec[ind] + 1
      }
      
      days_vec <- map_dbl(1:7, ~ sum(seconds_vec[(86400 * (.x - 1) + 1):(86400 * .x)]))
      
      return(days_vec / sum(days_vec))
    }
    
    res <- 
      data %>%
      group_by(vin, contract_start_date) %>%
      summarise(days_cols = list(compute_days_vec(datetime_start, datetime_end))) %>%
      ungroup() %>% 
      unnest(cols = days_cols) %>%
      mutate(names = rep(glue("p_{1:7}"), nrow(.) / 7)) %>%
      pivot_wider(names_from = names, values_from = days_cols)
    
    return(res)
  }
  
  # -----
  
  compute_speed_buckets <- function(speed_vec, duration_vec, nb_buckets) {
    bucket_vec <- vector(mode = "numeric", length = nb_buckets)
    
    for(i in seq(1, nb_buckets)) {
      bucket_vec[i] <- sum(duration_vec[speed_vec > 10 * (i - 1) & speed_vec <= 10 * i])
    }
    
    bucket_vec[nb_buckets] <- bucket_vec[nb_buckets] + sum(duration_vec[speed_vec > 10 * nb_buckets])
    
    return(bucket_vec / sum(bucket_vec))
  }
  
  # -----
  
  compute_distance_buckets <- function(distance_vec, nb_buckets) {
    bucket_vec <- vector(mode = "numeric", length = nb_buckets)
    
    for(i in seq(1, nb_buckets)) {
      bucket_vec[i] <- sum(distance_vec > 5 * (i - 1) & distance_vec <= 5 * i)
    }
    
    bucket_vec[nb_buckets] <- bucket_vec[nb_buckets] + sum(distance_vec > 5 * nb_buckets)
    
    return(bucket_vec / sum(bucket_vec))
  }
  
  # -----
  
  compute_vmo_cols <- function(data, nb_buckets = 13) {
    res <- 
      data %>%
      group_by(vin, contract_start_date) %>%
      summarise(vmo_cols = list(compute_speed_buckets(avg_speed, duration, nb_buckets))) %>%
      ungroup() %>% 
      unnest(cols = vmo_cols) %>%
      mutate(names = rep(glue("vmo_{1:nb_buckets}"), nrow(.) / nb_buckets)) %>%
      pivot_wider(names_from = names, values_from = vmo_cols)
    
    return(res)
  }
  
  # -----
  
  compute_vma_cols <- function(data, nb_buckets = 16) {
    res <- 
      data %>%
      group_by(vin, contract_start_date) %>%
      summarise(vma_cols = list(compute_speed_buckets(max_speed, duration, nb_buckets))) %>%
      ungroup() %>% 
      unnest(cols = vma_cols) %>%
      mutate(names = rep(glue("vma_{1:nb_buckets}"), nrow(.) / nb_buckets)) %>%
      pivot_wider(names_from = names, values_from = vma_cols)
    
    return(res)
  }
  
  # -----
  
  compute_d_cols <- function(data, nb_buckets = 10) {
    res <- 
      data %>%
      group_by(vin, contract_start_date) %>%
      summarise(d_cols = list(compute_distance_buckets(distance, nb_buckets))) %>%
      ungroup() %>% 
      unnest(cols = d_cols) %>%
      mutate(names = rep(glue("d_{1:nb_buckets}"), nrow(.) / nb_buckets)) %>%
      pivot_wider(names_from = names, values_from = d_cols)
    
    return(res)
  }
  
  # -----
  
  h_dat <- compute_h_cols(aug_trip_data)
  p_dat <- compute_p_cols(aug_trip_data)
  vmo_dat <- compute_vmo_cols(aug_trip_data)
  vma_dat <- compute_vma_cols(aug_trip_data)
  dist_dat <- compute_d_cols(aug_trip_data)
  
  res <- 
    h_dat%>% 
    left_join(p_dat, by = c("vin", "contract_start_date")) %>% 
    left_join(vmo_dat, by = c("vin", "contract_start_date")) %>% 
    left_join(vma_dat, by = c("vin", "contract_start_date")) %>% 
    left_join(dist_dat, by = c("vin", "contract_start_date"))
  
  # -----
  
  return(res)
}