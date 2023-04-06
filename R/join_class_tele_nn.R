join_class_tele_nn <- function(class_df, tele_df, nn_df) {
  class_df %>% 
    left_join(tele_df, by = c("vin", "contract_start_date")) %>% 
    left_join(nn_df, by = c("vin", "contract_start_date"))
}
