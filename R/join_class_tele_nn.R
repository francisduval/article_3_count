join_class_tele_nn <- function(DatasetCount) {
  DatasetCount$classic_ml_data %>% 
    left_join(DatasetCount$tele_ml_data, by = c("vin", "contract_start_date", "nb_claims")) %>% 
    left_join(DatasetCount$nn_data, by = c("vin", "contract_start_date", "nb_claims"))
}
