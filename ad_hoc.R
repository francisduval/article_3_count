train <- tar_read(train)

rec <- 
  recipe(nb_claims ~ ., data = select(train, nb_claims:distance)) %>%
  step_impute_median(commute_distance, years_claim_free) %>%
  step_other(all_nominal(), threshold = 0.05) %>%
  step_dummy(all_nominal()) %>% 
  step_normalize()  

spec <- poisson_reg(engine = "glm")
wf <- workflow() |> add_recipe(rec) %>% add_model(spec)
fit <- parsnip::fit(wf, data = train)

extract_fit_parsnip(fit) %>% vip(num_features = 100, include_type = T) + geom_col(col = "black", fill = "white")
extract_fit_parsnip(fit) %>% vip::vi()
extract_fit_parsnip(fit) %>% vip::vi() %>% vip()
