train <- tar_read(train)
valid <- tar_read(valid)
test <- tar_read(test)

total <- bind_rows(train, valid, test)

total %>% 
  group_by(vin) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = n)) +
  geom_bar(col = "white", fill = "black") +
  scale_x_continuous(breaks = 1:10) +
  xlab("Number of contracts") +
  ylab("Number of vehicles") #+
  # ggtitle("Number of contracts per vehicle")
