train <- tar_read(train)
valid <- tar_read(valid)
test <- tar_read(test)

total <- bind_rows(train, valid, test)

total %>% 
  ggplot(aes(x = expo)) +
  geom_histogram(col = "white", fill = "black") +
  xlab("xxx") +
  ylab("yyy") +
  ggtitle("Contracts duration")
