imp_mvnb <- tar_read(imp_mvnb)

vars <- 
  imp_mvnb %>% 
  summarise_all(median) %>% 
  pivot_longer(cols = everything()) %>% 
  arrange(desc(value)) %>% 
  slice(1:20) %>%
  pull(name)

p <-
  imp_mvnb %>% 
  pivot_longer(cols = everything()) %>% 
  filter(name %in% vars) %>% 
  mutate(name = fct_reorder(name, value, .fun = "median")) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(outlier.size = 0.5) +
  coord_flip() +
  xlab(NULL) +
  ylab("Increase in average loss") +
  theme(text = element_text(size = 15))

ggsave("figures/imp.png", plot = p, width = 11)
