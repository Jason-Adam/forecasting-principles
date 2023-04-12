library(fpp3)

olympic_running %>% distinct(Sex)

a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6)

autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian Antidiabetic Drug Sales")