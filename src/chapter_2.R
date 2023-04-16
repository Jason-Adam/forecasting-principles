library(fpp3)

olympic_running %>% distinct(Sex)

a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6)

autoplot(a10, Cost) +
  labs(y = "$ (millions)", title = "Australian Antidiabetic Drug Sales")

vic_elec %>%
  gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MW", title = "Electricity Demand: Victoria")

a10 %>%
  gg_subseries(Cost) +
  labs(y = "$ (millions)", title = "Australian Antidiabetic Drug Sales")

holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

autoplot(holidays, Trips) +
  labs(y = "Overnight Trips", title = "Australian Domestic Holidays")

holidays %>%
  gg_season(Trips)

holidays %>%
  gg_subseries(Trips)

vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  labs(y = "GW", title = "Half-hourly Electricity Demand: Victoria")

vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  labs(y = "Degrees Celsius", title = "Half-hourly Temperatures: Victoria")

vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temp (Celsius)", y = "Electricity Demand (GW)")

cor(vic_elec["Temperature"], vic_elec["Demand"])
