# Factores usando Forcats

# Librerias
library(tidyverse)

dias <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")
levels <- c(1:7)

dias_factores <- as_factor(dias)
dias_factores


# Encuesta anual 

View(gss_cat)
glimpse(gss_cat)


# Analisis

gss_cat %>%
  count(Total = as_factor(year))

gss_cat %>%
  count(relig)

gss_cat %>%
  filter(relig == c("Protestant",
                    "Christian",
                    "Catholic",
                    "Jewish",
                    "Orthodox-christian")) %>%
  group_by(race, relig, partyid) %>%
  summarise(Total = n()) %>% View()


gss_cat %>%
  group_by(relig) %>%
  summarise(meanTVHours = mean(x = tvhours, na.rm=T),
            meanAge = mean(age, na.rm = T),
            n = n()) -> religion_summary

religion_summary %>%
  mutate(relig = fct_reorder(relig, meanTVHours)) -> religion_summary

View(religion_summary)

ggplot(religion_summary, aes(meanTVHours, relig)) +
         geom_bar(stat = "identity")


gss_cat %>%
  group_by(rincome) %>%
  summarise(meanAge = mean(age, na.rm = T),
            meanTVHours = mean(tvhours, na.rm = T),
            n = n()) %>%
  mutate(rincome = fct_relevel(rincome, "Not applicable")) -> income_summary

View(income_summary)


ggplot(income_summary, aes(meanAge, rincome)) +
  geom_bar(stat = "identity")


gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() -> by_age

ggplot(by_age, aes(age, n, color = fct_reorder2(marital, age, n))) +
  geom_line(na.rm = T) +
  labs(color = "Marital")


gss_cat %>%
  mutate(marital = marital %>% fct_infreq()) %>%
  ggplot(aes(marital)) +
  geom_bar()


# Uso de fct_lump para recategorizar por grupos mayoritarios

gss_cat %>%
  mutate(relig = fct_lump(f = relig, n = 5)) %>%
  count(relig, sort = T)
