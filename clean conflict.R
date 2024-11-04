library(dplyr)
library(here)
here()
conflict <- read.csv(here("original", "conflictdata.csv"), header = TRUE)

conflict %>%
  # Outcome 1: Binary indicator of armed conflict (0 if <25, 1 if >= 25 battle related deaths) for each country-year
  group_by(ISO, year) |>
  summarise(totdeath = sum(best)) |>
  mutate(armconf1 = ifelse(totdeath < 25, 0, 1)) |>
  ungroup() |>
  mutate(year = year + 1) -> clean_conflict
# Outcome 2: Conflict intensity as categorical variable (0 = no, <25 battle-related deaths; 1 = minor conflict, 25-999 battle related deaths; 2 = war, >=1000 battle related deaths)
# mutate(confint = ifelse(totdeath < 25, 0, ifelse(totdeath < 1000, 1, 2)),
#        armconf2 = max(confint)) %>%
# dplyr::select(-confint, -best, -conflict_new_id) %>%
# slice(1) %>%
# ungroup -> clean_conflict
table(clean_conflict$armconf1)
head(clean_conflict)