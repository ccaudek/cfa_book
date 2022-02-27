
library(tidyverse)
library(rstanarm)

Outcomes <-
  tibble(
    outcome = c("rain", "no rain"),
    prob = c(0.6, 0.4)
  )
Outcomes

Actions <-
  tibble(action = c("umbrella", "no umbrella"))
Actions

Losses <-
  expand.grid(
    action = Actions$action,
    outcome = Outcomes$outcome
  ) %>%
  inner_join(Outcomes) %>%
  mutate(loss = c(2, 4, 2, 0))
Losses

Utility <-
  Losses %>%
  mutate(conditional_loss = prob * loss) %>%
  group_by(action) %>%
  summarise(expected_loss = sum(conditional_loss))
Utility

