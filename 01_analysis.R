

# Load and clean ------------------------------------------------------------------------------


library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(slider)


cases <- read.csv("COVID 19/ccaa_covid19_casos.csv", stringsAsFactors = F)

cases_lng <- cases %>% 
  pivot_longer(cols = X27.02.2020:X14.03.2020, names_to = "date", values_to = "cases") %>% 
  mutate(
    created_at = as.Date(str_sub(date, start = 2), format = "%d.%m.%Y")
  ) %>% 
  select(-date)


# Cases by ccaa -------------------------------------------------------------------------------

cases_lng %>% 
  filter(!CCAA %in% c("Total", "Madrid")) %>%
  # filter(!CCAA %in% c("Total")) %>%
  ggplot(aes(x = created_at, y = cases)) + 
  geom_line() + 
  facet_wrap(~ CCAA)

cases_lng$test <- cases_lng$created_at >= "2020-03-11"



# Features with slider ------------------------------------------------------------------------

# Define target
cases_lng <- cases_lng %>% mutate(target = log(cases))

cases_lng <- cases_lng %>% 
  group_by(CCAA) %>% 
  mutate(
    target_previous_2 = lag(target, 2),
    # 2 days vs yesterday
    ratio_new_cases_2 = slide_dbl(cases, ~.x[2]/.x[1], .before = 2, .after = -1),
    target_previous_3 = lag(target, 3),
    # 3 days vs yesterday
    ratio_new_cases_3 = slide_dbl(cases, ~.x[3]/.x[1], .before = 3, .after = -1)
  ) %>% 
  ungroup()


# Split train/test ----------------------------------------------------------------------------

# Delete ceuta and melilla, they have few cases
cases_lng <- cases_lng %>% filter(!CCAA %in% c("Ceuta", "Melilla"))
train_set <- cases_lng %>% filter(!test)
test_set <- cases_lng %>% filter(test)


# Train lm ------------------------------------------------------------------------------------

# Only train 
train_set <- train_set %>% filter(!is.na(ratio_new_cases_2), !is.infinite(ratio_new_cases_2))

lm_results <- glm(
  target ~ ratio_new_cases_2 + target_previous_2,
  data = train_set
  )

# Predict lm ----------------------------------------------------------------------------------

test_set$predictions <- unname(predict(lm_results, test_set))
train_set$predictions <- unname(predict(lm_results, train_set))

# Predictions are good!
test_set %>% 
  ggplot(aes(x = predictions, y = target)) + 
  geom_point() + 
  facet_wrap(~ CCAA)

# Error is pretty low
test_set %>% 
  filter(!is.infinite(predictions)) %>% 
  yardstick::mae(exp(predictions), exp(target))


# Create figures ------------------------------------------------------------------------------

full_set <- bind_rows(train_set, test_set)

full_set %>% 
  filter(!is.infinite(predictions)) %>% 
  filter(!CCAA %in% c("Total", "Madrid")) %>%
  # filter(!CCAA %in% c("Total")) %>%
  ggplot() + 
  geom_line(aes(x = created_at, y = cases, group = test, color = "Real")) + 
  geom_line(aes(x = created_at, y = exp(predictions), group = test, color = "Predictions")) + 
  facet_wrap(~ CCAA) + 
  ggsave("plots/predictions_all.png", width = 12)

full_set %>% 
  filter(!is.infinite(predictions)) %>% 
  filter(CCAA %in% c("Total", "Madrid")) %>%
  # filter(!CCAA %in% c("Total")) %>%
  ggplot() + 
  geom_line(aes(x = created_at, y = cases, group = test, color = "Real")) + 
  geom_line(aes(x = created_at, y = exp(predictions), group = test, color = "Predictions")) + 
  facet_wrap(~ CCAA) +
  ggsave("plots/predictions_mad.png", width = 12)


summary(lm_results)


full_set %>% 
  filter(!is.infinite(predictions)) %>% 
  filter(CCAA %in% c("Total", "Madrid")) %>% 
  mutate(exp(predictions)) %>% 
  View

# Predict next week ---------------------------------------------------------------------------


predict_next_step <- function(tbl, x) {

  new_row <- tbl %>% tail(x)
  new_row <- new_row %>% 
    mutate(
      created_at = max(tbl$created_at) + days(1),
      ratio_new_cases_2 = last(tbl$cases) / last(lag(tbl$cases, 1)),
      target_previous_2 = last(tbl$target)
    )
  
  new_row <- new_row %>% 
    mutate(
      target = unname(predict(lm_results, new_row)),
      cases = exp(target)
    )
  
  new_row
}

all_week <- full_set

for (i in 1:5) {
  next_prediction <- all_week %>% group_by(CCAA) %>% group_modify(~ predict_next_step(.x, 1))
  all_week <- all_week %>% bind_rows(
    next_prediction
  ) %>% arrange(CCAA, created_at)
  
}

all_week %>% View
# This is exponential explosion, probably not going to be like this!
