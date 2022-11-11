# 1
pass <- c("ok", "dubious", "men")
test_data_1 <- movies %>%
  mutate(
    group = if_else(clean_test %in% pass, 1, 0),
    metric = as.numeric(domgross_2013) + as.numeric(intgross_2013) -
      as.numeric(budget_2013)) %>%
  filter(!is.na(metric))

# 2
pass <- c("ok", "dubious")
test_data_2 <- movies %>%
  mutate(
    group = if_else(clean_test %in% pass, 1, 0),
    metric = as.numeric(domgross_2013) + as.numeric(intgross_2013) -
      as.numeric(budget_2013)) %>%
  filter(!is.na(metric))

# 3
pass <- c("ok")
test_data_3 <- movies %>%
  mutate(
    group = if_else(clean_test %in% pass, 1, 0),
    metric = as.numeric(domgross_2013) + as.numeric(intgross_2013) -
      as.numeric(budget_2013)) %>%
  filter(!is.na(metric))

# 1a
pass <- c("ok", "dubious", "men")
test_data_1a <- movies %>%
  mutate(
    group = if_else(clean_test %in% pass, 1, 0),
    metric = (as.numeric(domgross_2013) + as.numeric(intgross_2013)) /
      as.numeric(budget_2013)) %>%
  filter(!is.na(metric))

# 2a
pass <- c("ok", "dubious")
test_data_2a <- movies %>%
  mutate(
    group = if_else(clean_test %in% pass, 1, 0),
    metric = (as.numeric(domgross_2013) + as.numeric(intgross_2013)) /
      as.numeric(budget_2013)) %>%
  filter(!is.na(metric))

# 3a
pass <- c("ok")
test_data_3a <- movies %>%
  mutate(
    group = if_else(clean_test %in% pass, 1, 0),
    metric = (as.numeric(domgross_2013) + as.numeric(intgross_2013)) /
      as.numeric(budget_2013)) %>%
  filter(!is.na(metric))

t.test(formula = metric ~ group, data = test_data_1)
t.test(formula = metric ~ group, data = test_data_2)
t.test(formula = metric ~ group, data = test_data_3)
t.test(formula = metric ~ group, data = test_data_1a)
t.test(formula = metric ~ group, data = test_data_2a)
t.test(formula = metric ~ group, data = test_data_3a)
