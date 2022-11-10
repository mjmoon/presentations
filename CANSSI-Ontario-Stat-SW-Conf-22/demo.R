library(tidyverse)
library(mverse)
data(soccer)
soccer <- soccer |>
  select(playerShort, rater1, rater2, redCards, games,
         position, weight, height, leagueCountry) |>
  filter(!if_any(.fns = is.na)) |>
  mutate(
    skintone = (rater1 + rater2) * 2 + 1,
    redCards_raw = redCards,
    redCards = cbind(redCards, games - redCards)
  )
## tidy --------
# create position alternatives
soccer <- mutate(
  soccer,
  # fielder vs goalkeeper
  position_fld_vs_gk = if_else(position == "Goalkeeper",
                               "Goalkeeper", "Fielder"),
  # grouping into 4 groups
  position_group = forcats::fct_collapse(
    position,
    Forward = c("Center Forward", "Right Winger", "Left Winger"),
    Midfielder = c("Defensive Midfielder", "Attacking Midfielder",
                   "Right Midfielder", "Center Midfielder", "Left Midfielder"),
    Defender = c("Center Back", "Right Fullback", "Left Fullback"),
    Goalkeeper = c("Goalkeeper")
  )
)
# specify variable inclusion and exclusion
model_1 <- formula(redCards ~ skintone)
model_2 <- formula(redCards ~ skintone + position)
model_3 <- formula(redCards ~ skintone + position_fld_vs_gk)
model_4 <- formula(redCards ~ skintone + position_group)

model_5 <- formula(redCards ~ skintone + weight)
model_6 <- formula(redCards ~ skintone + position + weight)
model_7 <- formula(redCards ~ skintone + position_fld_vs_gk + weight)
model_8 <- formula(redCards ~ skintone + position_group + weight)

model_9 <- formula(redCards ~ skintone + height)
model_10 <- formula(redCards ~ skintone + position + height)
model_11 <- formula(redCards ~ skintone + position_fld_vs_gk + height)
model_12 <- formula(redCards ~ skintone + position_group + height)

model_13 <- formula(redCards ~ skintone + weight + height)
model_14 <- formula(redCards ~ skintone + position + weight + height)
model_15 <- formula(redCards ~ skintone + position_fld_vs_gk + weight + height)
model_16 <- formula(redCards ~ skintone + position_group + weight + height)

model_17 <- formula(redCards ~ skintone + leagueCountry)
model_18 <- formula(redCards ~ skintone + position + leagueCountry)
model_19 <- formula(redCards ~ skintone + position_fld_vs_gk + leagueCountry)
model_20 <- formula(redCards ~ skintone + position_group + leagueCountry)

model_21 <- formula(redCards ~ skintone + weight + leagueCountry)
model_22 <- formula(redCards ~ skintone + position + weight + leagueCountry)
model_23 <- formula(redCards ~ skintone + position_fld_vs_gk + weight + leagueCountry)
model_24 <- formula(redCards ~ skintone + position_group + weight + leagueCountry)

model_25 <- formula(redCards ~ skintone + height + leagueCountry)
model_26 <- formula(redCards ~ skintone + position + height + leagueCountry)
model_27 <- formula(redCards ~ skintone + position_fld_vs_gk + height + leagueCountry)
model_28 <- formula(redCards ~ skintone + position_group + height + leagueCountry)

model_29 <- formula(redCards ~ skintone + weight + height + leagueCountry)
model_30 <- formula(redCards ~ skintone + position + weight + height + leagueCountry)
model_31 <- formula(redCards ~ skintone + position_fld_vs_gk + weight + height + leagueCountry)
model_32 <- formula(redCards ~ skintone + position_group + weight + height + leagueCountry)

# bring the multiverse together and fit
models <- list(
  model_1, model_2, model_3, model_4,
  model_5, model_6, model_7, model_8,
  model_9, model_10, model_11, model_12,
  model_13, model_14, model_15, model_16,
  model_17, model_18, model_19, model_20,
  model_21, model_22, model_23, model_24,
  model_25, model_26, model_27, model_28,
  model_29, model_30, model_31, model_32
)
multiverse_fit <- lapply(
  models, glm, family = binomial, data = soccer)

## mverse -------------
# create position alternatives
position_alternative <- mutate_branch(
  as_is = position, # use position as is
  # fielder vs goalkeeper
  position_fld_vs_gk = if_else(position == "Goalkeeper",
                               "Goalkeeper", "Fielder"),
  # grouping into 4 groups
  `Fielder Grouping` = forcats::fct_collapse(
    position,
    Forward = c("Center Forward", "Right Winger", "Left Winger"),
    Midfielder = c("Defensive Midfielder", "Attacking Midfielder",
                   "Right Midfielder", "Center Midfielder", 
                   "Left Midfielder"),
    Defender = c("Center Back", "Right Fullback", "Left Fullback"),
    Goalkeeper = c("Goalkeeper")
  )
)

# create size alternatives
size <- mutate_branch(height, weight)

# specify variable inclusion and exclusion
frmls <- formula_branch(
  `No Covariate` = redCards ~ skintone,
  `Position` = redCards ~ skintone + position_alternative,
  `Position + Size` = redCards ~ skintone + position_alternative + size,
  `Position + Weight + Height` = redCards ~ skintone + position_alternative + weight + height,
  `Size` = redCards ~ skintone + size,
  `Weight + Height` = redCards ~ skintone + weight + height,
  `Country` = redCards ~ skintone + leagueCountry,
  `Position + Country` = redCards ~ skintone + position_alternative + leagueCountry,
  `Position + Size + Country` = redCards ~ skintone + position_alternative + size + leagueCountry,
  `Position + Weight + Height + Country` = redCards ~ skintone + position_alternative + weight + height + leagueCountry,
  `Size + Country` = redCards ~ skintone + size + leagueCountry,
  `Weight + Height + Country` = redCards ~ skintone + weight + height + leagueCountry,
)
# remove unnecessary branch combinations
condition_position_1 <- branch_condition(
  redCards ~ skintone, position)
condition_position_2 <- branch_condition(
  redCards ~ skintone + size, position)
condition_position_3 <- branch_condition(
  redCards ~ skintone + size + leagueCountry, position)
condition_position_4 <- branch_condition(
  redCards ~ skintone + leagueCountry, position)
condition_position_5 <- branch_condition(
  redCards ~ skintone + weight + height, position)
condition_position_6 <- branch_condition(
  redCards ~ skintone + weight + height + leagueCountry, position)

# bring the multiverse together and fit
mv <- mverse(soccer) %>%
  add_mutate_branch(position_alternative, size) %>%
  add_formula_branch(frmls) %>%
  add_family_branch(
    family_branch(logistic = binomial, name = "fam")
  ) %>%
  add_branch_condition(
    condition_position_1, condition_position_2, condition_position_3,
    condition_position_4, condition_position_5, condition_position_6
  )
glm_mverse(mv)
extract(
  mv, 
  columns = c("universe", "skintone", "position_alternative", 
              "redCards_raw", "games")
) |>
  write_csv("extract.csv")

# multiverse_tree(mv)
multiverse_tree(
  mv, branches = c("frmls", "position_alternative"),
  label = "name", label_size = 4
  ) +
  ggraph::scale_edge_colour_brewer(
    palette = "Dark2", labels = c("Model", "Position"))

spec_curve(mv, "skintone", branch_order = frmls_branch)
