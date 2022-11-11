library(tidyverse)
library(ggplot2)
library(mverse)
# Define mutate branches.
hurricane_strength <- mutate_branch(
  # damage vs. wind speed vs.pressure
  NDAM,
  HighestWindSpeed,
  Minpressure_Updated_2014,
  # Standardized versions
  scale(NDAM),
  scale(HighestWindSpeed),
  -scale(Minpressure_Updated_2014),
)
y <- mutate_branch(
  alldeaths, log(alldeaths + 1)
)
mv <- create_multiverse(hurricane) %>%
  add_mutate_branch(hurricane_strength, y)
# Define a filter branch.
hurricane_outliers <- filter_branch(
  ! Name %in% c("Katrina", "Audrey", "Andrew"),
  ! Name %in% c("Katrina"),
  ! Name %in% c("Katrina"),
  TRUE # include all
)
# Define a formula branch.
model_specifications <- formula_branch(
  y ~ femininity,
  y ~ femininity + hurricane_strength,
  y ~ femininity * hurricane_strength
)
# Define a family branch.
model_distributions <- family_branch(gaussian, poisson)
# Create a mverse, add the branches, and fit lm models.
mv <- create_multiverse(hurricane) %>%
  add_filter_branch(hurricane_outliers) %>%
  add_mutate_branch(hurricane_strength, y) %>%
  add_formula_branch(model_specifications) %>%
  add_family_branch(model_distributions) %>%
  glm_mverse()
# Display the multiverse table with estimated coefficients.
summary(mv)




#######################


hdata <- hurricane %>%
  mutate(
    z_min_pressure = -scale(Minpressure_Updated_2014),
    z_max_wind_speed = scale(HighestWindSpeed),
    z_damage = scale(NDAM)
  ) %>%
  rename(
    deaths = alldeaths,
    femininity = MasFem,
    name = Name,
    year = Year
  ) %>%
  select(
    name, deaths, femininity, year,
    z_min_pressure, z_max_wind_speed, z_damage
  )

# 1. original: remove top 2 deaths
data_1 <- hdata %>%
  arrange(-deaths) %>%
  slice(-c(1,2))
fit_1 <- MASS::glm.nb(
  deaths ~ z_min_pressure * femininity + z_damage * femininity,
  data = data_1, control = glm.control(maxit = 100))
coef_1 <- summary(fit_1)$coef

# 2. no outlier
data_2 <- hdata
fit_2 <- MASS::glm.nb(
  deaths ~ z_min_pressure * femininity + z_damage * femininity,
  data = data_2, control = glm.control(maxit = 100))
coef_2 <- summary(fit_2)$coef

# 3. one outlier
data_3 <- hdata %>%
  arrange(-deaths) %>%
  slice(-1)
fit_3 <- MASS::glm.nb(
  deaths ~ z_min_pressure * femininity + z_damage * femininity,
  data = data_3, control = glm.control(maxit = 100))
coef_3 <- summary(fit_3)$coef

# 4. one outlier based on damage
data_4 <- hdata %>%
  arrange(-z_damage) %>%
  slice(-1)
fit_4 <- MASS::glm.nb(
  deaths ~ z_min_pressure * femininity + z_damage * femininity,
  data = data_4, control = glm.control(maxit = 100))
coef_4 <- summary(fit_4)$coef



do.call(rbind, list(
  cbind(coef_1,c(outlier = "Remove 2 Highest Death Tolls")),
  coef_2 %>% mutate(outlier = "Don't Remove Any"),
  coef_3 %>% mutate(outlier = "Remove 1 Highest Death Tolls"),
  coef_4 %>% mutate(outlier = "Remove 1 Highest Financial Damage")
))


##
hdata %>%
  filter((femininity > 6)  != (femininity > mean(femininity))) %>%
  select(name)
hurricane$Minpressure_Updated_2014

#################

# Define mutate branches.
hurricane_strength <- mutate_branch(
# damage vs. wind speed vs.pressure
NDAM,
HighestWindSpeed,
Minpressure_Updated_2014,
# Standardized versions
scale(NDAM),
scale(HighestWindSpeed),
-scale(Minpressure_Updated_2014),
)
y <- mutate_branch(
alldeaths, log(alldeaths + 1)
)
# Create a mverse and add the branches.
mv <- create_multiverse(hurricane) %>%
 add_mutate_branch(hurricane_strength, y)
execute_multiverse(mv)
# Extract all branched columns from all universes
extract(mv)
# Specify the columns to extract from each universe using \code{columns}
# You can select both branched and non-branched columns
extract(mv, columns = c("hurricane_strength", "NDAM"))
# Specify the universe to extract from using \code{universe}
extract(mv, universe = 1)
# Specify the number of universes to extract from using \code{nuni}
# The universes are randomly selected
extract(mv, nuni = 3)
# Specify the proportion of data to extract from each universe using \code{frow}
# The rows are randomly selected
extract(mv, frow = 0.7)
