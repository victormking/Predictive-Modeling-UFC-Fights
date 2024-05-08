library(tidyverse)
library(readxl)
library(caret)

# Load Data
ufc_dataset <- read_csv("datasets(csv)/UFC_DataSet.csv")
new_ufc_dataset <- read_csv("datasets(csv)/2010-2020.csv")

# Convert Date Format
new_ufc_dataset <- new_ufc_dataset %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# Identify Relevant Fighters and Filter Original Dataset
relevant_fighters <- unique(c(new_ufc_dataset$R_fighter, new_ufc_dataset$B_fighter))
filtered_ufc <- ufc_dataset %>%
  filter(r_fighter %in% relevant_fighters | b_fighter %in% relevant_fighters)

# Join Datasets and Drop Missing Data
merged_ufc_clean <- filtered_ufc %>%
  left_join(
    new_ufc_dataset %>%
      select(R_fighter, B_fighter, date, R_odds, B_odds, location, country),
    by = c("r_fighter" = "R_fighter", "b_fighter" = "B_fighter"),
    relationship = "many-to-many"
  ) %>%
  drop_na(date, R_odds, B_odds, location, country)

# Calculate Total Fights for Each Fighter
count_total_fights <- function(fighter_name, current_date) {
  sum((merged_ufc_clean$r_fighter == fighter_name | merged_ufc_clean$b_fighter == fighter_name) & merged_ufc_clean$date < current_date)
}

merged_ufc_clean <- merged_ufc_clean %>%
  mutate(
    r_total_fights = mapply(count_total_fights, r_fighter, date),
    b_total_fights = mapply(count_total_fights, b_fighter, date),
    age_difference = abs(r_age - b_age),
    experience_diff = abs(r_total_fights - b_total_fights),
    reach_advantage_diff = r_reach - b_reach,
    winner_name = ifelse(winner == "Red", r_fighter, b_fighter)
  )

# Update Weight Classes
weight_class_mapping <- c(
  "Flyweight" = 125, "Bantamweight" = 135, "Featherweight" = 145,
  "Lightweight" = 155, "Welterweight" = 170, "Middleweight" = 185,
  "Light Heavyweight" = 205, "Heavyweight" = 265,
  "Women's Strawweight" = 115, "Women's Flyweight" = 125,
  "Women's Bantamweight" = 135, "Women's Featherweight" = 145,
  "Catchweight" = NA, "Super Heavyweight" = 266, "Openweight" = NA
)

# Clean Weight Class Names and Re-apply Numeric Mapping
clean_weight_class <- function(weight_class_name) {
  cleaned <- gsub("^(UFC |Ultimate Fighter.* )", "", weight_class_name) %>%
    gsub("Interim |Title| Tournament", "", .) %>%
    trimws()
  return(cleaned)
}

merged_ufc_clean <- merged_ufc_clean %>%
  mutate(
    weight_class = sapply(weight_class, clean_weight_class),
    weight_class_numeric = weight_class_mapping[weight_class]
  )

# Remove Rows with NA in Weight Class Numeric
merged_ufc_clean <- merged_ufc_clean %>%
  filter(!is.na(weight_class_numeric))

# Generate Profiles
create_detailed_profile <- function(fighter_name) {
  fights <- merged_ufc_clean[(merged_ufc_clean$r_fighter == fighter_name | merged_ufc_clean$b_fighter == fighter_name), ]
  wins <- sum(merged_ufc_clean$winner_name == fighter_name, na.rm = TRUE)
  losses <- nrow(fights) - wins
  total_strikes <- sum(fights$r_str[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_str[fights$b_fighter == fighter_name], na.rm = TRUE)
  takedown_accuracy <- mean(c(fights$r_td_acc[fights$r_fighter == fighter_name], fights$b_td_acc[fights$b_fighter == fighter_name]), na.rm = TRUE)
  control_time <- sum(fights$r_ctrl_sec[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_ctrl_sec[fights$b_fighter == fighter_name], na.rm = TRUE)
  submission_attempts <- sum(fights$r_sub_att[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_sub_att[fights$b_fighter == fighter_name], na.rm = TRUE)
  weight_class_numeric <- unique(fights$weight_class_numeric)
  experience_diff <- mean(fights$experience_diff, na.rm = TRUE)
  age_difference <- mean(fights$age_difference, na.rm = TRUE)
  r_odds <- mean(fights$R_odds, na.rm = TRUE)
  b_odds <- mean(fights$B_odds, na.rm = TRUE)
  fight_dates <- paste(unique(fights$date), collapse = ", ")
  
  return(data.frame(
    name = fighter_name, wins = wins, losses = losses, total_strikes = total_strikes,
    takedown_accuracy = takedown_accuracy, control_time = control_time,
    submission_attempts = submission_attempts, weight_class_numeric = weight_class_numeric,
    experience_diff = experience_diff, age_difference = age_difference, r_odds = r_odds,
    b_odds = b_odds, fight_dates = fight_dates
  ))
}

fighters <- unique(c(merged_ufc_clean$r_fighter, merged_ufc_clean$b_fighter))
fighter_profiles_detailed_df <- do.call(rbind, lapply(fighters, create_detailed_profile))

# Save Results
write_csv(merged_ufc_clean, "merged_ufc_clean_final.csv")
write_csv(fighter_profiles_detailed_df, "fighter_profiles_detailed_final.csv")
