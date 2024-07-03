# Load required libraries
library(tidyverse)
library(readxl)
library(caret)

# Load the final cleaned dataset
merged_UFC_final <- read_csv("merged_UFC_final_cleaned.csv")

# Calculate total fights for each fighter, rounding the calculations
count_total_fights <- function(fighter_name, current_date) {
  sum((merged_UFC_final$r_fighter == fighter_name | merged_UFC_final$b_fighter == fighter_name) & merged_UFC_final$date < current_date)
}

# Update the dataset with calculated fields
merged_UFC_final <- merged_UFC_final %>%
  mutate(
    r_total_fights = mapply(count_total_fights, r_fighter, date),
    b_total_fights = mapply(count_total_fights, b_fighter, date),
    age_difference = round(abs(r_age - b_age), 2),
    experience_diff = round(abs(r_total_fights - b_total_fights), 2),
    reach_advantage_diff = round(r_reach - b_reach, 2),
    winner_name = ifelse(winner == "Red", r_fighter, b_fighter)
  )

# Update weight classes
weight_class_mapping <- c(
  "Flyweight" = 125, "Bantamweight" = 135, "Featherweight" = 145,
  "Lightweight" = 155, "Welterweight" = 170, "Middleweight" = 185,
  "Light Heavyweight" = 205, "Heavyweight" = 265,
  "Women's Strawweight" = 115, "Women's Flyweight" = 125,
  "Women's Bantamweight" = 135, "Women's Featherweight" = 145,
  "Catchweight" = NA, "Super Heavyweight" = 266, "Openweight" = NA
)

# Clean weight class names and re-apply numeric mapping
clean_weight_class <- function(weight_class_name) {
  cleaned <- gsub("^(UFC |Ultimate Fighter.* )", "", weight_class_name) %>%
    gsub("Interim |Title| Tournament", "", .) %>%
    trimws()
  return(cleaned)
}

merged_UFC_final <- merged_UFC_final %>%
  mutate(
    weight_class = sapply(weight_class, clean_weight_class),
    weight_class_numeric = weight_class_mapping[weight_class]
  )

# Remove rows with NA in weight class numeric
merged_UFC_final <- merged_UFC_final %>%
  filter(!is.na(weight_class_numeric))

# Generate profiles for each fighter, rounding numerical outputs
create_detailed_profile <- function(fighter_name) {
  fights <- merged_UFC_final[(merged_UFC_final$r_fighter == fighter_name | merged_UFC_final$b_fighter == fighter_name), ]
  wins <- sum(merged_UFC_final$winner_name == fighter_name, na.rm = TRUE)
  losses <- nrow(fights) - wins
  total_strikes <- sum(fights$r_str[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_str[fights$b_fighter == fighter_name], na.rm = TRUE)
  total_sig_str <- sum(fights$r_sig_str[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_sig_str[fights$b_fighter == fighter_name], na.rm = TRUE)
  total_sig_str_att <- sum(fights$r_sig_str_att[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_sig_str_att[fights$b_fighter == fighter_name], na.rm = TRUE)
  sig_str_accuracy <- round(total_sig_str / total_sig_str_att, 2)
  total_td <- sum(fights$r_td[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_td[fights$b_fighter == fighter_name], na.rm = TRUE)
  total_td_att <- sum(fights$r_td_att[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_td_att[fights$b_fighter == fighter_name], na.rm = TRUE)
  td_accuracy <- round(total_td / total_td_att, 2)
  total_kd <- sum(fights$r_kd[fights$r_fighter == fighter_name], na.rm = TRUE) +
    sum(fights$b_kd[fights$b_fighter == fighter_name], na.rm = TRUE)
  avg_fight_duration <- round(mean(fights$time_sec, na.rm = TRUE), 2)
  avg_sig_str <- round(total_sig_str / nrow(fights), 2)
  avg_sig_str_att <- round(total_sig_str_att / nrow(fights), 2)
  avg_td <- round(total_td / nrow(fights), 2)
  avg_td_att <- round(total_td_att / nrow(fights), 2)
  avg_sub_att <- round(mean(fights$r_sub_att[fights$r_fighter == fighter_name], na.rm = TRUE) + mean(fights$b_sub_att[fights$b_fighter == fighter_name], na.rm = TRUE), 2)
  avg_ctrl_time <- round(mean(fights$r_ctrl_sec[fights$r_fighter == fighter_name], na.rm = TRUE) + mean(fights$b_ctrl_sec[fights$b_fighter == fighter_name], na.rm = TRUE), 2)
  weight_class_numeric <- unique(fights$weight_class_numeric)
  experience_diff <- round(mean(fights$experience_diff, na.rm = TRUE), 2)
  age_difference <- round(mean(fights$age_difference, na.rm = TRUE), 2)
  r_odds <- round(mean(fights$R_odds, na.rm = TRUE), 2)
  b_odds <- round(mean(fights$B_odds, na.rm = TRUE), 2)
  
  return(data.frame(
    name = fighter_name, wins = wins, losses = losses, total_strikes = total_strikes,
    avg_sig_str = avg_sig_str, avg_sig_str_att = avg_sig_str_att, sig_str_accuracy = sig_str_accuracy,
    avg_td = avg_td, avg_td_att = avg_td_att, td_accuracy = td_accuracy,
    avg_sub_att = avg_sub_att, avg_ctrl_time = avg_ctrl_time,
    total_kd = total_kd, avg_fight_duration = avg_fight_duration,
    weight_class_numeric = weight_class_numeric, experience_diff = experience_diff,
    age_difference = age_difference, r_odds = r_odds, b_odds = b_odds
  ))
}

fighters <- unique(c(merged_UFC_final$r_fighter, merged_UFC_final$b_fighter))
fighter_profiles_detailed_df <- do.call(rbind, lapply(fighters, create_detailed_profile))

# Save intermediate results
write_csv(merged_UFC_final, "merged_UFC_final_with_calculations.csv")
write_csv(fighter_profiles_detailed_df, "fighter_profiles_detailed_final.csv")

# Aggregate the data to remove duplicates and combine multiple entries
cleaned_fighter_profiles <- fighter_profiles_detailed_df %>%
  group_by(name) %>%
  summarise(
    wins = max(wins),
    losses = max(losses),
    total_strikes = sum(total_strikes),
    avg_sig_str = round(mean(avg_sig_str, na.rm = TRUE), 2),
    avg_sig_str_att = round(mean(avg_sig_str_att, na.rm = TRUE), 2),
    sig_str_accuracy = round(mean(sig_str_accuracy, na.rm = TRUE), 2),
    avg_td = round(mean(avg_td, na.rm = TRUE), 2),
    avg_td_att = round(mean(avg_td_att, na.rm = TRUE), 2),
    td_accuracy = round(mean(td_accuracy, na.rm = TRUE), 2),
    avg_sub_att = round(mean(avg_sub_att, na.rm = TRUE), 2),
    avg_ctrl_time = round(mean(avg_ctrl_time, na.rm = TRUE), 2),
    total_kd = sum(total_kd),
    avg_fight_duration = round(mean(avg_fight_duration, na.rm = TRUE), 2),
    weight_class_numeric = first(weight_class_numeric), # Assuming weight class doesn't change
    experience_diff = max(experience_diff),
    age_difference = round(mean(age_difference, na.rm = TRUE), 2),
    r_odds = round(mean(r_odds, na.rm = TRUE), 2),
    b_odds = round(mean(b_odds, na.rm = TRUE), 2)
  )

# Save the cleaned data
write_csv(cleaned_fighter_profiles, "cleaned_fighter_profiles_expanded.csv")

# Reconfirm if any duplicates still exist
num_duplicates <- cleaned_fighter_profiles %>%
  filter(duplicated(name)) %>%
  nrow()

print(paste("Number of duplicates remaining:", num_duplicates))

if (num_duplicates == 0) {
  print("All duplicates have been successfully removed.")
} else {
  print("There are still some duplicates remaining.")
}

