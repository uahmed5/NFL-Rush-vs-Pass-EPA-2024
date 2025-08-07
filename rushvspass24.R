library(tidyverse)
library(nflverse)
library(nflreadr)
library(nflfastR)
library(ggplot2)
library(nflplotR)

team_pass_epa <-pbp_24 %>%
  filter(!is.na(epa) & play_type == "pass") %>%
  group_by(posteam) %>%
  summarise(avg_pass_epa = mean(epa, na.rm = TRUE), 
            total_pass_epa = sum(epa, na.rm = TRUE), 
            plays = n()) %>%
  arrange(-avg_pass_epa)

team_rush_epa <-pbp_24 %>%
  filter(!is.na(epa) & play_type == "run") %>%
  group_by(posteam) %>%
  summarise(avg_rush_epa = mean(epa, na.rm = TRUE), 
            total_rush_epa = sum(epa, na.rm = TRUE), 
            plays = n()) %>%
  arrange(-avg_rush_epa)

total_epa <- team_pass_epa %>%
  left_join(team_rush_epa, by = "posteam") %>%
  left_join(teams_info %>% select(team_abbr, team_logo_espn), 
            by = c("posteam" = "team_abbr"))

ggplot(total_epa, aes(x = avg_rush_epa, y = avg_pass_epa)) +
  geom_hline(yintercept = mean(total_epa$avg_pass_epa), linetype = "dashed") +
  geom_vline(xintercept = mean(total_epa$avg_rush_epa), linetype = "dashed") +
  geom_smooth(method = "lm", color = "red") +
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.05) +
  labs(title = "Team Rushing Efficiency vs Passing Efficiency (2024)",
       x = "Rush EPA/play", y = "Pass EPA/play") +
  theme_minimal()