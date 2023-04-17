# Libraries ----

library(shiny)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(plotly)
library(here)
library(httr)
library(jsonlite)

# Data ----
# Establish data tables

lec_data <- read_csv(here("data/lec_data.csv")) %>% 
  mutate(game_id = as.factor(game_id),
         date = as.Date(date),
         # kda needs to account for deaths being 0 to prevent infinity.
         # sadly prevents deathless games from being special... perhaps mutate in a column to flag them?
         kda = ((kills + assists) / if_else(deaths == 0, 1, deaths)),
         flawless = (deaths == 0)) %>% 
  relocate(kda, .after = assists) %>% 
  mutate(kda = round(kda, 2))

# create sub-tables for teams and players to reduce the need for repeated filtering
# (the aggregate rows for team stats have NA for player name, making them identifiable)

lec_data_teams <- lec_data %>% 
  filter(is.na(player_name))

lec_data_players <- lec_data %>% 
  filter(!is.na(player_name)) %>% 
  mutate(position = case_when(
    position == "top" ~ "Top",
    position == "mid" ~ "Middle",
    position == "bot" ~ "ADC",
    position == "sup" ~ "Support",
    position == "jng" ~ "Jungle",
    TRUE ~ position
  ))

# Table additions

lec_data_teams <- lec_data_teams %>% 
  group_by(split, team_name) %>% 
  filter(playoffs == FALSE) %>% 
  arrange(date) %>% 
  mutate(split_wins = cumsum(winner),
         split_losses = cumsum(!winner), .after = split) %>% 
  ungroup()

# Functions ----

get_roster <- function(x_team){
  
  lec_data %>% 
    filter(!is.na(player_name), team_name == x_team) %>% 
    select(position, player_name, game_id) %>% 
    group_by(player_name) %>% 
    summarise(position, games = n_distinct(game_id)) %>% 
    distinct() %>% 
    ungroup() %>% 
    select(position, player_name, games) %>% 
    arrange(factor(position, levels = c("top", "mid", "bot", "sup", "jng")), player_name)
}

get_matches <- function(x_team){
  
  lec_data %>% 
    select(game_id, team_name) %>% 
    group_by(game_id) %>%
    filter(team_name == x_team) %>% 
    select(game_id) %>% 
    distinct() %>% 
    pull()
}

get_game_history <- function(x_team){
  
  team_games <- get_matches(x_team)
  
  lec_data %>% 
    filter(game_id %in% team_games, team_name != x_team) %>%
    select(date, split, playoffs, game, side, team_name, winner) %>% 
    distinct() %>% 
    mutate(winner = !winner) %>% 
    arrange(date, game)
}

get_match_history <- function(x_team, match_data){
  
  get_game_history(x_team) %>% 
    select(date, split, playoffs, game, team_name, winner) %>% 
    mutate(date = as.Date(date)) %>% 
    group_by(date) %>%
    count(team_name, playoffs, split, winner) %>% 
    pivot_wider(names_from = winner, values_from = n) %>% 
    rename(wins = `TRUE`, losses = `FALSE`) %>% 
    mutate(wins = coalesce(wins, 0), losses = coalesce(losses, 0),
           winner = if_else(wins > losses, TRUE, FALSE))
}

# Variables ----
# Used in Headline containers

no_players <- length(unique(na.omit(lec_data$player_name)))

no_games <- length(unique(na.omit(lec_data$game_id)))

no_teams <- length(unique(na.omit(lec_data$team_name)))

total_champs <- length(unique(na.omit(lec_data$champion)))

total_kills <- lec_data_teams %>% 
  summarise(total_kills = sum(kills)) %>% 
  pull()

total_deaths <- lec_data_teams %>% 
  summarise(total_deaths = sum(deaths)) %>% 
  pull()

top5_picks <- lec_data_players %>% 
  select(champion) %>% 
  group_by(champion) %>% 
  summarise(times_picked = n()) %>% 
  slice_max(times_picked, n = 5)

most_picked <- top5_picks %>% 
  slice_max(times_picked) %>% 
  pull(var = champion)

top5_bans <- lec_data %>% 
  filter(is.na(player_name)) %>% 
  select(ban_1:ban_5) %>%
  pivot_longer(1:5, names_to = "ban_no", values_to = "champion") %>% 
  group_by(champion) %>% 
  summarise(times_banned = n()) %>% 
  slice_max(times_banned, n = 5)

most_banned <- top5_bans %>% 
  slice_max(times_banned) %>% 
  pull(var = champion)

url <- "https://raw.communitydragon.org/13.7/plugins/rcp-be-lol-game-data/global/default/v1/champion-icons/1.png"