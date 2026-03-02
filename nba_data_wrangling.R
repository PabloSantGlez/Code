library(pxR)
library(MASS)
library(shinydashboard)
library(datasets)
library(highcharter) 
library(fpp3)
library(RColorBrewer)
library(openxlsx)  
library(leaflet)  
library(geojsonio)
library(plotly)
library(ggplot2)
library(tidyverse)
library(splines)
 
source("mod_heatmap.R")
 
select <- dplyr::select
mutate <- dplyr::mutate
filter <- dplyr::filter
group_by <- dplyr::group_by
summarise <- dplyr::summarise
selectInput <- shiny::selectInput



 
rawData <- readr::read_csv('nbaplayersdraft.csv')
 

 
categories <- rawData %>% select(-id, -rank, -year, -team, -player, -overall_pick, -field_goal_percentage, -free_throw_percentage, -`3_point_percentage`, -box_plus_minus, -win_shares, -win_shares_per_48_minutes, -value_over_replacement) %>% colnames()
data <- rawData %>% 
  select(-id, -rank) %>%
  replace_na(list(college = "Didn't play College")) %>%
  mutate(across(categories, ~replace_na(.x, 0))) %>%
  pivot_longer(years_active:value_over_replacement, names_to = 'indicator', values_to = 'value')
 

 
data <- data %>% group_by(indicator) %>%
  mutate(normValue = (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
  ungroup()
 

 
fields <- c("year", "overall_pick", "team" , "player", "college")
data_fields <- data %>% filter(!indicator %in% fields)

df_pca <- data_fields %>%
  filter(indicator %in% c("value_over_replacement", "box_plus_minus", "win_shares", 
                          "win_shares_per_48_minutes", "points_per_game", 
                          "average_total_rebounds", "average_assists", 
                          "field_goal_percentage", "3_point_percentage", 
                          "free_throw_percentage", "years_active", "games", 
                          "minutes_played", "average_minutes_played")) %>%
  select(player, year, overall_pick, indicator, normValue) %>%
  pivot_wider(names_from = indicator, values_from = normValue) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

columnas_metricas <- df_pca %>% select(-player, -year, -overall_pick)

pca_nba <- prcomp(columnas_metricas, scale. = TRUE)

var_explicada <- pca_nba$sdev^2 / sum(pca_nba$sdev^2)

rotaciones_abs <- abs(pca_nba$rotation)

pesos_crudos <- rotaciones_abs %*% var_explicada

pesos_finales <- (pesos_crudos / max(pesos_crudos)) * 3.0

df_pesos <- tibble(
  indicator = rownames(pesos_crudos),
  peso_calculado = as.numeric(pesos_finales)
) %>%
  arrange(desc(peso_calculado))

player_scores <- data_fields %>%
  filter(!indicator %in% c("points", "total_rebounds", "assists")) %>%
  left_join(df_pesos, by = "indicator") %>%
  mutate(peso_calculado = replace_na(peso_calculado, 1)) %>%
  mutate(weighted_value = normValue * peso_calculado) %>%
  group_by(player, year, overall_pick) %>%
  summarise(
    overall_performance = sum(weighted_value, na.rm = TRUE),
    .groups = "drop"
  )

baremo <- quantile(player_scores$overall_performance, probs = c(0.15, 0.30, 0.50, 0.65, 0.78, 0.88, 0.96, 0.99))
categories <- c("Generational", "SuperStar", "Star", "Elite", "Starter", "Solid", "Role", "Rotation", "Deep")

player_success <- player_scores %>%
  mutate(
    rank = case_when(
      overall_performance >= baremo[8] ~ "Generational",
      overall_performance >= baremo[7] ~ "SuperStar",
      overall_performance >= baremo[6] ~ "Star",
      overall_performance >= baremo[5] ~ "Elite",
      overall_performance >= baremo[4] ~ "Starter",
      overall_performance >= baremo[3] ~ "Solid",
      overall_performance >= baremo[2] ~ "Role",
      overall_performance >= baremo[1] ~ "Rotation",
      TRUE ~ "Deep"
    ),
    rank = factor(rank, levels = categories, ordered = TRUE)
  ) %>% 
  arrange(desc(overall_performance)) %>% 
  select(-overall_pick, -year)
 


 
fields <- c("year", "overall_pick", "team" , "player", "college")

data_fields <- data %>% filter(!indicator %in% fields)

player_scores <- data_fields %>%
  filter(!indicator %in% c("points", "total_rebounds", "assists")) %>%
  mutate(
    weighted_value = case_when(
      indicator %in% c("value_over_replacement", "box_plus_minus", "win_shares", "win_shares_per_48_minutes") ~ normValue * 3,
      indicator %in% c("points_per_game", "average_total_rebounds", "average_assists") ~ normValue * 2.5,
      indicator %in% c("field_goal_percentage", "3_point_percentage", "free_throw_percentage") ~ normValue * 1.5,
      indicator %in% c("years_active", "games", "minutes_played", "average_minutes_played") ~ normValue * 1.25,
      TRUE ~ normValue
    )
  ) %>%
  group_by(player, year, overall_pick) %>%
  summarise(
    overall_performance = sum(weighted_value, na.rm = TRUE),
  ) %>% ungroup()

baremo <- quantile(player_scores$overall_performance, probs = c(0.15, 0.30, 0.50, 0.65, 0.78, 0.88, 0.96, 0.99))

player_success <- player_scores %>%
  mutate(
    rank = case_when(
      overall_performance >= baremo[8] ~ "Generational",
      overall_performance >= baremo[7] ~ "SuperStar",
      overall_performance >= baremo[6] ~ "Star",
      overall_performance >= baremo[5] ~ "Elite",
      overall_performance >= baremo[4] ~ "Starter",
      overall_performance >= baremo[3] ~ "Solid",
      overall_performance >= baremo[2] ~ "Role",
      overall_performance >= baremo[1] ~ "Rotation",
      TRUE ~ "Deep"
    ),
    rank = factor(rank, levels = c("Generational", "SuperStar", "Star", "Elite", "Starter", "Solid", "Role", "Rotation", "Deep"), ordered = TRUE)
  )
 

 
dataPerformance <- data_fields %>% left_join(player_success, by = c("player", "year", "overall_pick")) %>%
  select(year, overall_pick, rank, team, player, college, indicator, value, overall_performance)
 


 
reDraft <- dataPerformance %>%
  group_by(year, player, overall_pick) %>%
  summarise(overall_performance = mean(overall_performance)) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(year, desc(overall_performance)) %>%
  mutate(newPick = row_number()) %>%
  filter(newPick <= 60) %>%
  ungroup() %>%
  select(-overall_performance)
 

 
dataReDraft <- dataPerformance %>% 
  left_join(reDraft, by = c("player", "year", "overall_pick")) %>%
  mutate(pickDifference = overall_pick - newPick) %>%
  relocate(c(newPick, pickDifference), .after = overall_pick)
 


 
picksAverage <- dataPerformance %>% group_by(overall_pick) %>%
  mutate(overall_pick = as.numeric(overall_pick)) %>%
  summarise(observed_perf = mean(overall_performance))



ideal_curve_simple <- data.frame(overall_pick = 1:60)

modelo_lineal <- lm(observed_perf ~ overall_pick, data = picksAverage)

ideal_curve_simple <- ideal_curve_simple %>%
  mutate(
    expected_perf = predict(modelo_lineal, newdata = ideal_curve_simple)
  )
 

 
picksAverage <- picksAverage %>% left_join(ideal_curve_simple, by = "overall_pick")
 

 
tidyData <- dataReDraft %>% left_join(picksAverage, by = "overall_pick") %>%
  mutate(pick_perf_difference = observed_perf - expected_perf)

base <- tidyData %>%
  select(-overall_performance, -observed_perf, -expected_perf, -pick_perf_difference)

newMetrics <- tidyData %>%
  select(year, overall_pick, newPick, pickDifference, rank, team, player, college, 
         overall_performance, observed_perf, expected_perf, pick_perf_difference) %>%
  distinct()
tidyMetrics <- newMetrics %>%
  pivot_longer(
    cols = c(overall_performance, observed_perf, expected_perf, pick_perf_difference),
    names_to = "indicator",
    values_to = "value"
  )

temptidyData <- bind_rows(base, tidyMetrics) %>% arrange(year, player, indicator)
 


 
picksIndicator <- c("observed_perf", "pick_perf_difference", "expected_perf")
tidyData <- temptidyData %>% 
  mutate(unit = case_when(
    indicator %in% picksIndicator ~ "Selection",
    TRUE ~ "Player"
  )) %>%
  relocate(unit, .before = indicator) %>% 
  arrange(year, overall_pick, unit)
 

 
untidyData <- temptidyData %>%
  pivot_wider(
    names_from = indicator,
    values_from = value
  )
 

 
tidyData %>% write_csv("tidyData.csv")
untidyData %>% write_csv("untidyData.csv")
 