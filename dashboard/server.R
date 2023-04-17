server <- function(input, output, session) {
  
  # Headlines ----
  
  output$dash_total_players <- renderValueBox({
    valueBox(no_players, "Players", color = "blue")})
  
  output$dash_total_teams <- renderValueBox({
    valueBox(no_teams, "Teams", color = "navy")})
  
  output$dash_total_games <- renderValueBox({
    valueBox(no_games, "Games", color = "olive")})
  
  output$dash_total_kills <- renderValueBox({
    valueBox(total_kills, "Kills", color = "maroon")})
  
  output$dash_total_deaths <- renderValueBox({
    valueBox(total_kills, "Deaths", color = "black")})
  
  output$dash_total_champs <- renderValueBox({
    valueBox(total_champs, "Champions Picked", color = "orange")})
  
  output$dash_most_picked <- renderValueBox({
    valueBox(paste(most_picked, collapse = " & "), "Most Picked", color = "black")})
  
  output$dash_most_banned <- renderValueBox({
    valueBox(paste(most_banned, collapse = " & "), "Most Banned", color = "black")})
  
  # Player Data ----
  
  ## Reactives ----
  
  observe({
    
    updateSelectInput(session, "pd_player_select",
                      choices = pd_player_select())
    
  })
  
  pd_filter_player <- reactive({
    
    lec_data_players %>% 
      filter(player_name == input$pd_player_select)
    
  })
  
  pd_player_select <- reactive({
    
    player <- lec_data_players %>%
      filter(team_name == input$pd_team_select) %>% 
      select(player_name) %>% 
      distinct() %>% 
      arrange(player_name) %>%
      pull()
    
    return(as.list(player))
    
  })
  
  pd_player_stats <- reactive({
    
    stats <- pd_filter_player() %>% 
      summarise(wins = sum(winner),
                losses = nrow(.) - sum(winner),
                games_played = nrow(.),
                winrate = round((sum(winner) / nrow(.) * 100), 2),
                kills = sum(kills),
                kda = mean(kda),
                kpg = round((sum(kills) / nrow(.)), 2),
                ds = round((mean(damage_share) * 100), 2),
                gs = round((mean(earned_gold_share) * 100), 2))
    
    stats$kpt <- pd_filter_player() %>% 
      summarise(kpt = round(mean(((kills + assists) / team_kills) * 100), 2))
    
    stats$dpt <- pd_filter_player() %>% 
      # need to filter out games with no team deaths to avoid division by 0
      filter(team_deaths > 0) %>% 
      summarise(dpt = round(mean((deaths / team_deaths) * 100), 2))
    
    stats$role <- pd_filter_player() %>% 
      select(position) %>% 
      distinct()
    
    return(as.list(stats))
    
  })
  
  pd_last_ten <- reactive({
    
    pd_filter_player() %>% 
      arrange(desc(date), desc(game)) %>% 
      slice_max(date, n = 10) %>% 
      select(date, champion, winner, kills, deaths, assists, kda, total_cs, game_length, side)
  })
  
  ## Outputs ----
  
  output$pd_player_select <- renderUI({
    
    selectInput("pd_player_select",
                label = tags$b("Select Player"),
                choices = pd_player_select())
  })
  
  ### Summary ----
  
  output$pd_role <- renderValueBox({
    
    valueBox(paste(pd_player_stats()$role, collapse = " & "), 
             "Position", color = "navy")
  })
  
  
  
  output$pd_games_played <- renderValueBox({
    
    valueBox(pd_player_stats()$games_played, 
             "Games Played", color = "blue")
    
  })
  
  output$pd_win_pct <- renderValueBox({
    
    valueBox(paste0(pd_player_stats()$winrate,"%"), 
             "Win Rate", color = "olive")
    
  })
  
  output$pd_kills_per_game <- renderValueBox({
    
    valueBox(pd_player_stats()$kpg, 
             "Kills per Game", color = "purple")
  })
  
  output$pd_avg_kda <- renderValueBox({
    
    valueBox(round(pd_player_stats()$kda, 2), 
             "Average KDA", color = "maroon")
    
  })
  
  output$pd_kill_participation <- renderValueBox({
    
    valueBox(paste0(pd_player_stats()$kpt,"%"), 
             "Kill Participation", color = "red")
  })
  
  output$pd_death_contrib <- renderValueBox({
    
    valueBox(paste0(pd_player_stats()$dpt, "%"), 
             "Death Contribution", color = "black")
  })
  
  output$pd_dmg_share <- renderValueBox({
    
    valueBox(paste0(pd_player_stats()$ds, "%"), 
             "Share of Champion Damage", color = "fuchsia")
  })
  
  output$pd_gold_share <- renderValueBox({
    
    valueBox(paste0(pd_player_stats()$gs, "%"), 
             "Average Gold Contribution", color = "orange")
  })
  
  ### Champ Picks ----
  
  output$pd_picks <- renderDataTable(
    
    (pd_filter_player() %>% 
      group_by(champion) %>% 
      summarise(picked = n(),
                wins = sum(winner),
                losses = n() - sum(winner),
                winrate = round((sum(winner) / n()) * 100, 2)) %>% 
      arrange(desc(picked), desc(winrate))),
    
    options = list(dom = "t",
                   columnDefs = list(list(targets = "_all", searchable = FALSE))
    )
    
  )
  
  ### Last 10 Games ----
  
  output$pd_last_game <- renderUI({
    
    res = GET("https://raw.communitydragon.org/latest/plugins/rcp-be-lol-game-data/global/default/v1/champion-summary.json")
    url <- fromJSON(rawToChar(res$content)) %>% 
      mutate(squarePortraitPath = str_replace_all(squarePortraitPath, "^/lol-game-data/assets", "https://raw.communitydragon.org/13.7/plugins/rcp-be-lol-game-data/global/default")) %>% 
      filter(name == ((slice(pd_last_ten(), 1)) %>% select(champion) %>% pull())) %>% 
      select(squarePortraitPath) %>% 
      pull()
    
    last <- slice(pd_last_ten(), 1)
    
    tagList(
      last$winner,
      img(src=url, height="10%", width="10%"),
      last$kills,
      "/",
      last$deaths,
      "/",
      last$assists
    )
    
  })
  
  # Split Data ----
  
  ## Outputs ----
  
  output$split_rankings <- renderDataTable(
    
    (lec_data_teams %>%
       group_by(team_name) %>% 
       filter((split == input$splits_split_select) & (playoffs == FALSE))  %>%  
       filter(split_wins == max(split_wins)) %>% 
       arrange(desc(split_wins)) %>% 
       select(team_name, split_wins, split_losses) %>% 
       distinct()
    ),
    
    options = list(dom = "t",
                   columnDefs = list(list(targets = "_all", searchable = FALSE))
    )
  )
  
  # Team Data ----
  
  ## Outputs ----
  
  output$roster <- renderDataTable(
    (get_roster(input$team_select)),
    options = list(dom = "t",
                   columnDefs = list(list(targets = "_all", searchable = FALSE))
    )
  )
  
  output$match_history <- renderDataTable(
    (get_match_history(input$team_select) %>% 
       filter(
         split == case_when(
           input$split_select == "Spring" ~ "Spring",
           input$split_select == "Summer" ~ "Summer",
           TRUE ~ split),
         playoffs ==
           case_when(
             input$playoff_select == "Yes" ~ TRUE,
             input$playoff_select == "No" ~ FALSE,
             TRUE ~ playoffs))
    ),
    options = list(pageLength = 10,
                   dom = "ltipr",
                   columnDefs = list(list(targets = "_all", searchable = FALSE))
    )
  )
  
  output$game_history <- renderDataTable(
    (get_game_history(input$team_select) %>% 
       filter(
         split == case_when(
           input$split_select == "Spring" ~ "Spring",
           input$split_select == "Summer" ~ "Summer",
           TRUE ~ split),
         playoffs ==
           case_when(
             input$playoff_select == "Yes" ~ TRUE,
             input$playoff_select == "No" ~ FALSE,
             TRUE ~ playoffs))
    ),
    options = list(pageLength = 10,
                   dom = "ltipr",
                   columnDefs = list(list(targets = "_all", searchable = FALSE))
    )
  )
  
  # Player Analysis ----
  
  ## Outputs ----
  
  output$player_graph <- renderPlotly({
    
    ggplotly(lec_data_players %>% 
               filter(
                 split == case_when(
                   input$player_split_select == "Spring" ~ "Spring",
                   input$player_split_select == "Summer" ~ "Summer",
                   TRUE ~ split),
                 position ==
                   case_when(
                     input$player_role_select == "Top" ~ "top",
                     input$player_role_select == "Mid" ~ "mid",
                     input$player_role_select == "ADC" ~ "bot",
                     input$player_role_select == "Support" ~ "sup",
                     input$player_role_select == "Jungle" ~ "jng",
                     TRUE ~ position),
                 playoffs ==
                   case_when(
                     input$player_playoff_select == "Yes" ~ TRUE,
                     input$player_playoff_select == "No" ~ FALSE,
                     TRUE ~ playoffs)) %>% 
               ggplot() +
               geom_line(aes_string(x = "date",
                                    y = (metric = case_when(
                                      input$player_metric_select == "Kills" ~ "kills",
                                      input$player_metric_select == "Deaths" ~ "deaths",
                                      input$player_metric_select == "Assists" ~ "assists",
                                      input$player_metric_select == "KDA" ~ "kda",
                                      input$player_metric_select == "Total CS" ~ "total_cs",
                                      input$player_metric_select == "Minion Kills" ~ "minion_kills",
                                      input$player_metric_select == "Monster Kills" ~ "monster_kills",
                                      TRUE ~ "kills")), 
                                    #    group = "player_name", 
                                    colour = "player_name")) +
               scale_x_date(date_breaks = "1 week",
                            date_labels = "%d %B") +
               theme(legend.position = "bottom"))
  })
}