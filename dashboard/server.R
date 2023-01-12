server <- function(input, output) {
  
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
  
  pd_filter_player <- reactive(
    
    lec_data_players %>% 
      filter(player_name == input$pd_player_select)
  )
  
  ## Outputs ----
  
  output$pd_total_kills <- renderValueBox({
    
    total_kills <- pd_filter_player() %>% 
      group_by(player_name) %>% 
      summarise(total_kills = sum(kills)) %>% 
      select(total_kills) %>% 
      pull()
    
    valueBox(total_kills, "Total Kills")
  })
  
  output$pd_kills_per_game <- renderValueBox({
    
    kills_per_game <- pd_filter_player() %>% 
      group_by(player_name) %>% 
      summarise(kpg = round((sum(kills) / n()), 2)) %>% 
      select(kpg) %>% 
      pull()
    
    valueBox(kills_per_game, "Kills per Game")
  })
  
  output$pd_kill_participation <- renderValueBox({

    kpt <- pd_filter_player() %>% 
      group_by(player_name, game_id) %>% 
      summarise(kpt = ((kills + assists) / team_kills) * 100) %>% 
      summarise(kpt = round(mean(kpt)), 2) %>% 
      select(kpt)
    
    valueBox(paste0(kpt,"%"), "Kill Participation")
  })
  
  
  # Team Data ----
  
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