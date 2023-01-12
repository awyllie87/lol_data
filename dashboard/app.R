### TODO: 
### - Enable reactivity on the server
### - split app.R into constituent components i.e server.R, ui.R, global.R
### - More of everything. This is fun!

library(shiny)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(plotly)

# Establish data tables

lec_data <- read_csv("data/lec_data.csv") %>% 
  mutate(game_id = as.factor(game_id),
         date = as.Date(date),
         # kda needs to account for deaths being 0 to prevent infinity.
         # sadly prevents deathless games from being special... perhaps mutate in a column to flag them?
         kda = ((kills + assists) / if_else(deaths == 0, 1, deaths))) %>% 
  relocate(kda, .after = assists) %>% 
  mutate(kda = round(kda, 2))

# create sub-tables for teams and players to reduce the need for repeated filtering
# (the aggregate rows for team stats have NA for player name, making them identifiable)

lec_data_teams <- lec_data %>% 
  filter(is.na(player_name))

lec_data_players <- lec_data %>% 
  filter(!is.na(player_name))

### Functions

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
    select(game_id, date, split, playoffs, game, side, team_name, winner) %>% 
    filter(game_id %in% team_games, team_name != x_team) %>%
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

### Variables

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


### UI

ui <- dashboardPage(
  
  dashboardHeader(),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Headlines", tabName = "headlines", icon = icon("dashboard")),
      menuItem("Team Data", tabName = "team_data", icon = icon("th")),
      menuItem("Player Analysis", tabName = "player_analysis", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "headlines",
              
              ### Produce value boxes with a breakdown of basic highlights of the whole dataset.
              ### Deaths probably isn't necessary.
              ### 
              ### I want to do more with this -- Dynamic icons depending on champions named, perhaps.
              
              fluidRow(valueBoxOutput("dash_total_players"),
                       valueBoxOutput("dash_total_teams"),
                       valueBoxOutput("dash_total_games")),
              
              fluidRow(valueBoxOutput("dash_total_kills"),
                       valueBoxOutput("dash_total_deaths")),
              
              fluidRow(valueBoxOutput("dash_total_champs"),
                       valueBoxOutput("dash_most_picked"),
                       valueBoxOutput("dash_most_banned"))
      ),
      
      tabItem(tabName = "team_data",
              
              ### Break down the team data in various modes of interest.
              ### A full version would be MUCH shinier than this instead of just producing data tables
              ### -- Quick dynamic match breakdowns based on table data, logos, icons, etc.
              ### This will have to do for now!
              
              fluidRow(
                column(width = 2,
                       selectInput(
                         inputId = "team_select",
                         label = tags$b("Select Team"),
                         choices = sort(unique(lec_data$team_name)))
                ),
                
                
                ### I used conditionalPanel() here to hide inputs when they don't do anything. Filtering
                ### the roster by Split or Playoffs doesn't adjust the result, so hide the options.
                ### With a little work I can probably make Split do something -- roster for some teams
                ### changes over the course of a season, and identifying when the changes happen is useful data
                
                column(width = 3,
                       conditionalPanel(
                         condition = "input.team_tabs == 'Game History'
                                    | input.team_tabs == 'Match History'",
                         radioButtons(inline = TRUE,
                                      inputId = "split_select",
                                      label = tags$b("Split"),
                                      choices = c("Both", "Spring", "Summer")))
                ),
                
                column(width = 4,
                       conditionalPanel(
                         condition = "input.team_tabs == 'Game History'
                                    | input.team_tabs == 'Match History'",
                         radioButtons(inline = TRUE,
                                      inputId = "playoff_select",
                                      label = tags$b("Playoffs"),
                                      choices = c("Both", "Yes", "No")))
                )
                
              ),
              
              fluidRow(
                tabBox(id = "team_tabs",
                       width = 12,
                       tabPanel("Roster",
                                dataTableOutput("roster")),
                       
                       tabPanel("Game History",
                                dataTableOutput("game_history")),
                       
                       tabPanel("Match History",
                                dataTableOutput("match_history")))
              )
      ),
      tabItem(tabName = "player_analysis",
              
              ### First run at dynamic plotting.
              ### The plots are EXTREMELY crowded with everything populated, so
              ### produced a bunch of filters to clean it up.
              ###
              ### Converted the ggplot() to a plotly() graph to allow cooler interactivity,
              ### but I haven't learned plotly syntax to do much adjustment yet.
              ###
              ### The metrics I expose for viewing are all useful pieces of data
              ### and there's a ton more I could and probably will expose with more work
              ###
              ### Biggest issue I'm confronting now is making it useful! I'm bad at graphs!!
              ###
              ### Second biggest issue -- sometimes multiple games occur on the same day.
              ### I need to figure out a way to show this and also have it readable.
              ### In the version I've settled on right now, they all just appear as one X coordinate,
              ### which is obviously not good.
              ### Returning the column to being datetime instead helps, but they're still crammed
              ### together since the time spans in question are weeks and months.
              ### Using the individual games as a factor makes sense, but making this present data correctly
              ### was proving a lot of trouble for a weekend homework.
              
              fluidRow(
                column(width = 2,
                       radioButtons(inline = TRUE,
                                    inputId = "player_split_select",
                                    label = tags$b("Split"),
                                    choices = c("Both", "Spring", "Summer")
                       )),
                
                column(width =2,
                       radioButtons(inline = TRUE,
                                    inputId = "player_playoff_select",
                                    label = tags$b("Playoffs"),
                                    choices = c("Both", "Yes", "No"))
                ),
                
                column(width = 2,
                       selectInput(inputId = "player_role_select",
                                   label = tags$b("Position"),
                                   choices = c("All", "Top", "Mid", "ADC", "Support", "Jungle"))
                ),
                
                column(width = 2,
                       selectInput(inputId = "player_metric_select",
                                   label = tags$b("Metric"),
                                   choices = c("Kills", "Deaths", "Assists", "KDA", "Total CS", "Minion Kills", "Monster Kills"))
                )),
              
              fluidRow(plotlyOutput("player_graph", height = 700))
      )
    )
  )
)

### Server

# Define server logic required to return data
server <- function(input, output) {
  
  ### Headline Dashboard
  
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
  
  ### Team Data
  
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
  
  ### Player Analysis
  
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

# Run the application 
shinyApp(ui = ui, server = server)