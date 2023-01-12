ui <- dashboardPage(
  
  # Header ----
  
  dashboardHeader(),
  
  # Sidebar ----
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Headlines", tabName = "headlines", icon = icon("dashboard")),
      menuItem("Players", tabName = "player_data"),
      menuItem("Teams", tabName = "team_data", icon = icon("th")),
      menuItem("Player Analysis", tabName = "player_analysis", icon = icon("th"))
    )
  ),
  
  # Body ----
  
  dashboardBody(
    
    tabItems(
      
      ## Headlines ----
      
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
      
      ## Player Data ----
      
      tabItem(tabName = "player_data",
              
              fluidRow(
                column(width = 2,
                       selectInput(
                         inputId = "pd_player_select",
                         label = tags$b("Select Player"),
                         choices = sort(unique(lec_data$player_name)))
                ),
              
                column(width = 2,
                       selectInput(
                         inputId = "pd_team_select",
                         label = tags$b("Filter by team"),
                         choices = sort(unique(lec_data$team_name)))
                )
              ),
              
              box(width = 16,
                  fluidRow(valueBoxOutput("pd_total_kills"),
                           valueBoxOutput("pd_kills_per_game"),
                           valueBoxOutput("pd_kill_participation")))
      ),
                
                
      
      ## Team Data ----
      
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
      
      ## Player Analysis
      
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