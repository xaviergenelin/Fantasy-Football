library(shiny)
library(shinythemes)
library(nflverse)
library(tidyverse)
library(fst)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(ggh4x)
library(magick)
library(gt)

# load data to use throughout the app
player_season <- read.fst("../data/player_season.fst")
player_weekly <- read.fst("../data/player_weekly.fst")
team_weekly <- read.fst("../data/team_weekly.fst")
team_season <- read.fst("../data/team_season.fst")

# list of columns for each team option
overall_cols <- c("passing_yards", "pass_td", "ints", "completions", "pass_attempts", "completion_pct", 
                      "carries", "rush_yards", "rush_td", "yard_per_car")
not_overall_cols <- c("yac", "pass_20plus", "pass_40plus", "rush_longest", "rush_20plus", "rush_40plus", "rush_stacked_box")
pass_cols <- c("passing_yards", "pass_td", "ints", "completions", "pass_attempts", "completion_pct", "yac", "pass_20plus", "pass_40plus")
rush_cols <- c("carries", "rush_yards", "rush_td", "yard_per_car", "rush_longest", "rush_20plus", "rush_40plus", "rush_stacked_box")
scoring_cols <- c("pass_td", "rush_td")
downs_cols <- c("ints", "yard_per_car")

# team wordmarks, remove the LA rams with the abbreviation of LA (same as the LAR abbrevation)
team_wordmarks <- teams_colors_logos %>%
  filter(team_abbr != "LA")

# load player information
players <- load_players()

# Define server logic required to analyze NFL data
shinyServer(function(input, output, session) {
  
  ### Team Data Table ###
  
  teamDTType <- reactive({
    # Select the dataset based on the user choosing Season or Weekly
    if(input$teamDataSeaWk == "Season"){
      team_season %>%
        arrange(team_name, desc(season))
    } else {
      team_weekly %>%
        arrange(team_name, desc(season), week)
    }
  })
  
  teamDTFilter <- reactive({
    # filter the data further based on user input
    # filter for the seasons, teams, and weeks
    teamDTType() %>%
      filter(season %in% input$teamDataSeasons) %>%
      filter(team_name %in% input$teamDataTeams) %>%
      filter(if(input$teamDataSeaWk == "Weekly") (week %in% input$teamDataWk) else TRUE)
      
  })
  
  teamDTOffDef <- reactive({
    # choose offense or defense columns based on user input
    if(input$teamDataGroup == "Offense"){
      teamDTFilter() %>%
        select(-ends_with("_def"))%>%
        rename_with(~str_remove(., "_off"))
    } else {
      teamDTFilter() %>%
        select(-ends_with("_off"))%>%
        rename_with(~str_remove(., "_def"))
    }
  })
  
  teamDTFinal <- reactive({
    # choose column groups based on user input
    if(input$teamOffDefTypes == "Overall"){
      teamDTOffDef() %>%
        select(-not_overall_cols)
    } else if(input$teamOffDefTypes == "Passing"){
      teamDTOffDef() %>%
        select(-rush_cols)
    } else if(input$teamOffDefTypes == "Rushing"){
      teamDTOffDef() %>%
        select(-pass_cols)
    } else if(input$teamOffDefTypes == "Scoring"){
      teamDTOffDef() %>%
        select(-pass_cols)
    } else if(input$teamOffDefTypes == "Downs"){
      teamDTOffDef() %>%
        select(-rush_cols)
    } else {
      teamDTOffDef()
    }
  })
  
  output$teamDT <- renderDT(
    # show the resulting dataset for the team data table
    datatable(teamDTFinal(), options = list(scrollX = TRUE))
  )
  
  ### Team Profile ###
  
  teamProfileData <- reactive({
    team_season %>%
      #filter(team_name == input$teamProfTeam) %>%
      filter(season %in% input$teamProfSeasons) %>%
      group_by(team_name) %>%
      summarise(carries_off = sum(carries_off),
                rush_yards_off = sum(rush_yards_off),
                rush_td_off = sum(rush_td_off),
                rush_fumble_off = sum(rush_fumble_off),
                passing_yards_off = sum(passing_yards_off),
                pass_td_off = sum(pass_td_def),
                ints_off = sum(ints_off),
                yac_off = sum(ints_off),
                sacks_off = sum(sacks_off),
                completions_off = sum(completions_off),
                pass_attempts_off = sum(pass_attempts_off),
                carries_def = sum(carries_def),
                rush_yards_def = sum(rush_yards_def),
                rush_td_def = sum(rush_td_def),
                rush_fumble_def = sum(rush_fumble_def),
                passing_yards_def = sum(passing_yards_def),
                pass_td_def = sum(pass_td_def),
                ints_def = sum(ints_def),
                yac_def = sum(ints_def),
                sacks_def = sum(sacks_def),
                completions_def = sum(completions_def),
                pass_attempts_def = sum(pass_attempts_def))
  })
  
  teamProfileRanks <- reactive({
    teamProfileData()%>%
      mutate(across(.cols = -team_name, 
                    ~ ifelse(duplicated(min_rank(desc(.))) | duplicated(min_rank(desc(.)), fromLast = TRUE), 
                             paste0("T-", min_rank(desc(.))), 
                             min_rank(desc(.)))))
  })
  
  teamProfileOff <- reactive({
    totals <- teamProfileData() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_off")) %>%
      t() %>%
      as.data.frame() %>%
      rename("Value" = "V1")
    
    ranks <- teamProfileRanks() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_off")) %>%
      t() %>%
      as.data.frame() %>%
      rename("NFL Rankings" = "V1")
    
    cbind(totals, ranks)
  })
  
  teamProfileDef <- reactive({
    totals <- teamProfileData() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_def")) %>%
      t() %>%
      as.data.frame() %>%
      rename("Value" = "V1")
    
    ranks <- teamProfileRanks() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_def")) %>%
      t() %>%
      as.data.frame() %>%
      rename("NFL Rankings" = "V1")
    
    cbind(totals, ranks)
  })
  
  output$teamHeading <- renderUI({
    wordmark <- teamProfileData() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(team_name) %>%
      left_join(team_wordmarks, by = c("team_name" = "team_name")) %>%
      select(team_wordmark) %>%
      pull()
    
    tags$img(src = wordmark, style = "max-width: 50%; heigh: auto;")
  })
  
  output$teamProfOff <- renderDT({
    datatable(teamProfileOff(), options = list(
      searching = FALSE,
      pageLength = nrow(teamProfileOff()),
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    ))
  })
  
  output$teamProfDef <- renderDT({
    datatable(teamProfileDef(), options = list(
      searching = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    ),
    rownames = FALSE)
  })
  
  ############## Team Comparison
  
  # Limits the multiInput for only 2 teams to be chosen at a time
  
  remove_choice_team <- reactiveValues(limit = 0)
  
  observeEvent(input$teamCompTeams, {
    if(length(input$teamCompTeams) > 1){
      if(remove_choice_team$limit == 0){
        remove_choice_team$limit <- 1
      }
    } else{
      if(remove_choice_team$limit == 1){
        remove_choice_team$limit <- 0
      }
    }
  })
  
  observeEvent(remove_choice_team$limit, {
    if(remove_choice_team$limit == 1){
      updateMultiInput(
        session = session,
        inputId = "teamCompTeams",
        choices = input$teamCompTeams,
        selected = input$teamCompTeams
      )
    } else {
      updateMultiInput(
        session = session,
        inputId = "teamCompTeams",
        choices = str_sort(unique(team_season$team_name)),
        selected = input$teamCompTeams
      )
    }
  })
  
  # team comparison data
  teamCompData <- reactive({
    team_season %>%
      filter(team_name %in% input$teamCompTeams) %>%
      filter(season %in% input$teamCompSeasons)
  })
  
  team_list <- reactive({
    c(unique(teamCompData()$team_name))
  })
  
  teamCompBarData <- reactive({
    teamCompData() %>%
      mutate(team_name = factor(team_name, levels = team_list())) %>%
      group_by(team_name) %>%
      summarise(carries_off = sum(carries_off),
                rush_td_off = sum(rush_td_off)) %>%
      ungroup() %>%
      pivot_longer(cols = !team_name, names_to = "stat", values_to = "value")
  })
  
  ## Team 1
  
  output$team1_name <- renderText({
    team_list()
  })
  
  output$team1_wordmark <- renderUI({
    wordmark1 <- teamCompData() %>%
      filter(team_name == team_list()[1]) %>%
      select(team_name) %>%
      left_join(team_wordmarks, by = c("team_name" = "team_name")) %>%
      select(team_wordmark) %>%
      distinct() %>%
      pull()
    
    tags$img(src = wordmark1, style = "max-width: 75%; heigh: auto;")
  })
  
  team1_color_main <- reactive({
    team_wordmarks %>%
      filter(team_name == team_list()[1]) %>%
      select(team_color) %>%
      pull()
  })
  
  team1_color_text <- reactive({
    team_wordmarks %>%
      filter(team_name == team_list()[1]) %>%
      mutate(selected_color = ifelse(team_color == team1_color_main(), team_color2, team_color)) %>%
      select(selected_color) %>%
      pull()
  })

  ## Team 2
  output$team2_wordmark <- renderUI({
    wordmark2 <- teamCompData() %>%
      filter(team_name == team_list()[2]) %>%
      select(team_name) %>%
      left_join(team_wordmarks, by = c("team_name" = "team_name")) %>%
      select(team_wordmark) %>%
      distinct() %>%
      pull()
    
    tags$img(src = wordmark2, style = "max-width: 75%; heigh: auto;")
  })
  
  team2_color_main <- reactive({
    team_wordmarks %>%
      filter(team_name == team_list()[2]) %>%
      mutate(selected_color = ifelse(team_color == team1_color_main(), team_color2, team_color)) %>%
      select(selected_color) %>%
      pull()
  })
  
  team2_color_text <- reactive({
    team_wordmarks %>%
      filter(team_name == team_list()[2]) %>%
      mutate(selected_color = ifelse(team_color == team2_color_main(), team_color2, team_color)) %>%
      select(selected_color) %>%
      pull()
  })
  
  # Stacked Bar Chart
  
  output$teamCompBarGraph <- renderPlot({
    ggplot(data = teamCompBarData(), aes(x = stat, y = value, fill = team_name)) +
      geom_col(position = position_fill(reverse = TRUE)) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = value,
                    color = team_name),
                position = position_fill(vjust = 0.5, reverse = TRUE)) +
      scale_color_manual(values = c(team1_color_text(), team2_color_text())) +
      guides(color = "none") +
      coord_flip() +
      scale_fill_manual(values = alpha(c(team1_color_main(), team2_color_main())))
  })
  
  ############## Player Data Table

  playerDataChoice <- reactive({
    if(input$playerTableType == "Season"){
      player_season %>%
        select(-c(player_id, headshot_url))
    } else {
      player_weekly
    }
  })
  
  output$playerDT <- renderDT({
    datatable(playerDataChoice(), options = list(scrollX = TRUE))
  })
  
  ############## Player Profile
  
  playerProfileDataSea <- reactive({
    player_season %>%
      filter(player_display_name == input$playerProfPlayers)
  })
  
  playerInfo <- reactive({
    playerProfileDataSea() %>%
      left_join(players, by = c("player_display_name" = "display_name"), suffix = c("", ".drop")) %>%
      select(-ends_with(".drop")) %>%
      arrange(season) %>%
      slice_tail(n = 1) %>%
      select(player_display_name, headshot_url, position, team_abbr, uniform_number, years_of_experience) %>%
      gt() %>%
      cols_label(player_display_name = "Name",
                 headshot_url = "",
                 position = "Pos",
                 team_abbr = "Team",
                 uniform_number = "Num",
                 years_of_experience = "Exp") %>%
      text_transform(
        locations = cells_body(columns = c(headshot_url)),
        fn = function(x){
          gt::web_image(x)
        }
      )
  })
  
  output$playerTest <- render_gt({
    expr = playerInfo()
  })
  
  ############## Player comparison
  
  # Limits the multiInput for only 4 players to be chosen at most
  
  remove_choice_player <- reactiveValues(limit = 0)
  
  observeEvent(input$playerCompPlayers, {
    if(length(input$playerCompPlayers) > 3){
      if(remove_choice_player$limit == 0){
        remove_choice_player$limit <- 1
      }
    } else{
      if(remove_choice_player$limit == 1){
        remove_choice_player$limit <- 0
      }
    }
  })
  
  observeEvent(remove_choice_player$limit, {
    if(remove_choice_player$limit == 1){
      updateMultiInput(
        session = session,
        inputId = "playerCompPlayers",
        choices = input$playerCompPlayers,
        selected = input$playerCompPlayers
      )
    } else {
      updateMultiInput(
        session = session,
        inputId = "playerCompPlayers",
        choices = str_sort(unique(player_season$player_display_name)),
        selected = input$playerCompPlayers
      )
    }
  })
  
  # create a dataset based on user input
  playerCompData <- reactive({
    player_weekly %>%
      filter(player_display_name %in% input$playerCompPlayers) %>%
      filter(season %in% input$playerCompSeason)
  })
  
  # create a list of the players names 
  player_list <- reactive({
    c(unique(playerCompData()$player_display_name))
  })
  
  
  #### hide the boxes until a player is chosen
  observe(
    ## player 1
    if(is.na(player_list()[1])){
      shinyjs::hide(id = "player_box1", asis = TRUE)
    } else {
      shinyjs::show(id = "player_box1")
    }
  )
  
  observe(
    ## player 2
    if(is.na(player_list()[2])){
      shinyjs::hide(id = "player_box2")
    } else {
      shinyjs::show(id = "player_box2")
    }
  )
  
  observe(
    ## player 3
    if(is.na(player_list()[3])){
      shinyjs::hide(id = "player_box3")
    } else {
      shinyjs::show(id = "player_box3")
    }
  )
  
  observe(
    ## player 4
    if(is.na(player_list()[4])){
      shinyjs::hide(id = "player_box4")
    } else {
      shinyjs::show(id = "player_box4")
    }
  )
  
  ###########################
  ########## Box 1 ##########
  ###########################

  # player name
  output$player1_name <- renderText({
    player_list()[1]
  })
  
  # player data
  player1_data <- reactive({
    playerCompData() %>%
      filter(player_display_name == player_list()[1])
  })
  
  # player picture
  p1_headshot <- reactive({
    c(unique(player1_data()$headshot_url))
  })
  
  output$player1_pic <- renderText({
    c("<img src='", substring(p1_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })

  # player's median value for selected variable
  output$player1_med <- renderText({
    median(player1_data()[[input$playerCompStat]])
  })
  
  # player's average value for selected variable
  output$player1_avg <- renderText({
    mean(player1_data()[[input$playerCompStat]])
  })

  
  ###########################
  ########## Box 2 ##########
  ###########################
  
  # player name
  output$player2_name <- renderText({
    player_list()[2]
  })
  
  # player data
  player2_data <- reactive({
    playerCompData() %>%
      filter(player_display_name == player_list()[2])
  })
  
  # player picture
  p2_headshot <- reactive({
    c(unique(player2_data()$headshot_url))
  })
  
  output$player2_pic <- renderText({
    c("<img src='", substring(p2_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  # player's median value for selected variable
  output$player2_med <- renderText({
    median(player2_data()[[input$playerCompStat]])
  })
  
  # player's average value for selected variable
  output$player2_avg <- renderText({
    mean(player2_data()[[input$playerCompStat]])
  })
  
  ###########################
  ########## Box 3 ##########
  ###########################
  
  # player name
  output$player3_name <- renderText({
    player_list()[3]
  })
  
  # player data
  player3_data <- reactive({
    playerCompData() %>%
      filter(player_display_name == player_list()[3])
  })
  
  # player picture
  p3_headshot <- reactive({
    c(unique(player3_data()$headshot_url))
  })
  
  output$player3_pic <- renderText({
    c("<img src='", substring(p3_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  # player's median value for selected variable
  output$player3_med <- renderText({
    median(player3_data()[[input$playerCompStat]])
  })
  
  # player's average value for selected variable
  output$player3_avg <- renderText({
    mean(player3_data()[[input$playerCompStat]])
  })
  
  ###########################
  ########## Box 4 ##########
  ###########################
  
  # player name
  output$player4_name <- renderText({
    player_list()[4]
  })
  
  # player data
  player4_data <- reactive({
    playerCompData() %>%
      filter(player_display_name == player_list()[4])
  })
  
  # player picture
  p4_headshot <- reactive({
    c(unique(player4_data()$headshot_url))
  })
  
  output$player4_pic <- renderText({
    c("<img src='", substring(p4_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  # player's median value for selected variable
  output$player4_med <- renderText({
    median(player4_data()[[input$playerCompStat]])
  })
  
  # player's average value for selected variable
  output$player4_avg <- renderText({
    mean(player4_data()[[input$playerCompStat]])
  })
  
  ## player comparison graphs
  
  # stacked bar chart data
  playerCompBarData <- reactive({
    playerCompData() %>%
      mutate(player_display_name = factor(player_display_name, levels = player_list())) %>%
      group_by(player_display_name) %>%
      summarise(carries = sum(carries),
                rushing_yards = sum(rushing_yards),
                rushing_tds = sum(rushing_tds),
                receptions = sum(receptions),
                targets = sum(targets),
                receiving_yards = sum(receiving_yards),
                receiving_air_yards = sum(receiving_air_yards),
                receiving_yards_after_catch = sum(receiving_yards_after_catch),
                receiving_tds = sum(receiving_tds),
                fantasy_points = sum(fantasy_points),
                fantasy_points_ppr = sum(fantasy_points_ppr)) %>%
      ungroup() %>%
      pivot_longer(cols = !player_display_name, names_to = "stat", values_to = "value") %>%
      arrange(factor(player_display_name, levels = player_list()))
  })
  
  # stacked bar chart
  output$playerCompBarGraph <- renderPlot({
    
    ggplot(data = playerCompBarData(), aes(fill = player_display_name, x = stat, y = value)) +
      geom_col(position = position_fill(reverse = TRUE)) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = value,
                    color = player_display_name),
                position = position_fill(vjust = 0.5, reverse = TRUE)) +
      scale_color_manual(values = c("white", "white", "black", "black")) +
      guides(color = "none") +
      coord_flip() +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank()) +
      scale_fill_manual(values = alpha(c("#000080", "#800080", "#C0C0C0", "#FFD700")))

  })
  
  # line graph data
  playerCompLineData <- reactive({
    playerCompData() %>%
      mutate(player_display_name = factor(player_display_name, levels = player_list()))
  })
  
  output$playerCompLineGraph <- renderPlot({
    ggplot(data = playerCompLineData(), aes(x = interaction(week, season, sep = "-"), 
                                            y = receptions, 
                                            group = player_display_name, 
                                            color = player_display_name)) +
      geom_line(size = 1) +
      scale_x_discrete(guide = guide_axis_nested(delim = "-")) +
      theme_bw() +
      scale_color_manual(values = c("#000080", "#800080", "#C0C0C0", "#FFD700"))
      
  })
  
})


