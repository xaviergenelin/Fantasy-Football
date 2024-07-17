library(shiny)
library(shinythemes)
library(nflverse)
library(tidyverse)
library(fst)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)

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
  
  output$teamDTTest <- renderDT(
    # show the resulting dataset for the team data table
    datatable(teamDTFinal(), options = list(scrollX = TRUE))
  )
  
  ### Team Profile ###
  
  teamProfileData <- reactive({
    team_season %>%
      filter(team_name == input$teamProfTeam) %>%
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
  
  teamProfileOff <- reactive({
    teamProfileData() %>%
      select(ends_with("_off")) %>%
      t()
  })
  
  teamProfileDef <- reactive({
    teamProfileData() %>%
      select(ends_with("def")) %>%
      t()
  })
  
  output$teamHeading <- renderText({
    unique(teamProfileData()$team_name)
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
  
  ### Player Data Table

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
  
  
  ############## Player comparison
  
  # create a dataset based on user input
  playerCompData <- reactive({
    player_season %>%
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
  
  output$plyaerCompTest <- renderDT({
    datatable(playerCompData())
  })
  
  
  ###########################
  ########## Box 1 ##########
  ###########################

  output$player1_name <- renderText({
    player_list()[1]
  })
  
  player1_data <- reactive({
    playerCompData() %>%
      filter(player_display_name == player_list()[1])
  })
  
  p1_headshot <- reactive({
    c(unique(player1_data()$headshot_url))
  })
  
  output$player1_pic <- renderText({
    c("<img src='", substring(p1_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })

  
  ###########################
  ########## Box 2 ##########
  ###########################
  
  output$player2_name <- renderText({
    player_list()[2]
  })
  
  player2_data <- reactive({
    playerCompData() %>%
      filter(player_display_name == player_list()[2])
  })
  
  p2_headshot <- reactive({
    c(unique(player2_data()$headshot_url))
  })
  
  output$player2_pic <- renderText({
    c("<img src='", substring(p2_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  ###########################
  ########## Box 3 ##########
  ###########################
  
  output$player3_name <- renderText({
    player_list()[3]
  })
  
  player3_data <- reactive({
    playerCompData() %>%
      filter(player_display_name == player_list()[3])
  })
  
  p3_headshot <- reactive({
    c(unique(player3_data()$headshot_url))
  })
  
  output$player3_pic <- renderText({
    c("<img src='", substring(p3_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  ###########################
  ########## Box 4 ##########
  ###########################
  
  output$player4_name <- renderText({
    player_list()[4]
  })
  
  player4_data <- reactive({
    playerCompData() %>%
      filter(player_display_name == player_list()[4])
  })
  
  p4_headshot <- reactive({
    c(unique(player4_data()$headshot_url))
  })
  
  output$player4_pic <- renderText({
    c("<img src='", substring(p4_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  # Limits the multiInput for only 4 players to be chosen at most
  
  remove_choice <- reactiveValues(limit = 0)
  
  observeEvent(input$playerCompPlayers, {
    if(length(input$playerCompPlayers) > 3){
      if(remove_choice$limit == 0){
        remove_choice$limit <- 1
      }
    } else{
      if(remove_choice$limit == 1){
        remove_choice$limit <- 0
      }
    }
  })
  
  observeEvent(remove_choice$limit, {
    if(remove_choice$limit == 1){
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
  
  ## player comparison graph
  
  output$player_list_check <- renderText({
    player_list
  })
  
  playerCompGraphData <- reactive({
    playerCompData() %>%
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
  
  output$player_list_display <- renderText({
    player_list()
  })
  
  output$playerDTTest <- renderDT({
    datatable(playerCompGraphData())
  })
  
  output$playerCompGraph <- renderPlot({
    
    ggplot(data = playerCompGraphData(), aes(fill = factor(player_display_name, player_list()), x = stat, y = value)) +
      geom_col(position = position_fill()) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = value,
                    color = factor(player_display_name)),
                position = position_fill(vjust = 0.5)) +
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
      scale_fill_manual(values = alpha(c("#000080", "#800080", "#C0C0C0", "#FFFF00")))
  })
  
})


