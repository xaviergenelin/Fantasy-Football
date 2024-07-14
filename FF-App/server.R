library(shiny)
library(shinythemes)
library(nflverse)
library(tidyverse)
library(fst)
library(DT)
library(shinyWidgets)

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
  
  
  ### Player comparison
  
  # create a dataset based on user input
  playerCompData <- reactive({
    player_season %>%
      filter(player_display_name %in% input$playerCompPlayers)
  })
  
  # create a list of the players used for comparison
  player_list <- reactive({
    c(unique(playerCompData()$player_display_name))
  })
  
  # hide the boxes until a user is chosen
  observe(
    if(is.na(player_list()[1])){
      shinyjs::hide(id = "player_box1")
    } else {
      shinyjs::show(id = "player_box1")
    }
  )
  
  observe(
    if(is.na(player_list()[2])){
      shinyjs::hide(id = "player_box2")
    } else {
      shinyjs::show(id = "player_box2")
    }
  )
  
  observe(
    if(is.na(player_list()[3])){
      shinyjs::hide(id = "player_box3")
    } else {
      shinyjs::show(id = "player_box3")
    }
  )
  
  observe(
    if(is.na(player_list()[4])){
      shinyjs::hide(id = "player_box4")
    } else {
      shinyjs::show(id = "player_box4")
    }
  )
  
  output$plyaerCompTest <- renderDT({
    datatable(playerCompData())
  })
})
