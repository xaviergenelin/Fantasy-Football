library(shiny)
library(shinythemes)
library(nflverse)
library(tidyverse)
library(fst)
library(DT)

# load data to use throughout the app
team_offense_season <- read.fst("../data/team_offense_season.fst")
team_defense_season <- read.fst("../data/team_defense_season.fst")
team_offense_weekly <- read.fst("../data/team_offense_weekly.fst")
team_defense_weekly <- read.fst("../data/team_defense_weekly.fst")

# list of columns for each team option
overall_cols <- c("team", "season", "passing_yards", "pass_td", "ints", "completions", "pass_attempts", "completion_pct", 
                      "carries", "rush_yards", "rush_td", "yard_per_car")
pass_cols <- c("team", "season", "passing_yards", "pass_td", "ints", "completions", "pass_attempts", "completion_pct")
rush_cols <- c("team", "season", "carries", "rush_yards", "rush_td", "yard_per_car")
scoring_cols <- c("team", "season", "pass_td", "rush_td")
downs_cols <- c("team", "season")

# Define server logic required to analyze NFL data
shinyServer(function(input, output, session) {
  
  # get the data of interest for the team tab
  teamDataChoice <- reactive({

    if(input$teamDataType == "Season" & input$teamGroup == "Offense"){
      # Offensive Season Data
      if(input$teamOffDefTypes == "Overall"){
        team_offense_season %>%
          select(overall_cols) %>%
          arrange(team, season)
      } else if(input$teamOffDefTypes == "Passing"){
        team_offense_season %>%
          select(pass_cols) %>%
          arrange(team, season)
      } else if(input$teamOffDefTypes == "Rushing"){
        team_offense_season %>%
          select(rush_cols) %>%
          arrange(team, season)
      } else if(input$teamOffDefTypes == "Receiving"){
        team_offense_season %>%
          select(pass_cols) %>%
          arrange(team, season)
      } else if(input$teamOffDefTypes == "Scoring"){
        team_offense_season %>%
          select(scoring_cols) %>%
          arrange(team, season)
      } else {
        team_offense_season %>%
          select(downs_cols) %>%
          arrange(team, season)
      }
    } else if(input$teamDataType == "Season" & input$teamGroup == "Defense"){
      # Defensive Season Data
      if(input$teamOffDefTypes == "Overall"){
        team_defense_season %>%
          select(overall_cols) %>%
          arrange(team, season)
      } else if(input$teamOffDefTypes == "Passing"){
        team_defense_season %>%
          select(pass_cols) %>%
          arrange(team, season)
      } else if(input$teamOffDefTypes == "Rushing"){
        team_defense_season %>%
          select(rush_cols) %>%
          arrange(team, season)
      } else if(input$teamOffDefTypes == "Receiving"){
        team_defense_season %>%
          select(pass_cols) %>%
          arrange(team, season)
      } else if(input$teamOffDefTypes == "Scoring"){
        team_defense_season %>%
          select(scoring_cols) %>%
          arrange(team, season)
      } else {
        team_defense_season %>%
          select(downs_sea_cols) %>%
          arrange(team, season)
      }
    } else if(input$teamDataType == "Season" & input$teamGroup == "Special Teams"){
      # Special Teams Season Data
      if(input$teamSPTypes == "Field Goals"){
        team_offense_season %>%
          select(overall_cols) %>%
          arrange(team, season)
      } else if(input$teamSPTypes == "Scoring"){
        team_offense_season %>%
          select(rush_cols) %>%
          arrange(team, season)
      } else if(input$teamSPTypes == "Kick Returns"){
        team_offense_season %>%
          select(pass_cols) %>%
          arrange(team, season)
      } else if(input$teamSPTypes == "Punt Retruns"){
        team_offense_season %>%
          select(scoring_cols) %>%
          arrange(team, season)
      } else if(input$teamSPTypes == "Kickoffs"){
        team_offense_season %>%
          select(scoring_sea_cols) %>%
          arrange(team, season)
      } else {
        team_offense_season %>%
          select(downs_cols) %>%
          arrange(team, season)
      }
    } else if(input$teamDataType == "Weekly" & input$teamGroup == "Offense"){
      # Offensive Weekly Data
      if(input$teamOffDefTypes == "Overall"){
        team_offense_weekly %>%
          select(overall_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamOffDefTypes == "Passing"){
        team_offense_weekly %>%
          select(pass_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamOffDefTypes == "Rushing"){
        team_offense_weekly %>%
          select(rush_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamOffDefTypes == "Receiving"){
        team_offense_weekly %>%
          select(pass_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamOffDefTypes == "Scoring"){
        team_offense_weekly %>%
          select(scoring_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else {
        team_offense_weekly %>%
          select(downs_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      }
    } else if(input$teamDataType == "Weekly" & input$teamGroup == "Special Teams"){
      # Speical Teams Weekly Data
      if(input$teamSPTypes == "Field Goals"){
        team_offense_weekly %>%
          select(overall_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamSPTypes == "Scoring"){
        team_offense_weekly %>%
          select(rush_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamSPTypes == "Kick Returns"){
        team_offense_weekly %>%
          select(pass_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamSPTypes == "Punt Retruns"){
        team_offense_weekly %>%
          select(scoring_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamSPTypes == "Kickoffs"){
        team_offense_weekly %>%
          select(scoring_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else {
        team_offense_weekly %>%
          select(downs_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      }
    } else {
      # Defensive Weekly Data
      if(input$teamOffDefTypes == "Overall"){
        team_defense_weekly %>%
          select(overall_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamOffDefTypes == "Passing"){
        team_defense_weekly %>%
          select(pass_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamOffDefTypes == "Rushing"){
        team_defense_weekly %>%
          select(rush_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamOffDefTypes == "Receiving"){
        team_defense_weekly %>%
          select(pass_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else if(input$teamOffDefTypes == "Scoring"){
        team_defense_weekly %>%
          select(scoring_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      } else {
        team_defense_weekly %>%
          select(downs_cols, "week") %>%
          relocate("week", .after = "season") %>%
          arrange(team, season, week)
      }
    }
  })
  
  output$teamDT <- renderDT({
    datatable(teamDataChoice(), options = list(scrollX = TRUE))
  })




})
