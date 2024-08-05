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
library(plotly)

# load data to use throughout the app
player_season <- read.fst("../data/player_season.fst")
player_weekly <- read.fst("../data/player_weekly.fst")
team_weekly <- read.fst("../data/team_weekly.fst")
team_season <- read.fst("../data/team_season.fst")

# list of columns for each team option
overall_cols <- c("Passing Yds", "Pass TDs", "Ints", "Completions", "Pass Attempts", "Completion%", 
                      "Carries", "Rush Yds", "Rush TDs", "Yds/Carr")
not_overall_cols <- c("YAC", "Pass 20+", "Pass 40+", "Rush Longest", "Rush 20+", "Rush 40+", "Rush Stacked Box")
pass_cols <- c("Passing Yds", "Pass TDs", "Ints", "Completions", "Pass Attempts", "Completion%", "YAC", "Pass 20+", "Pass 40+")
rush_cols <- c("Carries", "Rush Yds", "Rush TDs", "Yds/Carr", "Rush Longest", "rush_20plus", "rush_40plus", "rush_stacked_box")
scoring_cols <- c("Pass TDs", "Rush TDs")
downs_cols <- c("Ints", "Yds/Carr")
st_cols <- c("FG Made", "FG Att", "FG Missed", "FG Blocked", "FG%", "FG Long", "PAT Made", "PAT Att", "PAT Missed", "PAT Blocked", "PAT%",
             "FG Made 0-19", "FG Made 20-29", "FG Made 30-39", "FG Made 40-49", "FG Made 50-59", "FG Made 60+", "FG Missed 0-19", "FG Missed 20-29",
             "FG Missed 30-39", "FG Missed 40-49", "FG Missed 50-59", "FG Missed 60+")

team_profile_order <- c("Points", "Carries", "Rush Yds", "YPC", "Rush TDs", "Rush Fum", "Completions", "Pass Attempts", "Comp%", 
                   "Pass Yds", "Pass TDs", "Ints", "Yds/Att", "YAC", "Sacks")
team_comp_totals_order <- c("Comp%", "Pass Longest", "Pass 40+", "Pass 20+", "YAC", "Yds/Att", "Ints", "Pass TDs", "Pass Yds", 
                     "Pass Attempts", "Completions", "Rush Longest", "Rush 40+", "Rush 20+", "Rush TDs", 
                     "YPC", "Rush Yds", "Carries", "Points")
team_comp_per_game_order <- c("Comp%","YAC", "Yds/Att", "Ints", "Pass TDs", "Pass Yds", 
                              "Pass Attempts", "Completions", "Rush TDs", 
                              "YPC", "Rush Yds", "Carries", "Points")
player_comp_cols_order <- c("Receiving Air Yds", "Receiving YAC", "Receiving TDs", "Receiving Yds", "Targets", "Receptions", "Rushing TDs", 
                            "Rushing Yds", "Carries", "Passing YAC", "Passing Air Yds", "Interceptions", "Passing TDs", "Passing Yds", "Attempts", 
                            "Completions", "Fantasy Points PPR", "Fantasy Points")

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
        select(-ends_with("_def")) %>%
        rename_with(~str_remove(., "_off"))
    } else {
      teamDTFilter() %>%
        select(-ends_with("_off")) %>%
        rename_with(~str_remove(., "_def"))
    }
  })
  
  teamDTFinal <- reactive({
    # choose column groups based on user input
    # if(input$teamOffDefTypes == "Overall"){
    #   teamDTOffDef() %>%
    #     select(-not_overall_cols)
    # } else if(input$teamOffDefTypes == "Passing"){
    #   teamDTOffDef() %>%
    #     select(-rush_cols)
    # } else if(input$teamOffDefTypes == "Rushing"){
    #   teamDTOffDef() %>%
    #     select(-pass_cols)
    # } else if(input$teamOffDefTypes == "Scoring"){
    #   teamDTOffDef() %>%
    #     select(-pass_cols)
    # } else if(input$teamOffDefTypes == "Downs"){
    #   teamDTOffDef() %>%
    #     select(-rush_cols)
    # } else {
    #   teamDTOffDef()
    # }
    teamDTOffDef()
  })
  
  output$teamDT <- renderDT({
    # show the resulting dataset for the team data table
    teamDTFreeze <- ifelse(input$teamDataSeaWk == "Season", 4, 5)
    datatable(teamDTFinal(), extensions = 'FixedColumns', options = list(scrollX = TRUE, fixedColumns = list(leftColumns = teamDTFreeze)))
  })
  
  ###### Team Profile ######
  
  teamProfileData <- reactive({
    team_weekly %>%
      filter(season %in% input$teamProfSeasons) %>%
      group_by(team_name) %>%
      summarise(Carries_off = sum(Carries_off),
                `Rush Yds_off` = sum(`Rush Yds_off`),
                `Rush TDs_off` = sum(`Rush TDs_off`),
                `Rush Fum_off` = sum(`Rush Fum_off`),
                `Pass Yds_off` = sum(`Passing Yds_off`),
                `Pass TDs_off` = sum(`Pass TDs_off`),
                Ints_off = sum(Ints_off),
                YAC_off = sum(YAC_off),
                Sacks_off = sum(Sacks_off),
                Completions_off = sum(Completions_off),
                `Pass Attempts_off` = sum(`Pass Attempts_off`),
                Carries_def = sum(Carries_def),
                `Rush Yds_def` = sum(`Rush Yds_def`),
                `Rush TDs_def` = sum(`Rush TDs_def`),
                `Rush Fum_def` = sum(`Rush Fum_def`),
                `Pass Yds_def` = sum(`Passing Yds_def`),
                `Pass TDs_def` = sum(`Pass TDs_def`),
                Ints_def = sum(Ints_def),
                YAC_def = sum(YAC_def),
                Sacks_def = sum(Sacks_def),
                Completions_def = sum(Completions_def),
                `Pass Attempts_def` = sum(`Pass Attempts_def`),
                Points_off = sum(Points_off),
                Points_def = sum(Points_def),
                games = n()) %>%
      mutate(`Comp%_off` = format(round(`Completions_off`/`Pass Attempts_off`, 4) * 100, nsmall = 2),
             `Comp%_def` = format(round(`Completions_def`/`Pass Attempts_def`, 4) * 100, nsmall = 2),
             YPC_off = round(`Rush Yds_off` / `Carries_off`, 2),
             YPC_def = round(`Rush Yds_def` / `Carries_def`, 2),
             `Yds/Att_off` = round(`Pass Yds_off` / `Pass Attempts_off`, 1),
             `Yds/Att_def` = round(`Pass Yds_def` / `Pass Attempts_def`, 1))
  })
  
  teamProfileRanks <- reactive({
    teamProfileData() %>%
      mutate(across(.cols = ends_with("_off"), 
                    ~ ifelse(duplicated(min_rank(desc(.))) | duplicated(min_rank(desc(.)), fromLast = TRUE), 
                             paste0("T-", min_rank(desc(.))), 
                             min_rank(desc(.))))) %>%
      mutate(across(.cols = ends_with("_def"),
                    ~ ifelse(duplicated(min_rank(.)) | duplicated(min_rank(.), fromLast = TRUE), 
                             paste0("T-", min_rank(.)), 
                             min_rank(.))))
  })
  
  # data per game
  teamProfileDataPerGame <- reactive({
    teamProfileData() %>%
      select(-c(`Comp%_off`, `Comp%_def`)) %>%
      mutate(across(.cols = -team_name, ~round(.x/games, 2))) %>%
      select(-games) %>%
      mutate(`Comp%_off` = format(round(`Completions_off`/`Pass Attempts_off`, 4) * 100, nsmall = 2),
             `Comp%_def` = format(round(`Completions_def`/`Pass Attempts_def`, 4) * 100, nsmall = 2),
             YPC_off = round(`Rush Yds_off` / `Carries_off`, 2),
             YPC_def = round(`Rush Yds_def` / `Carries_def`, 2),
             `Yds/Att_off` = round(`Pass Yds_off` / `Pass Attempts_off`, 1),
             `Yds/Att_def` = round(`Pass Yds_def` / `Pass Attempts_def`, 1))
      
  })
  
  # data per game rankings
  teamProfileRanksPerGame <- reactive({
    teamProfileDataPerGame() %>%
      mutate(across(.cols = ends_with("_off"), 
                    ~ ifelse(duplicated(min_rank(desc(.))) | duplicated(min_rank(desc(.)), fromLast = TRUE), 
                             paste0("T-", min_rank(desc(.))), 
                             min_rank(desc(.))))) %>%
      mutate(across(.cols = ends_with("_def"),
                    ~ ifelse(duplicated(min_rank(.)) | duplicated(min_rank(.), fromLast = TRUE), 
                             paste0("T-", min_rank(.)), 
                             min_rank(.))))
  })
  
  ### Team Heading Wordmark
  output$teamHeading <- renderUI({
    wordmark <- teamProfileData() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(team_name) %>%
      left_join(team_wordmarks, by = c("team_name" = "team_name")) %>%
      select(team_wordmark) %>%
      pull()
    
    tags$img(src = wordmark, style = "max-width: 50%; heigh: auto;")
  })
  
  ###### Offensive Team Profile
  
  # Offensive Totals Data 
  teamProfileOffTotals <- reactive({
    totals <- teamProfileData() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_off")) %>%
      rename_with(~str_remove(., "_off")) %>%
      t() %>%
      as.data.frame() %>%
      rename("Value" = "V1")
    
    ranks <- teamProfileRanks() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_off")) %>%
      t() %>%
      as.data.frame() %>%
      rename("NFL Rankings" = "V1")
    
    df <- cbind(totals, ranks)
    
    df[team_profile_order, ]
  })
  
  # Offensive per game data
  teamProfileOffPerGame <- reactive({
    totals <- teamProfileDataPerGame() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_off")) %>%
      rename_with(~str_remove(., "_off")) %>%
      t() %>%
      as.data.frame() %>%
      rename("Value" = "V1")
    
    ranks <- teamProfileRanksPerGame() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_off")) %>%
      t() %>%
      as.data.frame() %>%
      rename("NFL Rankings" = "V1")
    
    df <- cbind(totals, ranks)
    
    df[team_profile_order, ]
  })
  
  # Datatable for the offensive total data
  output$teamProfOffTotals <- renderDT({
    datatable(teamProfileOffTotals(), options = list(
      searching = FALSE,
      pageLength = nrow(teamProfileOffTotals()),
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    ))
  })
  
  # Datatable for the offensive per game data
  output$teamProfOffPerGame <- renderDT({
    datatable(teamProfileOffPerGame(), options = list(
      searching = FALSE,
      pageLength = nrow(teamProfileOffPerGame()),
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    ))
  })
  
  # Offensive Output
  output$teamProfOffCondition <- renderUI({
    if(input$teamProfType == "Totals"){
      DTOutput("teamProfOffTotals")
    } else {
      DTOutput("teamProfOffPerGame")
    }
  })
  
  ###### Defensive Team Profile 
  
  # Defensive Totals Data
  teamProfileDefTotals <- reactive({
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
    
    df <- cbind(totals, ranks)
    
    df[team_profile_order, ]
  })
  
  # Defensive per game data
  teamProfileDefPerGame <- reactive({
    totals <- teamProfileDataPerGame() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_def")) %>%
      rename_with(~str_remove(., "_def")) %>%
      t() %>%
      as.data.frame() %>%
      rename("Value" = "V1")
    
    ranks <- teamProfileRanksPerGame() %>%
      filter(team_name == input$teamProfTeam) %>%
      select(ends_with("_def")) %>%
      t() %>%
      as.data.frame() %>%
      rename("NFL Rankings" = "V1")
    
    df <- cbind(totals, ranks)
    
    df[team_profile_order, ]
  })
  
  # Datatable for the defensive total data
  output$teamProfDefTotals <- renderDT({
    datatable(teamProfileDefTotals(), options = list(
      searching = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    ),
    rownames = FALSE)
  })
  
  # Datatable for the defensive per game data
  output$teamProfDefPerGame <- renderDT({
    datatable(teamProfileDefPerGame(), options = list(
      searching = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    ),
    rownames = FALSE)
  })
  
  # Defensive Conditional output
  output$teamProfDefCondition <- renderUI({
    if(input$teamProfType == "Totals"){
      DTOutput("teamProfDefTotals")
    } else {
      DTOutput("teamProfDefPerGame")
    }
  })
  
  ##### Kicking Data Tables
  
  # Team Profile Kicking Data
  teamProfileKickingData <- reactive({
    # summarize the kicking data for the selected seasons
    team_weekly %>%
      filter(season %in% input$teamProfSeasons) %>%
      group_by(team_name) %>%
      summarise(`FG Made` = sum(`FG Made`, na.rm = TRUE),
                `FG Att` = sum(`FG Att`, na.rm = TRUE),
                `FG Missed` = sum(`FG Missed`, na.rm = TRUE),
                `FG Blocked` = sum(`FG Blocked`, na.rm = TRUE),
                `FG Long` = max(`FG Long`, na.rm = TRUE),
                `PAT Made` = sum(`PAT Made`, na.rm = TRUE),
                `PAT Att` = sum(`PAT Att`, na.rm = TRUE),
                `PAT Missed` = sum(`PAT Missed`, na.rm = TRUE),
                `PAT Blocked` = sum(`PAT Blocked`, na.rm = TRUE),
                games = n()) %>%
      mutate(`FG%` = round(`FG Made` / `FG Att`, 4) * 100,
             `PAT%` = round(`PAT Made` / `PAT Att`, 4) * 100) %>%
      relocate(`FG%`, .after = `FG Blocked`)
  })
  
  # Kicking Totals
  teamProfileKickTotals <- reactive({
    teamProfileKickingData() %>%
      select(-games)
  })
  
  # Kicking Totals Ranks
  teamProfileKickRanks <- reactive({
    # calculate the rankings for each summarized value
    teamProfileKickingData() %>%
      select(-games) %>%
      mutate(across(.cols = c(`FG Made`, `FG Att`, `FG%`, `FG Long`, `PAT Made`, `PAT Att`, `PAT%`),
                    ~ ifelse(duplicated(min_rank(desc(.))) | duplicated(min_rank(desc(.)), fromLast = TRUE), 
                             paste0("T-", min_rank(desc(.))), 
                             min_rank(desc(.))))) %>%
      mutate(across(.cols = c(`FG Missed`, `FG Blocked`, `PAT Missed`, `PAT Blocked`),
                    ~ ifelse(duplicated(min_rank(.)) | duplicated(min_rank(.), fromLast = TRUE), 
                             paste0("T-", min_rank(.)), 
                             min_rank(.))))
  })
  
  # Kicking Per Game
  teamProfileKickPerGame <- reactive({
    teamProfileKickingData() %>%
      mutate(across(.cols = -team_name, ~round(.x/games, 2))) %>%
      select(-c(`FG%`, `PAT%`, games)) %>%
      mutate(`FG%` = round(`FG Made`/`FG Att`, 4),
             `PAT%` = round(`PAT Made`/`PAT Att`, 4)) %>%
      relocate(`FG%`, .after = `FG Blocked`)
  })
  
  # Kicking Ranks Per Game
  teamProfileKickRanksPerGame <- reactive({
    teamProfileKickPerGame() %>%
      mutate(across(.cols = c(`FG Made`, `FG Att`, `FG%`, `FG Long`, `PAT Made`, `PAT Att`, `PAT%`),
                    ~ ifelse(duplicated(min_rank(desc(.))) | duplicated(min_rank(desc(.)), fromLast = TRUE), 
                             paste0("T-", min_rank(desc(.))), 
                             min_rank(desc(.))))) %>%
      mutate(across(.cols = c(`FG Missed`, `FG Blocked`, `PAT Missed`, `PAT Blocked`),
                    ~ ifelse(duplicated(min_rank(.)) | duplicated(min_rank(.), fromLast = TRUE), 
                             paste0("T-", min_rank(.)), 
                             min_rank(.))))
  })
  
  # team profile kicking totals datatable
  teamProfileKickingTotals <- reactive({
    # combine the summarized dataset and the ranking dataset
    rbind(teamProfileKickTotals(), teamProfileKickRanks()) %>%
      filter(team_name == input$teamProfTeam) %>%
      mutate(row_names = c("Value", "NFL Rank")) %>%
      relocate(row_names, 1) %>%
      select(-team_name) %>%
      `row.names<-`(., NULL) %>%
      column_to_rownames(var = "row_names")
  })
  
  # team profile per game datatable
  teamProfileKickingPerGame <- reactive({
    # combine the summarized dataset and the ranking dataset
    rbind(teamProfileKickPerGame(), teamProfileKickRanksPerGame()) %>%
      filter(team_name == input$teamProfTeam) %>%
      mutate(row_names = c("Value", "NFL Rank")) %>%
      relocate(row_names, 1) %>%
      select(-team_name) %>%
      `row.names<-`(., NULL) %>%
      column_to_rownames(var = "row_names")
  })
  
  # Datatable for the kicking total data
  output$teamProfKickTotals <- renderDT({
    datatable(teamProfileKickingTotals(), options = list(
      searching = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    ))
  })
  
  output$teamProfKickPerGame <- renderDT({
    datatable(teamProfileKickingPerGame(), options = list(
      searching = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    ))
  })
  
  output$teamProfKickCondition <- renderUI({
    if(input$teamProfType == "Totals"){
      DTOutput("teamProfKickTotals")
    } else {
      DTOutput("teamProfKickPerGame")
    }
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
    team_weekly %>%
      filter(team_name %in% input$teamCompTeams) %>%
      filter(season %in% input$teamCompSeasons)
  })
  
  team_list <- reactive({
    c(unique(teamCompData()$team_name))
  })
  
  teamCompBarData <- reactive({
    if(input$teamCompSide == "Offense"){
      teamCompData() %>%
        select(team_name, ends_with("_off")) %>%
        rename_with(~str_remove(., "_off")) %>%
        group_by(team_name) %>%
        summarise(Completions = sum(Completions),
                  `Pass Attempts` = sum(`Pass Attempts`),
                  `Pass Yds` = sum(`Passing Yds`),
                  `Pass TDs` = sum(`Pass TDs`),
                  `Pass 20+` = sum(`Pass 20+`),
                  `Pass 40+` = sum(`Pass 40+`),
                  `Pass Longest` = max(`Pass Longest`),
                  #`Passing Air Yds` = sum(`Passing Air Yds`),
                  `YAC` = sum(`YAC`),
                  Ints = sum(Ints),
                  Carries = sum(Carries),
                  `Rush Yds` = sum(`Rush Yds`),
                  `Rush TDs` = sum(`Rush TDs`),
                  `Rush 20+` = sum(`Rush 20+`),
                  `Rush 40+` = sum(`Rush 40+`),
                  `Rush Longest` = max(`Rush Longest`),
                  Points = sum(Points)) %>%
        ungroup() %>%
        mutate(`Comp%` = round(Completions / `Pass Attempts`, 4) * 100,
               `YPC` = round(`Rush Yds` / Carries, 2),
               `Yds/Att` = round(`Pass Yds` / `Pass Attempts`, 1)) %>%
        relocate(YPC, .after = `Rush Yds`) %>%
        relocate(`Comp%`, .after = `Pass Yds`) %>%
        pivot_longer(cols = !team_name, names_to = "stat", values_to = "value")
    } else {
      teamCompData() %>%
        select(team_name, ends_with("_def")) %>%
        rename_with(~str_remove(., "_def")) %>%
        group_by(team_name) %>%
        summarise(Completions = sum(Completions),
                  `Pass Attempts` = sum(`Pass Attempts`),
                  `Pass Yds` = sum(`Passing Yds`),
                  `Pass TDs` = sum(`Pass TDs`),
                  `Pass 20+` = sum(`Pass 20+`),
                  `Pass 40+` = sum(`Pass 40+`),
                  `Pass Longest` = max(`Pass Longest`),
                  #`Passing Air Yds` = sum(`Passing Air Yds`),
                  `YAC` = sum(`YAC`),
                  Ints = sum(Ints),
                  Carries = sum(Carries),
                  `Rush Yds` = sum(`Rush Yds`),
                  `Rush TDs` = sum(`Rush TDs`),
                  `Rush 20+` = sum(`Rush 20+`),
                  `Rush 40+` = sum(`Rush 40+`),
                  `Rush Longest` = max(`Rush Longest`),
                  Points = sum(Points)) %>%
        ungroup() %>%
        mutate(`Comp%` = round(Completions / `Pass Attempts`, 4) * 100,
               `YPC` = round(`Rush Yds` / Carries, 2),
               `Yds/Att` = round(`Pass Yds` / `Pass Attempts`, 1)) %>%
        relocate(YPC, .after = `Rush Yds`) %>%
        relocate(`Comp%`, .after = `Pass Yds`) %>%
        pivot_longer(cols = !team_name, names_to = "stat", values_to = "value")
    }
  })
  
  teamCompBarPerGame <- reactive({
    if(input$teamCompSide == "Offense"){
      teamCompData() %>%
        select(team_name, ends_with("_off")) %>%
        rename_with(~str_remove(., "_off")) %>%
        group_by(team_name) %>%
        summarise(Completions = sum(Completions),
                  `Pass Attempts` = sum(`Pass Attempts`),
                  `Pass Yds` = sum(`Passing Yds`),
                  `Pass TDs` = sum(`Pass TDs`),
                  #`Pass 20+` = sum(`Pass 20+`),
                  #`Pass 40+` = sum(`Pass 40+`),
                  #`Passing Air Yds` = sum(`Passing Air Yds`),
                  `YAC` = sum(`YAC`),
                  Ints = sum(Ints),
                  Carries = sum(Carries),
                  `Rush Yds` = sum(`Rush Yds`),
                  `Rush TDs` = sum(`Rush TDs`),
                  #`Rush 20+` = sum(`Rush 20+`),
                  #`Rush 40+` = sum(`Rush 40+`),
                  #`Rush Longest` = max(`Rush Longest`),
                  Points = sum(Points),
                  games = n()) %>%
        mutate(across(.cols = -team_name, ~round(.x/games, 2))) %>%
        select(-games) %>%
        ungroup() %>%
        mutate(`Comp%` = round(Completions / `Pass Attempts`, 4) * 100,
               `YPC` = round(`Rush Yds` / Carries, 2),
               `Yds/Att` = round(`Pass Yds` / `Pass Attempts`, 1)) %>%
        relocate(YPC, .after = `Rush Yds`) %>%
        relocate(`Comp%`, .after = `Pass Yds`) %>%
        pivot_longer(cols = !team_name, names_to = "stat", values_to = "value")
    } else {
      teamCompData() %>%
        select(team_name, ends_with("_def")) %>%
        rename_with(~str_remove(., "_def")) %>%
        group_by(team_name) %>%
        summarise(Completions = sum(Completions),
                  `Pass Attempts` = sum(`Pass Attempts`),
                  `Pass Yds` = sum(`Passing Yds`),
                  `Pass TDs` = sum(`Pass TDs`),
                  #`Pass 20+` = sum(`Pass 20+`),
                  #`Pass 40+` = sum(`Pass 40+`),
                  #`Passing Air Yds` = sum(`Passing Air Yds`),
                  `YAC` = sum(`YAC`),
                  Ints = sum(Ints),
                  Carries = sum(Carries),
                  `Rush Yds` = sum(`Rush Yds`),
                  `Rush TDs` = sum(`Rush TDs`),
                  #`Rush 20+` = sum(`Rush 20+`),
                  #`Rush 40+` = sum(`Rush 40+`),
                  #`Rush Longest` = max(`Rush Longest`),
                  Points = sum(Points),
                  games = n()) %>%
        mutate(across(.cols = -team_name, ~round(.x/games, 2))) %>%
        select(-games) %>%
        ungroup() %>%
        mutate(`Comp%` = round(Completions / `Pass Attempts`, 4) * 100,
               `YPC` = round(`Rush Yds` / Carries, 2),
               `Yds/Att` = round(`Pass Yds` / `Pass Attempts`, 1)) %>%
        relocate(YPC, .after = `Rush Yds`) %>%
        relocate(`Comp%`, .after = `Pass Yds`) %>%
        pivot_longer(cols = !team_name, names_to = "stat", values_to = "value")
    }
  })
  
  ## Team 1

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
  
  # get the main color to be used in the stacked bar chart
  # looks to see if the main color matches the first team's main color, if it is then the secondary color is ued to fill the bar
  team2_color_main <- reactive({
    team_wordmarks %>%
      filter(team_name == team_list()[2]) %>%
      mutate(selected_color = ifelse(team_color == team1_color_main(), team_color2, team_color)) %>%
      select(selected_color) %>%
      pull()
  })
  
  # get the color for the data values in the bars
  team2_color_text <- reactive({
    team_wordmarks %>%
      filter(team_name == team_list()[2]) %>%
      mutate(selected_color = ifelse(team_color == team2_color_main(), team_color2, team_color)) %>%
      select(selected_color) %>%
      pull()
  })
  
  # Stacked Bar Chart Totals
  output$teamCompBarGraphTotals <- renderPlot({
    # require a team to be selected in order to show the plot
    req(input$teamCompTeams)
    
    ggplot(data = teamCompBarData(), aes(x = factor(stat, team_comp_totals_order), 
                                         y = value, 
                                         fill = team_name)) +
      geom_col(position = position_fill(reverse = TRUE)) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = value,
                    color = team_name),
                position = position_fill(vjust = 0.5, reverse = TRUE)) +
      # text colors
      scale_color_manual(values = c(team1_color_text(), team2_color_text())) +
      guides(color = "none", fill = guide_legend(title = "Team")) +
      coord_flip() +
      # bar colors
      scale_fill_manual(values = alpha(c(team1_color_main(), team2_color_main()))) +
      labs(x = "Stats")
  })
  
  output$teamCompBarGraphPerGame <- renderPlot({
    req(input$teamCompTeams)
    
    ggplot(data = teamCompBarPerGame(), aes(x = factor(stat, team_comp_per_game_order), 
                                         y = value, 
                                         fill = team_name)) +
      geom_col(position = position_fill(reverse = TRUE)) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = value,
                    color = team_name),
                position = position_fill(vjust = 0.5, reverse = TRUE)) +
      # text colors
      scale_color_manual(values = c(team1_color_text(), team2_color_text())) +
      guides(color = "none", fill = guide_legend(title = "Team")) +
      coord_flip() +
      # bar colors
      scale_fill_manual(values = alpha(c(team1_color_main(), team2_color_main()))) +
      labs(x = "Stats")
  })
  
  output$teamCompGraph <- renderUI({
    if(input$teamCompType == "Totals"){
      plotOutput("teamCompBarGraphTotals")
    } else {
      plotOutput("teamCompBarGraphPerGame")
    }
  })
  
  ############## Player Data Table

  playerDataChoice <- reactive({
    if(input$playerTableType == "Season"){
      player_season %>%
        select(-c(player_id, headshot_url))
    } else {
      player_weekly %>%
        select(-c(player_id, position_group, headshot_url, season_type, opponent_team))
    }
  })
  
  playerData <- reactive({
    playerDataChoice() %>%
      filter(Player %in% input$playerTablePlayer)
  })
  
  # update player list based on selected positions
  observeEvent(input$playerTablePosition, {
    newData <- player_season %>%
      filter(position %in% input$playerTablePosition)
    
    updatePickerInput(session = session,
                      inputId = "playerTablePlayer",
                      choices = str_sort(unique(newData$Player)),
                      selected = unique(newData$Player))
  })
  
  output$playerDT <- renderDT({
    playerDTFreeze <- ifelse(input$playerTableType == "Season", 4, 6)
    datatable(playerData(), extensions = 'FixedColumns', options = list(scrollX = TRUE, fixedColumns = list(leftColumns = playerDTFreeze)))
  })
  
  ############## Player Profile
  
  # season data for the player profile
  playerProfileSea <- reactive({
    player_season %>%
      filter(season %in% input$playerProfSeason) %>%
      group_by(player_id, Player, position, headshot_url) %>%
      summarise(`Fantasy Points` = mean(`Fantasy Points`),
                `Fantasy Points PPR` = mean(`Fantasy Points PPR`)) %>%
      ungroup() %>%
      group_by(position) %>%
      mutate(position_rank_fp = min_rank(desc(`Fantasy Points`)),
             position_rank_ppr = min_rank(desc(`Fantasy Points PPR`))) %>%
      ungroup() %>%
      left_join(players, by = c("player_id" = "gsis_id"), suffix = c("", ".drop")) %>%
      select(-ends_with(".drop")) %>%
      filter(Player == input$playerProfPlayer)
  })
  
  # data for the top gt table as the heading based on the chosen player
  playerProfileHead <- reactive({
    playerProfileSea() %>%
      select(Player, headshot_url, position, team_abbr, uniform_number, years_of_experience, 
             `Fantasy Points`, position_rank_fp, `Fantasy Points PPR`, position_rank_ppr) %>%
      rename(fantasy_points = `Fantasy Points`) %>%
      gt() %>%
      cols_label(Player = "Name",
                 headshot_url = "",
                 position = "Pos",
                 team_abbr = "Team",
                 uniform_number = "Num",
                 years_of_experience = "Exp",
                 fantasy_points = "AVG Fantasy Points",
                 position_rank_fp = "Position Rank",
                 "Fantasy Points PPR" = "AVG Fantasy Points PPR",
                 position_rank_ppr = "Position Rank PPR") %>%
      # changes the headshot url to the image
      text_transform(
        locations = cells_body(columns = c(headshot_url)),
        fn = function(x){
          gt::web_image(x)
        }
      )
  })
  
  # player profile heading gt output
  output$playerProfileHeading <- render_gt({
    expr = playerProfileHead()
  })
  
  # weekly data for the player profile
  playerProfileWk <- reactive({
    player_weekly %>%
      filter(Player == input$playerProfPlayer) %>%
      filter(season %in% input$playerProfSeason)
  })
  
  # player profile line graph
  
  output$playerProfWkGraph <- renderPlot({
    ggplot(data = playerProfileWk(), aes(x = interaction(week, season, sep = "-"),
                                         group = 1)) +
      geom_line(aes_string(y = as.name(input$playerProfVariable)), size = 1) +
      scale_x_discrete(guide = guide_axis_nested(delim = "-")) +
      geom_hline(yintercept = mean(playerProfileWk()[[input$playerProfVariable]]), linetype = "dashed") +
      theme_bw() +
      labs(x = "Game")
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
        choices = str_sort(unique(player_season$Player)),
        selected = input$playerCompPlayers
      )
    }
  })
  
  # create a dataset based on user input
  playerCompData <- reactive({
    player_weekly %>%
      filter(Player %in% input$playerCompPlayers) %>%
      filter(season %in% input$playerCompSeason)
  })
  
  # create a list of the players names 
  player_list <- reactive({
    c(unique(playerCompData()$Player))
  })
  
  # create a list of the players positions
  player_positions <- reactive({
    c(unique(playerCompData()$Position))
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
      filter(Player == player_list()[1])
  })
  
  # player picture
  p1_headshot <- reactive({
    c(unique(player1_data()$headshot_url))
  })
  
  output$player1_pic <- renderText({
    c("<img src='", substring(p1_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  # player's average value for selected variable
  output$player1_avg <- renderText({
    paste0("Avg ", input$playerCompStat, ": ", round(mean(player1_data()[[input$playerCompStat]]), 1))
  })

  # player's median value for selected variable
  output$player1_med <- renderText({
    paste0("Med ", input$playerCompStat, ": ", median(player1_data()[[input$playerCompStat]]))
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
      filter(Player == player_list()[2])
  })
  
  # player picture
  p2_headshot <- reactive({
    c(unique(player2_data()$headshot_url))
  })
  
  output$player2_pic <- renderText({
    c("<img src='", substring(p2_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  # player's average value for selected variable
  output$player2_avg <- renderText({
    paste0("Avg ", input$playerCompStat, ": ", round(mean(player2_data()[[input$playerCompStat]]), 1))
  })
  
  # player's median value for selected variable
  output$player2_med <- renderText({
    paste0("Med ", input$playerCompStat, ": ", median(player2_data()[[input$playerCompStat]]))
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
      filter(Player == player_list()[3])
  })
  
  # player picture
  p3_headshot <- reactive({
    c(unique(player3_data()$headshot_url))
  })
  
  output$player3_pic <- renderText({
    c("<img src='", substring(p3_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  # player's average value for selected variable
  output$player3_avg <- renderText({
    paste0("Avg ", input$playerCompStat, ": ", round(mean(player3_data()[[input$playerCompStat]]), 1))
  })
  
  # player's median value for selected variable
  output$player3_med <- renderText({
    paste0("Med ", input$playerCompStat, ": ", median(player3_data()[[input$playerCompStat]]))
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
      filter(Player == player_list()[4])
  })
  
  # player picture
  p4_headshot <- reactive({
    c(unique(player4_data()$headshot_url))
  })
  
  output$player4_pic <- renderText({
    c("<img src='", substring(p4_headshot()[1], 1), "', height = '90%', width = '90%'>")
  })
  
  # player's average value for selected variable
  output$player4_avg <- renderText({
    paste0("Avg ", input$playerCompStat, ": ", round(mean(player4_data()[[input$playerCompStat]]), 1))
  })
  
  # player's median value for selected variable
  output$player4_med <- renderText({
    paste0("Med ", input$playerCompStat, ": ", median(player4_data()[[input$playerCompStat]]))
  })
  
  ## player comparison graphs
  
  # stacked bar chart data
  playerCompBarData <- reactive({
    playerCompData() %>%
      mutate(Player = factor(Player, levels = player_list())) %>%
      group_by(Player) %>%
      summarise(`Fantasy Points` = sum(`Fantasy Points`),
                `Fantasy Points PPR` = sum(`Fantasy Points PPR`),
                Completions = sum(Completions),
                Attempts = sum(Attempts),
                `Passing Yds` = sum(`Passing Yds`),
                `Passing TDs` = sum(`Passing TDs`),
                `Passing Air Yds` = sum(`Passing Air Yds`),
                `Passing YAC` = sum(`Passing YAC`),
                Interceptions = sum(Interceptions),
                Carries = sum(Carries),
                `Rushing Yds` = sum(`Rushing Yds`),
                `Rushing TDs` = sum(`Rushing TDs`),
                Receptions = sum(Receptions),
                Targets = sum(Targets),
                `Receiving Yds` = sum(`Receiving Yds`),
                `Receiving Air Yds` = sum(`Receiving Air Yds`),
                `Receiving YAC` = sum(`Receiving YAC`),
                `Receiving TDs` = sum(`Receiving TDs`)) %>%
      ungroup() %>%
      pivot_longer(cols = !Player, names_to = "stat", values_to = "value") %>%
      arrange(factor(Player, levels = player_list()))
  })
  
  # stacked bar chart
  output$playerCompBarGraph <- renderPlot({

    ggplot(data = subset(playerCompBarData(), value > 0), aes(fill = Player, x = factor(stat, player_comp_cols_order), y = value)) +
      geom_col(position = position_fill(reverse = TRUE)) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = value,
                    color = Player),
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
            panel.border = element_blank(),
            legend.position = "none") +
      scale_fill_manual(values = alpha(c("#000080", "#800080", "#C0C0C0", "#FFD700")))

  })
  
  # Player Comparison graph data
  playerCompGraphs <- reactive({
    playerCompData() %>%
      mutate(Player = factor(Player, levels = player_list()))
  })
  
  # Player Comparison Line graph
  output$playerCompLineGraph <- renderPlot({
    ggplot(data = playerCompGraphs(), aes(x = interaction(week, season, sep = "-"),
                                            group = Player, 
                                            color = Player)) +
      geom_line(aes_string(y = as.name(input$playerCompStat)), size = 1) +
      scale_x_discrete(guide = guide_axis_nested(delim = "-")) +
      theme_bw() +
      scale_color_manual(values = c("#000080", "#800080", "#C0C0C0", "#FFD700")) +
      labs(x = "Game") +
      theme(legend.position = "none")
  })
  
  # Boxplot
  output$playerCompBoxplot <- renderPlot({
    ggplot(data = playerCompGraphs(), aes(x = Player, y = .data[[input$playerCompStat]], fill = Player)) +
      geom_boxplot() +
      geom_jitter(color = "black", size = 0.4, alpha = 0.6) +
      labs(x = "") +
      scale_fill_manual(values = alpha(c("#000080", "#800080", "#C0C0C0", "#FFD700"))) +
      theme(legend.position = "none")
  })
  
  # Density
  output$playerCompDensity <- renderPlot({
    ggplot(data = playerCompGraphs(), aes(x = .data[[input$playerCompStat]], group = Player, fill = Player)) +
      geom_density(alpha = 0.4, adjust = 1.5) +
      scale_fill_manual(values = alpha(c("#000080", "#800080", "#C0C0C0", "#FFD700"))) +
      theme(legend.position = "none")
  })
})


