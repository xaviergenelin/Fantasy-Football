library(shiny)
library(shinythemes)
library(nflverse)
library(tidyverse)
library(fst)
library(DT)
library(shinyWidgets)
library(shinydashboard)

# load data to use throughout the app
player_season <- read.fst("../data/player_season.fst")
player_weekly <- read.fst("../data/player_weekly.fst")
team_weekly <- read.fst("../data/team_weekly.fst")
team_season <- read.fst("../data/team_season.fst")

# Define UI for application that analyzes NFL data
shinyUI(navbarPage(
  title = "Fantasy Football",
  theme = shinytheme("cosmo"),
  tabsetPanel(
    
    navbarMenu(
      ### About Tab
      title = "About",
               tabPanel(
                 # Overall App information
                 title = "Overall", 
                 
                 h2("Overall"),
                 "This app will be used to easily examine fantasy football data. There will be various options for a person to use to explore NFL data."),
               
               tabPanel(
                 # Team Data information
                 title = "Team Tab",
                 h2("Data Table"),
                 "The team section will have general team data from both the offense and defense",
                 "It will be broken out into different secitons: Overall, Rushing, Passing, Red Zone, Scoring, Downs. More details about each are below",
                 # Team Overall information
                 h2("Team Profile"),
                 "This will allow you to select a team and get recent statistics on them",
                 # Team Passing information
                 h3("Team Comparison"),
                 "This will allow you to compare teams side by side",
                
                 ),
               
               tabPanel(
                 # Player Data information
                 title = "Player Tab",
                 h2("Data Table"),
                 "The Player tab will have 3 different options: Data Table, Player Profile, and Player Comparison",
                 h3("Player Data"),
                 "Something",
                 h3("Player Profile"),
                 "An overview of a player",
                 h3("Player Comparison"),
                 "This will allow someone to compare players side by side"
               )
      
      ),# End of the About Tab
    
    navbarMenu(
      title = "Team",
      
      tabPanel(
        title = "Data Table",
        
        sidebarPanel(
          
          radioButtons(
            inputId = "teamDataSeaWk",
            label = "Choose the type of data",
            choices = c("Season", "Weekly"),
            selected = "Season",
            inline = TRUE
          ),
          
          radioButtons(
            inputId = "teamDataGroup",
            label = "Choose the data grouping",
            choices = c("Offense", "Defense"),
            selected = "Offense",
            inline = TRUE
          ),
          
          radioButtons(
            inputId = "teamOffDefTypes",
            label = "Stat options",
            choices = c("Overall", "Passing", "Rushing", "Scoring", "Downs", "Special Teams"),
            selected = "Overall",
            inline = TRUE
          ),
          
          pickerInput(
            inputId = "teamDataSeasons",
            label = "Select the season(s)",
            choices = sort(unique(team_weekly$season), decreasing = TRUE),
            selected = unique(team_weekly$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.teamDataSeaWk == 'Weekly'",
            pickerInput(
              inputId = "teamDataWk",
              label = "Select the week(s)",
              choices = sort(unique(team_weekly$week)),
              selected = unique(team_weekly$week),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE)
            )
          ),
          
          pickerInput(
            inputId = "teamDataTeams",
            label = "Select the team(s)",
            choices = unique(team_weekly$team_name),
            selected = unique(team_weekly$team_name),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE,
                                    liveSearch = TRUE)
          )
        ), # end of the Team Data sidebar panel
        
        mainPanel(
          DTOutput("teamDTTest")
        ) # end of the Team Data main panel
      ),
      
      tabPanel(
        title = "Team Profile",
        
        sidebarPanel(
          selectizeInput(
            inputId = "teamProfTeam",
            label = "Select Team",
            choices = unique(team_weekly$team_name),
            multiple = FALSE
          ),
          
          pickerInput(
            inputId = "teamProfSeasons",
            label = "Select season(s)",
            choices = sort(unique(team_season$season), decreasing = TRUE),
            selected = unique(team_season$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          )
        ),
        
        mainPanel(
          fluidRow(h2(textOutput("teamHeading")), align = "center"),
          fluidRow(
            column(
              # Offensive Profile
              width = 6,
              align = "center",
              h4("Offense"),
              DTOutput("teamProfOff")
            ),
            column(
              # Defensive Profile
              width = 6,
              align = "center",
              h4("Defense"),
              DTOutput("teamProfDef")
            )
          )
          
        )
      ),
      
      tabPanel(
        title = "Team Comparison",
        
        sidebarPanel(
          selectizeInput(
            inputId = "teamCompTeams",
            label = "Select Teams",
            choices = unique(team_season$team_name),
            multiple = TRUE
          )
        ),
        
        mainPanel(
          
        )
      )
      
    ), # End of the Team Panels
    
    navbarMenu(
      title = "Player",
      
      tabPanel(
        title = "Data Table",
        
        sidebarPanel(
          radioButtons(
            inputId = "playerTableType",
            label = "Choose the type of data",
            choices = c("Season", "Weekly"),
            selected = "Season",
            inline = TRUE
          ),
          
          pickerInput(
            inputId = "playerTableCateogry",
            label = "Select stat category",
            choices = c("Passing", "Rushing", "Receiving"),
            selected = c("Passing", "Rushing", "Receiving"),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          pickerInput(
            inputId = "playerTableSeason",
            label = "Select season(s)",
            choices = unique(player_season$season),
            selected = unique(player_season$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          pickerInput(
            inputId = "playerTablePosition",
            label = "Select position(s)",
            choices = unique(player_season$position),
            selected = unique(player_season$position),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          multiInput(
            inputId = "playerTablePlayer",
            label = "Select player(s)",
            choices = str_sort(unique(player_season$player_display_name)),
            selected = NULL
          )
          
        ),
        mainPanel(
          DTOutput("playerDT")
        )
      ),
      
      tabPanel(
        title = "Player Profile",
        
        sidebarPanel(
          multiInput(
            inputId = "playerProfPlayers",
            label = "Select player(s)",
            choices = str_sort(unique(player_season$player_display_name)),
            selected = NULL
          ),
          
          pickerInput(
            inputId = "playerProfSeason",
            label = "Select season(s)",
            choices = sort(unique(team_season$season), decreasing = TRUE),
            selected = unique(team_season$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          )
        ), # end of the player profile sidebar panel
        
        mainPanel(
          
        ) # end of the player profile main panel
      ),
      
      tabPanel(
        title = "Player Comparison",
        
        sidebarPanel(
          multiInput(
            inputId = "playerCompPlayers",
            label = "Select player(s)",
            choices = str_sort(unique(player_season$player_display_name)),
            selected = NULL
          ),
          
          pickerInput(
            inputId = "playerCompSeason",
            label = "Select season(s)",
            choices = sort(unique(team_season$season), decreasing = TRUE),
            selected = unique(team_season$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          )
        ), # end of the player comparison sidebar panel
        
        mainPanel(
          fluidRow(
            div(
              # First player box
              id = "player_box1",
              box(
                id = "box1",
                width = 3,
                solidHeader = TRUE,
                fluidRow(align = "center", 
                         "Name"),
                fluidRow(align = "center", 
                         "Picture"),
                fluidRow(align = "center", 
                         "Value1"),
                fluidRow(align = "center", 
                         "Value2")
              )
            ),
            div(
              # Second player 
              id = "player_box2",
              box(
                id = "box2",
                width = 3,
                fluidRow(align = "center", 
                         "Name"),
                fluidRow(align = "center", 
                         "Picture"),
                fluidRow(align = "center", 
                         "Value1"),
                fluidRow(align = "center", 
                         "Value2")
              )
            ),
            div(
              # Third player box
              id = "player_box3",
              box(
                id = "box3",
                width = 3,
                fluidRow(align = "center", 
                         "Name"),
                fluidRow(align = "center", 
                         "Picture"),
                fluidRow(align = "center", 
                         "Value1"),
                fluidRow(align = "center", 
                         "Value2")
              )
            ),
            div(
              # Fourth player box
              id = "player_box4",
              box(
                id = "box4",
                width = 3,
                fluidRow(align = "center", 
                         "Name"),
                fluidRow(align = "center", 
                         "Picture"),
                fluidRow(align = "center", 
                         "Value1"),
                fluidRow(align = "center", 
                         "Value2")
              )
            ),
            
            DTOutput("plyaerCompTest")
          ),
        ) # end of the player comparison main panel
        
      ) # end of the player comparison tab
    )
  )
) # end of navbarpage
  
  
)