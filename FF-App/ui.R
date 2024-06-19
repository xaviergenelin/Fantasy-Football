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
                 title = "Team Data",
                 h2("Team Data"),
                 "The team section will have general team data from both the offense and defense",
                 "It will be broken out into different secitons: Overall, Rushing, Passing, Red Zone, Scoring, Downs. More details about each are below",
                 # Team Overall information
                 h3("Overall"),
                 "General stats across all aspects of the game for each team",
                 # Team Passing information
                 h3("Passing"),
                 "This will have more in depth stats about passing",
                 # Team Rushing information
                 h3("Rushing"),
                 "This will have more in depth stats about rushing",
                 # Team Red Zone information
                 h3("Red Zone"),
                 "Red Zone secific stats",
                 # Team Downs information
                 h3("Downs"),
                 "This will have general information about each down"
                 ),
               
               tabPanel(
                 # Player Data information
                 title = "Player Data",
                 h2("Player Data"),
                 "Explain this section")
      
      
      ),# End of the About Tab
    
    tabPanel(
      title = "Team",
      
      sidebarPanel(
        
        radioButtons(
          inputId = "teamDataType",
          label = "Choose the type of data",
          choices = c("Season", "Weekly"),
          selected = "Season",
          inline = TRUE
        ),
        
        radioButtons( # Offense, Defense, ST selector
          inputId = "teamGroup",
          label = "Choose team grouping",
          choices = c("Offense", "Defense", "Special Teams"),
          selected = "Offense",
          inline = TRUE
        ),
        
        conditionalPanel( # data types based on the selected Group = Offense or Defense
          condition = "input.teamGroup == 'Offense' || input.teamGroup == 'Defense'",
          radioButtons(
            inputId = "teamOffDefTypes",
            label = "Stat options",
            choices = c("Overall", "Passing", "Rushing", "Receiving", "Scoring", "Downs"),
            selected = "Overall",
            inline = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.teamGroup == 'Special Teams'",
          radioButtons(
            inputId = "teamSPTypes",
            label = "Stat options",
            choices = c("Field Goals", "Scoring", "Kick Returns", "Punt Returns", "Kickoffs", "Punting"),
            selected = "Field Goals",
            inline = TRUE
          )
        )
        
      ), # End of Team sidebarPanel
      
      mainPanel(
        DTOutput("teamDT")
      ) # End of Team mainPanel
    ), # End of the Team Panels
    
    navbarMenu(
      title = "Player"
    )
  )
) # end of navbarpage
  
  
)
