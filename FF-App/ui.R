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

# Define UI for application that analyzes NFL data
shinyUI(navbarPage(
  title = "Fantasy Football",
  useShinyjs(),
  theme = shinytheme("cosmo"),
  tabsetPanel(
    
    navbarMenu(
      ### About Tab
      title = "About",
               tabPanel(
                 # Overall App information
                 title = "About the App", 
                 
                 h2("About the App"),
                 "Welcome to my fantasy football app!",
                 "This app will be used to easily examine fantasy football data. There will be various options for you to use to explore NFL data since the 2015 season.",
                 br(),
                 br(),
                 "Currently it is just a data visualization tool to explore team and player data. It is broken out between team and player data.",
                 "Each tab will have a data table where you can explore the data you're interested in, a profile for the team/player that you choose,
                 or a comparison between teams/players.",
                 "There are more details under the About tab for some features or how to interpret the visuals in case you aren't very familiar with some of them.",
                 br(),
                 br(),
                 "Eventually, I hope to build a model to predict fantasy points at a weekly level to compare to current projections and if possible for this
                 year help build some type of draft optimizer",
                 "(Ideally before the upcoming season but we'll see about that).",
                 "I'd also like to do some other nerdy statistical stuff but I'm still working on how I want to approach it.",
                 br(),
                 br(),
                 "If there are any thoughts you have about any additions you'd like to see or any modifications to what I have currently, I'd love to hear them!",
                 "None of this is set in stone and just is a starting point.",

                 h3("Some Quick Notes:"),
                 "The player comparison line graph I'm looking to show 0's for weeks a player doesn't play. Currently it just jumps to the next point and continues
                 the line. I think it can be a little cleaner.",
                 br(),
                 "I'm also still trying to figure out the colors for the player comparison. In the stacked bar chart those seemed to work well, but don't look as nice
                 in the line graph.",
                 br(),
                 "Lastly, I'm hoping to add in the ability to select columns of interest for both data tables. So instead of scrolling to see rushing or kicking data,
                 you can just select that option and those columns will appear."
                 ),
               
               tabPanel(
                 # Team Data information
                 title = "Team Tab",
                 h2("Data Table"),
                 "The data table will have general team data from both the offense and defense.",
                 #"It will be broken out into different secitons: Overall, Rushing, Passing, Scoring, Downs.",
                 "You can select from season and weekly data, as well as the teams and seasons/weeks you want to see.",
                 # Team Overall information
                 h2("Team Profile"),
                 "This will allow you to select a single team and look at their offensive, defensive, and kicking data for whatever seasons you decide to look at. 
                 It also includes NFL rankings for each category.",
                 "Offensive rankings are based on the higest values, so the team with the most passing touchdowns will be ranked first",
                 "Defense is the opposite using lowest values, so the team with the least passing touchdowns given up will be ranked first",
                 "Kicking is a mix of the two, FG/PAT made, FG/PAT Attempted, and percentages are ranked by the highest values and the rest use the lowest values.",
                 # Team Passing information
                 h2("Team Comparison"),
                 "This will allow you to compare teams side by side. This is similar to the team profile, except it shows either offensive or defensive data at once.",
                 "If you select offensive data, it will show the offensive data for both teams and compare the two using a stacked bar chart. At some point I'd like to
                 do a head-to-head comparison option as if they were playing each other."
                
                 ),
               
               tabPanel(
                 # Player Data information
                 title = "Player Tab",
                 h2("Data Table"),
                 "The data table will have player statistics in a table.",
                 "You can select between season and weekly data, as well as the positions of interest to help filter down the data.",
                 h2("Player Profile"),
                 "This gives a general overview of a player.",
                 "It will show a weekly line graph for a statistic you choose and some information about their fantasy scoring for the seasons selected.",
                 h2("Player Comparison"),
                 "This will allow you to compare up to 4 players at once side by side.",
                 "The colors of the box for each player will be the same as the color in the charts below",
                 "You will have 4 chart options to choose from: Stacked Bar Chart, Line Graph, Boxplot, Density.",
                 h4("Stacked Bar Chart"),
                 "This chart will show all total non-zero data for the players chosen during the given time period.",
                 h4("Line Graph"),
                 "This will show a line for each player for the stat selected in the given time period.",
                 "This can help show trends over time and understand certain characteristis of players throughout their careers.",
                 h4("Boxplot"),
                 "This will show a boxplot for each player for the stat selected in the given time period.",
                 "This can help show the distribution of a statistic for each player. The box will represent the middle 50% of data for that statistic.",
                 "Wider boxes mean more variability",
                 "Any dots at the tails represent outliers in the data.",
                 h4("Density"),
                 "This will show a density plot for each player for the stat selected in the given time period.",
                 "This is similar to a boxplot in that it shows the distribution for a statistic using a curve."
               )
      
      ),# End of the About Tab
    
    navbarMenu(
      title = "Team",
      useShinyjs(),
      
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
            label = NULL,
            choices = c("Offense", "Defense"),
            selected = "Offense",
            inline = TRUE
          ),
          
          # radioButtons(
          #   inputId = "teamOffDefTypes",
          #   label = "Stat options",
          #   choices = c("Overall", "Passing", "Rushing", "Scoring", "Downs", "Special Teams"),
          #   selected = "Overall",
          #   inline = TRUE
          # ),
          
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
          DTOutput("teamDT")
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
          ),
          
          radioButtons(
            inputId = "teamProfType",
            label = NULL,
            choices = c("Totals", "Per Game"),
            selected = "Totals",
            inline = TRUE
          )
        ),
        
        mainPanel(
          fluidRow(uiOutput("teamHeading"), align = "center"),
          fluidRow(
            column(
              # Offensive Profile
              width = 6,
              align = "center",
              h4("Offense"),
              uiOutput("teamProfOffCondition")
              # conditionalPanel(condition = "input$teamProfType == 'Totals'", DTOutput("teamProfOffTotals")),
              # ############# not working properly
              # conditionalPanel(condition = "input$teamProfType == 'Per Game'", DTOutput("teamProfOffPerGame"))
            ),
            column(
              # Defensive Profile
              width = 6,
              align = "center",
              h4("Defense"),
              uiOutput("teamProfDefCondition")
            )
          ),
          fluidRow(h4("Kicking"), align = "center"),
          fluidRow(uiOutput("teamProfKickCondition"))
          
        )
      ),
      
      tabPanel(
        title = "Team Comparison",
        
        sidebarPanel(
          multiInput(
            inputId = "teamCompTeams",
            label = "Select Teams",
            choices = unique(team_season$team_name),
            options = list(max = 2)
          ),
          
          pickerInput(
            inputId = "teamCompSeasons",
            label = "Select season(s)",
            choices = sort(unique(team_season$season), decreasing = TRUE),
            selected = unique(team_season$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          radioButtons(
            inputId = "teamCompSide",
            label = NULL,
            choices = c("Offense", "Defense"),
            selected = "Offense",
            inline = TRUE
          ),
          
          radioButtons(
            inputId = "teamCompType",
            label = NULL,
            choices = c("Totals", "Per Game"),
            selected = "Totals",
            inline = TRUE
          )
        ),
        
        mainPanel(
          fluidRow(
            column(
              width = 6,
              uiOutput("team1_wordmark", align = "center")
            ),
            column(
              width = 6,
              uiOutput("team2_wordmark", align = "center")
            )
          ),
          uiOutput("teamCompGraph")
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
          
          # come back to this and get it working
          # pickerInput(
          #   inputId = "playerTableCateogry",
          #   label = "Select stat category",
          #   choices = c("Passing", "Rushing", "Receiving", "Kicking"),
          #   selected = c("Passing", "Rushing", "Receiving", "Kicking"),
          #   multiple = TRUE,
          #   options = pickerOptions(actionsBox = TRUE)
          # ),
          
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
          
          pickerInput(
            inputId = "playerTablePlayer",
            label = "Select Player(s)",
            choices = str_sort(unique(player_season$Player)),
            selected = unique(player_season$Player),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE,
                                    liveSearch = TRUE)
          )
        ),
        mainPanel(
          DTOutput("playerDT")
        )
      ),
      
      tabPanel(
        title = "Player Profile",
        
        sidebarPanel(
          pickerInput(
            inputId = "playerProfPlayer",
            label = "Select Player",
            choices = str_sort(unique(player_season$Player)),
            selected = NULL,
            multiple = FALSE,
            options = pickerOptions(liveSearch = TRUE)
          ),
          pickerInput(
            inputId = "playerProfSeason",
            label = "Select season(s)",
            choices = sort(unique(team_season$season), decreasing = TRUE),
            selected = unique(team_season$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          selectInput(
            inputId = "playerProfVariable",
            label = "Select Variable",
            choices = c("Fantasy Points", "Fantasy Points PPR", "Completions", "Attempts", "Passing Yds", "Passing TDs", "Interceptions", "Passing Air Yds", 
                        "Passing YAC", "Carries", "Rushing Yds", "Rushing TDs", "Receptions", "Targets", "Receiving Yds", "Receiving TDs", "Receiving Air Yds",
                        "Receiving YAC", "Target Share", "Air Yds Share")
          )
        ), # end of the player profile sidebar panel
        
        mainPanel(
          fluidRow(gt_output("playerProfileHeading")),
          fluidRow(plotOutput("playerProfWkGraph"))
        ) # end of the player profile main panel
      ),
      
      tabPanel(
        title = "Player Comparison",
        
        sidebarPanel(
          multiInput(
            inputId = "playerCompPlayers",
            label = "Select up to 4 players",
            choices = str_sort(unique(player_season$Player)),
            selected = NULL,
            options = list(max = 4)
          ),
          
          pickerInput(
            inputId = "playerCompSeason",
            label = "Select season(s)",
            choices = sort(unique(team_season$season), decreasing = TRUE),
            selected = unique(team_season$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          radioButtons(
            inputId = "playerCompPlot",
            label = "Chart",
            choices = c("Stacked Bar Chart", "Line Graph", "Boxplot", "Density"),
            selected = "Stacked Bar Chart",
            inline = TRUE
          ),
          
          selectInput(
            inputId = "playerCompStat",
            label = "Select a Stat",
            choices = c("Fantasy Points", "Fantasy Points PPR", "Completions", "Attempts", "Passing Yds", "Passing TDs", "Interceptions", "Passing Air Yds", 
                        "Passing YAC", "Carries", "Rushing Yds", "Rushing TDs", "Receptions", "Targets", "Receiving Yds", "Receiving TDs", "Receiving Air Yds",
                        "Receiving YAC", "Target Share", "Air Yds Share"),
            selected = "Fantasy Points"
          )
        ), # end of the player comparison sidebar panel
        
        mainPanel(
          tags$head(tags$style(
            HTML(
              # first box, purple
              '.box.box-solid.box-primary{
              background:#000080;
              color: white
              }',
              # second box, purple
              '.box.box-solid.box-success{
              background:#800080;
              color: white
              }',
              # third box, grey
              '.box.box-solid.box-warning{
              background:#C0C0C0
              }',
              # fourth box, yellow
              '.box.box-solid.box-danger{
              background:#FFD700
              }'
            )
          )),
          fluidRow(
            div(
              # First player box
              id = "player_box1",
              box(
                id = "box1",
                width = 3,
                status = "primary",
                solidHeader = TRUE,
                fluidRow(align = "center", 
                         htmlOutput("player1_name")),
                fluidRow(align = "center", 
                         htmlOutput("player1_pic")),
                fluidRow(align = "center", 
                         textOutput("player1_avg")),
                fluidRow(align = "center", 
                         textOutput("player1_med"))
              )
            ),
            div(
              # Second player 
              id = "player_box2",
              box(
                id = "box2",
                width = 3,
                status = "success",
                solidHeader = TRUE,
                fluidRow(align = "center", 
                         htmlOutput("player2_name")),
                fluidRow(align = "center", 
                         htmlOutput("player2_pic")),
                fluidRow(align = "center", 
                         textOutput("player2_avg")),
                fluidRow(align = "center", 
                         textOutput("player2_med"))
              )
            ),
            div(
              # Third player box
              id = "player_box3",
              box(
                id = "box3",
                width = 3,
                status = "warning",
                solidHeader = TRUE,
                fluidRow(align = "center", 
                         htmlOutput("player3_name")),
                fluidRow(align = "center", 
                         htmlOutput("player3_pic")),
                fluidRow(align = "center", 
                         textOutput("player3_avg")),
                fluidRow(align = "center", 
                         textOutput("player3_med"))
              )
            ),
            div(
              # Fourth player box
              id = "player_box4",
              box(
                id = "box4",
                width = 3,
                status = "danger",
                solidHeader = TRUE,
                fluidRow(align = "center", 
                         htmlOutput("player4_name")),
                fluidRow(align = "center", 
                         htmlOutput("player4_pic")),
                fluidRow(align = "center", 
                         textOutput("player4_avg")),
                fluidRow(align = "center", 
                         textOutput("player4_med"))
              )
            ),
            # Overall stacked bar chart
            conditionalPanel(
              condition = "input.playerCompPlot == 'Stacked Bar Chart'",
              plotOutput("playerCompBarGraph")
            ),
            conditionalPanel(
              condition = "input.playerCompPlot == 'Line Graph'",
              plotOutput("playerCompLineGraph")
            ),
            conditionalPanel(
              condition = "input.playerCompPlot == 'Boxplot'",
              plotOutput("playerCompBoxplot")
            ),
            conditionalPanel(
              condition = "input.playerCompPlot == 'Density'",
              plotOutput("playerCompDensity")
            )
            
          ),
        ) # end of the player comparison main panel
        
      ) # end of the player comparison tab
    )
  )
) # end of navbarpage
  
  
)
