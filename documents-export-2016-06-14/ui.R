library(shiny) 

shinyUI(fluidPage(
  img(src = "NFLlogo.jpg", align="right", height=100, width=100),
  titlePanel("Simulating Two NFL Teams Against Eachother"),
  p("This application gives you the ability to choose and display two NFL teams and have them play against each other head to head using the 2015 statistics."),
  
  fluidRow(
    
    column(3,
      selectInput("select_box_NFL_A", 
                  label = h6("Choose your first team:"),
                  choices = list("Arizona Cardinals" = "ARI", "Atlana Falcons" = "ATL", 
                                 "Baltimore Ravens" = "BAL", "Buffalo Bills" = "BUF",
                                 "Carolina Panthers" = "CAR", "Chicago Bears" = "CHI",
                                 "Cincinnati Bengals" = "CIN", "Cleveland Browns" = "CLE",
                                 "Dallas Cowboys" = "DAL", "Denver Bronces" = "DEN",
                                 "Detroit Lions" = "DET", "Green Bay Packers" = "GB",
                                 "Houston Texans" = "HOU", "Indianapolis Colts" = "IND",
                                 "Jacksonville Jaguars" = "JAC", "Kansas City Chiefs" = "KC",
                                 "Miami Doplhins" = "MIA", "Minnesota Vikings" = "MIN",
                                 "New England Patriots" = "NE", "New Orleans Saints" = "NO",
                                 "New York Giants" = "NYG", "New York Jets" = "NYJ",
                                 "Oakland Raiders" = "OAK", "Philadelphia Eagles" = "PHI",
                                 "Pittsburgh Steelers" = "PIT", "San Diego Chargers" = "SD",
                                 "Seattle Seahawks" = "SEA", "San Francisco 49ers" = "SF",
                                 "St. Louis Rams" = "STL", "Tampa Bay Buccaneers" = "TB",
                                 "Tennessee Titans" = "TEN", "Washington Redskins" = "WAS")),
    
      selectInput("select_box_NFL_B", 
                  label = h6("Choose your second team:"),
                  choices = list("Arizona Cardinals" = "ARI", "Atlana Falcons" = "ATL", 
                                 "Baltimore Ravens" = "BAL", "Buffalo Bills" = "BUF",
                                 "Carolina Panthers" = "CAR", "Chicago Bears" = "CHI",
                                 "Cincinnati Bengals" = "CIN", "Cleveland Browns" = "CLE",
                                 "Dallas Cowboys" = "DAL", "Denver Bronces" = "DEN",
                                 "Detroit Lions" = "DET", "Green Bay Packers" = "GB",
                                 "Houston Texans" = "HOU", "Indianapolis Colts" = "IND",
                                 "Jacksonville Jaguars" = "JAC", "Kansas City Chiefs" = "KC",
                                 "Miami Doplhins" = "MIA", "Minnesota Vikings" = "MIN",
                                 "New England Patriots" = "NE", "New Orleans Saints" = "NO",
                                 "New York Giants" = "NYG", "New York Jets" = "NYJ",
                                 "Oakland Raiders" = "OAK", "Philadelphia Eagles" = "PHI",
                                 "Pittsburgh Steelers" = "PIT", "San Diego Chargers" = "SD",
                                 "Seattle Seahawks" = "SEA", "San Francisco 49ers" = "SF",
                                 "St. Louis Rams" = "STL", "Tampa Bay Buccaneers" = "TB",
                                 "Tennessee Titans" = "TEN", "Washington Redskins" = "WAS")),
      br(),
      p(em("You are adjusting only the first team strategies, the second team will perform traditional strategies"), style = "font-family: 'times'; font-si16pt")                                        
    ),
    
    
    column(4,
           
      sliderInput("num_sims", label = h6("Number of Simulations"),
                  min = 0, max = 1000, value = 100),
      
      sliderInput("punt_yard_line", label = h6("Choose a Yard Line to Punt From"),
                  min = 0, max = 100, value = 64),
      
      sliderInput("fourth_yard_line", label = h6("When to go for it on Fourth"),
                  min = 0, max = 100, value = 100),
      br(),
      
      p(em("This is set to the traditional method where they will never go for it on fourth down"), style = "font-family: 'times'; font-si16pt")
    ),
      
    column(5,
      
      sliderInput("FG_min_yard_line", label = h6("Minimum Yard Line to Kick A Field Goal From"),
                  min = 0, max = 100, value = 65),
      
      sliderInput("FG_max_yard_line", label = h6("Maximum Yard Line to Kick A Field Goal From"),
                  min = 0, max = 100, value = 99),
      
      
      h6("Click When Ready"),
      actionButton("submit", label = "Play")

    )),
    
    mainPanel(
      br(),
      plotOutput("TwoTeamSimulation_1"),
 
      
      p(em("This data is being pulled from the ", style = "font-family: 'times'; font-si16pt"),
        a(em("CIA World Factbook.", style = "font-family: 'times'; font-si16pt"),
          href = "https://www.cia.gov/library/publications/the-world-factbook/")),
      p(em("Programming was made possible with the help of Deborah Nolan and Duncan Lang with ", style = "font-family: 'times'; font-si16pt"),
        a(em("Data Science in R", style = "font-family: 'times'; font-si16pt"),
          href = "https://www.crcpress.com/Data-Science-in-R-A-Case-Studies-Approach-to-Computational-Reasoning-and/Nolan-Lang/p/book/9781482234817"))
        )
    )
)