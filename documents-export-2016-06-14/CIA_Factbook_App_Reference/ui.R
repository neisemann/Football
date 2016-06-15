library(shiny) 

shinyUI(fluidPage(
  img(src = "CIA_logo.jpg", align="right", height=100, width=100),
  titlePanel("Mapping Demographics From the CIA Factbook"),
  p("This application gives you the ability to choose and display a demographic world map of the specific field options listed below."),
  sidebarLayout(
    sidebarPanel(
      br(),
      
      selectInput("select_box_CIA", 
                  label = "Choose a Field to Display:",
                  choices = list("Obesity - Adult Prevalence Rate" = 'f2228', "Health Expenditures" = 'f2225',
                                 "Birth Rate" = 'f2054', "Education Expenditures" = 'f2206',
                                 "Population Growth Rate" = 'f2002', "Death Rate" = 'f2066',
                                 "Maternal Mortality Rate" = 'f2223', "Industrial Production Growth Rate" = 'f2089',
                                 "Internet Users" = 'f2153')),
      
      br(),
      
      radioButtons("color_CIA", 
                   label = "Choose a Color:",
                   choices = c("Blue" = 'Blues',
                               "Green" = 'BuGn', 
                               "Purple" = 'Purples', 
                               "Red" = 'OrRd'),
                   selected = "Purples"),

      br(),
      br(),
      
      img(src = "DataScienceinR.jpg", height=150, width=100),
      
      br(),
      br()
      
    ),
    
    mainPanel(
      br(),
      textOutput("Describe1"),
      plotOutput("map_it1"),
 
      
      p(em("This demographic data is being pulled from the ", style = "font-family: 'times'; font-si16pt"),
        a(em("CIA World Factbook.", style = "font-family: 'times'; font-si16pt"),
          href = "https://www.cia.gov/library/publications/the-world-factbook/")),
      p(em("Programming was made possible with the help of Deborah Nolan and Duncan Lang with ", style = "font-family: 'times'; font-si16pt"),
        a(em("Data Science in R", style = "font-family: 'times'; font-si16pt"),
          href = "https://www.crcpress.com/Data-Science-in-R-A-Case-Studies-Approach-to-Computational-Reasoning-and/Nolan-Lang/p/book/9781482234817"))
        )
    )
)) 