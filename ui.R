library(shiny)
library(plotly)
library(DT)

navbarPage(
  title="Lotto Project",
  id="navPage",
  
  tabPanel(# WEIRD GLITCH -- DataTable breaks if not loaded before 'Analysis Page'; Hence placed first!
    title="Analysis",
    value="analysis", # acts as ID
    htmlOutput("analysis_html")
  ),
  
  tabPanel(
    title="Odds Information",
    value="odds", # acts as ID
    sidebarLayout(
      sidebarPanel(
        width=2,
        radioButtons(
          "odds_df_choice",
          label="Dataset:",
          choices=list(
            "All Draws (NR)"="Lotto",
            "Cleaned Draws"="Lotto-Clean",
            "Bi-Weekly Draws"="Bi-Weekly", 
            "Smartplay Draws"="Smartplay"
          ),
          selected="Lotto-Clean"
        ),
        selectInput(
          "odds_no",
          label="Ball Order:",
          choices=c(1, 2, 3, 4, 5, 6, "PB") # converts to character
        ),
        selectInput(
          "odds_num_input",
          label = "Number Choice(s):",
          choices = 1:40,
          selected = NULL,
          multiple = TRUE
        )
      ),
      
      mainPanel(
        width=10,
        fluidRow(
          column(width=7,
                 plotlyOutput("odds_prob_bar")
          ),
          column(width=5,
                 plotlyOutput("odds_line")
          )
        ),
        fluidRow(
          column(width=7,
                 plotlyOutput("odds_wait_plot")
          ),
          column(width=5,
                 h4("Ball Number Probability Ranks Per Model"),
                 DTOutput("odds_table")
          )
        ),
        fluidRow(
          column(width=12,
                 plotlyOutput("odds_wed_sat")
          )
        )
      )
    )
  ),
  
  tabPanel(
    title="Number Generator",
    value="generator", # acts as ID
    sidebarLayout(
      sidebarPanel(
        width=2,
        numericInput(
          "rng_drawNo",
          label="Draw Number:",
          value=0
        ),
        radioButtons(
          "rng_mode",
          label="Game Mode:",
          choices=c("Strike"=4, "Lotto"=6, "Powerball"=7)
        ),
        radioButtons(
          "rng_df_choice",
          label="Dataset:",
          choices=list(
            "All Draws"="Lotto",
            "Cleaned Draws"="Lotto-Clean",
            "Bi-Weekly Draws"="Bi-Weekly", 
            "Smartplay Draws"="Smartplay"
          )
        ),
        numericInput(
          'rng_nTimes',
          label='Resamples Per Line:',
          value=100,
          min=1,
          max=100000
        ),
        radioButtons(
          "rng_model",
          label="Model:",
          choices=list("Proportion", "Binomial", "Random",
                       "Exponential", "Density", "Multinomial")
        ),
        actionButton(
          "rng_button",
          label="Generate Line"
        ),
        actionButton(
          "rng_reset",
          label="Reset"
        )
      ),
      
      mainPanel(
        width=10,
        fluidRow(
          column(width=12,
                 DT::dataTableOutput("rng_table")
          )
        )
      )
    )
  ),
  
  tabPanel(
    title="Past Results",
    value="results",
    DTOutput("results_table")
  )
)