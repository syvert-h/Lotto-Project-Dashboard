library(shiny)
library(plotly)
library(DT)

navbarPage(
  title="Lotto Analysis",
  id="navPage",
  
  tabPanel(
    title="Analysis",
    value="analysis", # acts as ID
    # htmlOutput("analysis_md")
    htmltools::tags$iframe(src="Lotto-Predictions-using-Bayes.html", width='100%', 
                           height=1000,  style="border:none;")
  ),
  
  tabPanel(
    title="Odds Information",
    value="odds", # acts as ID
    sidebarLayout(
      sidebarPanel(
        width=2,
        radioButtons(
          "odds_filters",
          label="Dataset Options:",
          choices=list(
            "Exlude Outliers (Recommended)"=1,
            "Bi-Weekly Draws"=2, 
            "Smartplay Draws (Latest Machine)"=3,
            "All Draws"=4
          )
        ),
        selectInput(
          "odds_no",
          label="Choose a ball order:",
          choices=c(1, 2, 3, 4, 5, 6, "Powerball"="PB") # converts to character
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
          column(width=8,
            plotlyOutput("odds_wait_plot")
          ),
          column(width=4,
            plotlyOutput("odds_even_plot")
          )
        ),
        conditionalPanel(
          condition="input.odds_no != 'PB'",
          fluidRow(
            column(width=4,
                   h4("Initial Ball Row/Column Indices"),
                   DT::dataTableOutput("odds_init_balls")
            ),
            column(width=4,
                   plotlyOutput("odds_row_plot")
            ),
            column(width=4,
                   plotlyOutput("odds_col_plot")
            )
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
          value=2000
        ),
        radioButtons(
          "rng_mode",
          label="Which game mode?",
          choices=list("Strike", "Lotto")
        ),
        radioButtons(
          "rng_filters",
          label="Dataset Options:",
          choices=list(
            "Exlude Outliers (Recommended)"=1,
            "Bi-Weekly Draws"=2, 
            "Smartplay Draws (Latest Machine)"=3,
            "All Draws"=4
          )
        ),
        radioButtons(
          "rng_method",
          label="Prediction Method:",
          choices=list("Singular"="single", "Most Likely (Slower)"="most.common")
        ),
        conditionalPanel(
          "input.rng_method == 'most.common'",
          numericInput(
            'rng_nTimes',
            label='How many resamples?',
            value=100,
            min=1,
            max=10000
          )
        ),
        radioButtons(
          "rng_model",
          label="Which model?",
          choices=list("Proportion (Bayes)"="prop", "Reverse Proportion (Bayes)"="rev.prop", 
                       "Binomial (Bayes)"="binom", "Uniform (Random)"="rng",
                       "Exponential"="exp", "Reverse Exponential"="rev.exp")
        ),
        conditionalPanel(
          "input.rng_mode == 'Lotto'",
          checkboxInput(
            "rng_pb",
            label="Include Powerball?",
            value=TRUE
          )
        ),
        actionButton(
          "rng_button",
          label="Get Line"
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
    DT::dataTableOutput("results_table")
  )
)