library(shiny)
library(plotly)

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
        width=3,
        radioButtons(
          "odds_radio",
          label="Use all draws?",
          choices=list("No (Recommended)"=FALSE, "Yes"=TRUE)
        ),
        selectInput(
          "odds_no",
          label="Choose a ball order:",
          choices=c(1, 2, 3, 4, 5, 6, "Bonus"="7", "Powerball"="PB") # converts to character
        )
      ),
      
      mainPanel(
        width=9,
        fluidRow(
          column(width=6,
            plotlyOutput("odds_prob_bar")
          ),
          column(width=6,
            plotlyOutput("odds_line")
          )
        ),
        fluidRow(
          column(width=8,
            fluidRow(plotlyOutput("odds_boxplot")),
            fluidRow(DT::dataTableOutput("odds_summary_table"))
          ),
          column(width=4,
            h4("Previous Results"),
            DT::dataTableOutput("odds_df")
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
        width=3,
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
          "rng_filter",
          label="Use all the data?",
          choices=list("No (Recommended)"=FALSE, "Yes"=TRUE)
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
            value=1000,
            min=1,
            max=10000
          )
        ),
        radioButtons(
          "rng_model",
          label="Which model?",
          choices=list("Proportion (Bayes)"="prop", "Inverse Proportion (Bayes)"="invert.prop", 
                       "Binomial (Bayes)"="binom", "Uniform (Random)"="rng")
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
        width=9,
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