library(DT)
library(dplyr)
library(ggplot2)
# library(data.table) # for faster file reading

function(input, output, session) {
  # read dataframe
  # lotto = fread("./data/lotto_clean.csv", sep=",",
  #               colClasses=c("integer","character", rep("integer",8))
  # )
  lotto = load_data()
  
  ### RESULTS PAGE ###
  output$results_table = DT::renderDataTable({lotto}, rownames=FALSE)
  
  ### NUMBER GENERATOR PAGE ###
  # Update dataframe used by the model
  rng_df = reactive({
    req(input$rng_filter)
    if (input$rng_filter == FALSE) {
      return(lotto %>% filter(`Draw` > 600))
    } else {return(lotto)}
  })
  # Update draw number numericInput
  observeEvent(input$navPage, {
    if (input$navPage == "generator") {
      maximum = max(rng_df()$`Draw`)
      minimum = min(rng_df()$`Draw`)
      updateNumericInput(
        session,
        "rng_drawNo",
        min=minimum,
        max=maximum+1,
        value=maximum+1
      )
    }
  })
  # Store active dataframe for prediction
  rng_preds_df = reactiveVal(
    data.frame("1"=integer(0),  "2"=integer(0), "3"=integer(0), "4"=integer(0), 
               "5"=integer(0), "6"=integer(0), "PB"=integer(0), "Model"=character(0), 
               "Restrict"=logical(0), "Method"=character(0), "All Data"=logical(0),
               check.names=F)
  )
  
  # Fill dataframe with predictions
  observeEvent(input$rng_button, {
    line = character(9)
    preds = NULL
    if (input$rng_method == "random") {
      if (input$rng_model == "rng") { # Uniform/Random Model
        preds = rand_line()
      } else { # Bayes Model
        preds = bayes_line(df=rng_df(), drawNo=input$rng_drawNo, N=6, ll=input$rng_model, restrict=input$rng_indep)
      }
    } else { # Method == "most.common"
      if (input$rng_model == "rng") {
        preds = most_common_random(nTimes=1000, N=6)
      } else { # bayes
        preds = most_common_bayes(nTimes=1000, df=rng_df(), drawNo=input$rng_drawNo, N=6, ll=input$rng_model, restrict=input$rng_indep)
      }
    }
    for (i in seq_along(preds)) {line[i] = preds[i]}
    line[7] = NA
    if (input$rng_pb == TRUE) {line[7] = sample(1:10, size=1)}
    convert_model = c("prop"="Proportion (Bayes)", "invert.prop"="Inverse Proportion (Bayes)", 
                      "binom"="Binomial (Bayes)", "rng"="Uniform (Random)")
    convert_method = c("most.common"="Most Common", "random"="RNG")
    line[8] = convert_model[input$rng_model]
    line[9] = input$rng_indep
    line[10] = convert_method[input$rng_method]
    line[11] = input$rng_filter

    before = rng_preds_df()
    before[nrow(before)+1,] = line
    rng_preds_df(before)
  })
  # Display datatable of predictions
  output$rng_table = DT::renderDataTable({ 
    rng_preds_df()
  }, options=list(bFilter=FALSE))
  # Reset dataframe of predictions
  observeEvent(input$rng_reset, {
    rng_preds_df(
      data.frame("1"=integer(0),  "2"=integer(0), "3"=integer(0), "4"=integer(0), 
                 "5"=integer(0), "6"=integer(0), "PB"=integer(0), "Model"=character(0), 
                 "Restrict"=logical(0), "Method"=character(0), "All Data"=logical(0),
                 check.names=F)
    )
  })
  
  ### ANALYSIS PAGE ###
  # # Display Rmarkdown of Analysis
  # output$analysis_md = renderUI({
  #   includeHTML("Lotto-Predictions-using-Bayes.html")
  # })
  
  ### ODDS INFO PAGE ###
  # Update the dataframe used for graphs
  odds_df = reactive({
    req(input$odds_radio)
    req(input$odds_no)
    temp = lotto
    if (input$odds_radio == FALSE) {temp = lotto %>% filter(`Draw` > 600)}
    return(temp %>% select("Draw","Date",input$odds_no))
  })
  # Display dataframe
  output$odds_df = DT::renderDataTable({
    odds_df()
  }, rownames=F, options=list(
    bFilter=FALSE, # removes search bar
    bLengthChange=F, # removes dropdown option
    columnDefs = list(list(className = 'dt-left', targets = 0:2)) # left-align
    )
  )
  # Line chart of previous results (maybe slider)
  output$odds_line = renderPlotly({
    p = odds_df() %>%
      head(100) %>%
      ggplot(aes(x=`Draw`, y=!!sym(input$odds_no))) +
      geom_point() + geom_line() + geom_smooth() +
      labs(x="Draw", y="Ball Number", title="Lastest 100 Draws") +
      theme_minimal()
    ggplotly(p) %>% toWebGL()
  })
  # Probability bar plot of each ball number
  output$odds_prob_bar = renderPlotly({
    convert = c("1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "PB"=31)
    fair = 1/(41 - convert[input$odds_no])
    p = odds_df() %>%
      ggplot(aes(x=!!sym(input$odds_no))) +
      geom_bar(aes(y=after_stat(count / sum(count)), 
                   text=sprintf("Probability: %.5f", after_stat(count/sum(count))))
               ) +
      geom_hline(yintercept=fair, color="red", linewidth=1) +
      labs(y="Probability", x="Ball Number", title="Probability per Ball Number") +
      theme_minimal()
    ggplotly(p, tooltip=c("x", "text"))
  })
  # Summary table (means, variance, median, quartiles, etc.) -- maybe boxplot?
  output$odds_summary_table = DT::renderDataTable({
    odds_df() %>%
      filter(!is.na(!!sym(input$odds_no))) %>%
      reframe(
        "Min"=min(!!sym(input$odds_no)),
        "LQ"=quantile(!!sym(input$odds_no), prob=c(.25)),
        "Mean"=round(mean(!!sym(input$odds_no)), 4),
        "Median"=median(!!sym(input$odds_no)),
        "UQ"=quantile(!!sym(input$odds_no), prob=c(.75)),
        "Max"=max(!!sym(input$odds_no)),
        "Std. Dev."=round(sd(!!sym(input$odds_no)), 4)
      )
  }, rownames=F, options=list(
    dom='t', # remove top filters
    columnDefs = list(list(className = 'dt-left', targets = 0:2)) # left-align
  ))
  # Boxplot of summary statistics
  output$odds_boxplot = renderPlotly({
    req(input$odds_no)
    avg = odds_df() %>% pull(!!sym(input$odds_no)) %>% mean(na.rm=T)
    p = odds_df() %>%
      filter(!is.na(!!sym(input$odds_no))) %>%
      ggplot(aes(x=1, y=!!sym(input$odds_no))) +
      geom_boxplot() +
      geom_hline(aes(yintercept=avg), color="red") +
      coord_flip() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x="", y="Ball Number", title="Ball Number (Percentile) Distribution") +
      theme_minimal()
    ggplotly(p) %>% toWebGL()
  })
}