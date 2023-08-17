library(DT)
library(dplyr)
library(ggplot2)

function(input, output, session) {
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
  rng_preds_df = reactiveVal(rng_df_default)
  
  # Fill dataframe with predictions
  observeEvent(input$rng_button, {
    line = character(11)
    line[1] = input$rng_mode
    ### Select relevant columns before predictions
    nBalls = NULL
    current_df = NULL
    if (input$rng_mode == 'Strike') {
      nBalls = 4
      current_df = rng_df() %>% select(c(1, 3:6)) %>%
        filter(`Draw` < input$rng_drawNo) %>% select(-`Draw`)
    } else { # input$rng_mode == 'Lotto'
      nBalls = 6
      current_df = rng_df() %>% select(c(1, 3:8, 10)) %>%
        filter(`Draw` < input$rng_drawNo) %>% select(-`Draw`)
    }
    ### Make predictions
    preds = NULL
    if (input$rng_method == "single") {
      if (input$rng_model == "rng") { # Uniform/Random Model
        preds = sample(1:40, size=nBalls)
        if (input$rng_pb == T & input$rng_mode == 'Lotto') {preds = c(preds, sample(1:10, size=1))}
      } else { # Bayes Model
        temp_df = current_df[,!(names(current_df) %in% c("PB"))] # remove PB column for Bayes preds
        preds = bayes_line(df=temp_df, nBalls=nBalls, pb=F, ll=input$rng_model)
        if (input$rng_pb == T & input$rng_mode == 'Lotto') {
          pb_pred = bayes_line(df=current_df['PB'], nBalls=1, pb=T, ll=input$rng_model)
          preds = c(preds, pb_pred)
        }
      }
    } 
    else { # input$rng_method == "most.common"
      if (input$rng_model == "rng") {
        preds = most_common_balls(nTimes=input$rng_nTimes, nBalls=nBalls, df=NULL, pb=input$rng_pb, ll=NULL)
      } else { # bayes
        preds = most_common_balls(nTimes=input$rng_nTimes, nBalls=nBalls, df=current_df, pb=input$rng_pb, ll=input$rng_model)
      }
    }
    ### Store predictions
    for (i in 1:nBalls) {line[i+1] = preds[i]} # store predictions
    if (input$rng_pb == T) {line[8] = preds[7]}
    ### Store process information
    convert_model = c("prop"="Proportion (Bayes)", "invert.prop"="Inverse Proportion (Bayes)", 
                      "binom"="Binomial (Bayes)", "rng"="Uniform (Random)")
    convert_method = c("most.common"="Most Common", "single"="Singular")
    line[9] = convert_model[input$rng_model]
    line[10] = convert_method[input$rng_method]
    line[11] = input$rng_filter
    ### Update predictions dataframe with new row
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
    rng_preds_df(rng_df_default)
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
    ggplotly(p)
  })
  # Probability bar plot of each ball number
  output$odds_prob_bar = renderPlotly({
    convert = c("1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "PB"=31)
    fair = 1/(41 - convert[input$odds_no])
    probs = odds_df() %>% pull(!!sym(input$odds_no)) %>% table() %>% prop.table()
    probs_df = data.frame("Ball Number"=as.numeric(names(probs)), "Probability"=probs, check.names=F)[,c(1,3)]
    colnames(probs_df) = c("Ball Number", "Probability")
    p = probs_df %>%
      ggplot(aes(x=`Ball Number`, y=`Probability`)) +
      geom_bar(stat="identity") +
      geom_hline(yintercept=fair, color="red", linewidth=1) +
      labs(title="Probability per Ball Number") +
      theme_minimal() + # theme_minimal() must be before theme() to apply changes
      theme(axis.text.x = element_text(angle=90))
    ggplotly(p, tooltip=c("x", "y"))
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
    ggplotly(p)
  })
}