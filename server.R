library(DT)
library(dplyr)
library(lubridate)
library(RColorBrewer)

function(input, output, session) {
  ##### ODDS INFOMATION PAGE #####
  # Store active ball order preds (if any)
  odds_pred_nums = reactive({
    unique(rng_preds_df()[[input$odds_no]]) %>% as.integer() %>% na.omit() # NA from 'as.integer("")'
  })
  # Store all ball order predicted nums (RNG + Input)
  odds_input_nums = reactive({
    unique(c(input$odds_num_input, odds_pred_nums())) %>% as.integer()
  })
  # Store active Odds Dataset
  odds_df = reactive({ datasets[[input$odds_df_choice]] })
  # Store active Odds Ball Order
  odds_ball_order = reactive({ odds_df()[[input$odds_no]] })
  # Store active Odds Ball Order Probabilities DataFrame (commonly used by plots)
  odds_ball_order_probs_df = reactive({
    odds_ball_order() %>% table() %>% prop.table() %>%
      as.data.frame() %>% rename("Number"=".", "Probability"="Freq")
  })
  # Store active dataset+ball order wait times (per ball number)
  odds_df_ball_order_wait_times = reactive({
    general_wait_times[[input$odds_df_choice]][[input$odds_no]][["wait_times"]] # list of vectors per ball number
  })
  odds_df_ball_order_current_wait_times = reactive({
    general_wait_times[[input$odds_df_choice]][[input$odds_no]][["current_wait_times"]] # single vector (index per ball number)
  })
  # Output Lineplot of Wed. vs Sat. Probs.
  output$odds_wed_sat = renderPlotly({
    create_wed_sat_plot(input$odds_df_choice, input$odds_no, odds_input_nums())
  })
  # Output Boxplot of Wait Times Per Ball Number
  output$odds_wait_plot = renderPlotly({
    create_wait_time_boxplot(odds_df_ball_order_wait_times(), odds_df_ball_order_current_wait_times(), odds_input_nums())
  })
  # Output Lineplot of Latest 100 Draws
  output$odds_line = renderPlotly({
    create_latest_100_line(datasets[["Smartplay"]], input$odds_no, odds_input_nums())
  })
  # Output Barplot of Probabilities Per Ball Number
  output$odds_prob_bar = renderPlotly({
    create_probs_barplot(odds_ball_order_probs_df(), input$odds_no, odds_input_nums()) # requires global variable hline(), get_bar_colours()
  })
  # Output Table of Model Probabilities+Ranks
  output$odds_table = DT::renderDT({
    # Setup
    maxBall = 40
    color_palette = RColorBrewer::brewer.pal(10, "RdYlGn") %>% rep(each=4)
    selected_nums = odds_input_nums()
    if (input$odds_no == 'PB') {
      maxBall = 10
      color_palette = color_palette[seq(1, 40, by=4)]
      selected_nums = selected_nums[selected_nums <= 10]
    }
    df_content = create_odds_table(odds_ball_order(), input$odds_no, selected_nums)
    # Display DataTable
    datatable(
      df_content,
      options=list(
        bFilter=F, # hide search bar
        lengthChange = F, # hide 'show entries' dropdown above
        scrollY = 310, # set vertical scrolling height in pixels
        paging = F, # disable pagination
        info = FALSE, # hides "Showing entries..." information text below
        columnDefs = list(list(className='dt-left', targets="_all")) # left-align all columns
      ), 
      rownames=F) %>%
      formatStyle(
        'Rank',
        backgroundColor = styleInterval(
          seq(1, maxBall, length.out=maxBall-1), # intervals for a smooth gradient
          color_palette # apply the color palette
        )
      )
  })
  
  ##### NUMBER GENERATOR PAGE #####
  observeEvent(input$navPage, {
    if (input$navPage == "generator") {
      # Update Draw Number UI for RNG Page
      draws = rng_df()[["Draw"]]
      updateNumericInput(
        session,
        "rng_drawNo",
        min=draws[length(draws)],
        max=(draws[1])+1,
        value=ifelse(input$rng_drawNo <= 0, (draws[1])+1, input$rng_drawNo)
      )
    }
  })
  # Store the selected RNG DataFrame
  rng_df = reactive({ datasets[[input$rng_df_choice]] })
  # Stores the active Predictions DataFrame for RNG Page
  rng_preds_df = reactiveVal(rng_df_default)
  # Outputs current state of Predictions DataFrame
  output$rng_table = DT::renderDT({ 
    datatable(rng_preds_df(), options=list(bFilter=F, pageLength=25), rownames=F)
  })
  # Attach Prediction Line to Current Predictions DataFrame
  observeEvent(input$rng_button, {
    ### Make predictions
    temp_df = rng_df() %>% filter(`Draw` < input$rng_drawNo) # store state
    preds = get_pred_line(temp_df, input$rng_mode, input$rng_nTimes, input$rng_model)
    ### Store predictions
    line = character(12) # using since line is mixture data types
    for (i in 1:input$rng_mode) {line[i] = preds[i]} # store predictions
    ### Store process information
    line[8] = ifelse(input$rng_mode == 4, "Strike", ifelse(input$rng_mode == 6, "Lotto", "Powerball"))
    line[9] = input$rng_model
    line[10] = input$rng_df_choice
    line[11] = input$rng_nTimes
    line[12] = input$rng_drawNo
    ### Update predictions dataframe with new row
    new_pred_df = rng_preds_df() # store state before
    new_pred_df[nrow(new_pred_df)+1,] = line
    rng_preds_df(new_pred_df) # update new state of dataframe
  })
  # Reset dataframe of predictions
  observeEvent(input$rng_reset, {
    rng_preds_df(rng_df_default)
  })
  
  ##### ANALYSIS PAGE #####
  # Output Analysis HTML
  output$analysis_html <- renderText({analysis_html_s3})
  
  ##### RESULTS PAGE #####
  # Output 'Lotto' dataset as DataTable
  output$results_table = DT::renderDT({
    datatable(lotto, options=list(searchHighlight=F), rownames=F)
  })
}