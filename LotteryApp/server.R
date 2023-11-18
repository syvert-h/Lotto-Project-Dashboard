library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)

function(input, output, session) {
  ### RESULTS PAGE ###
  output$results_table = DT::renderDataTable({lotto}, rownames=FALSE)
  
  ### NUMBER GENERATOR PAGE ###
  # Update dataframe used by the model
  rng_df = reactive({
    req(input$rng_filters)
    if (input$rng_filters == 1) {return(post600)} 
    else if (input$rng_filters == 2) {return(biweekly)} 
    else if (input$rng_filters == 3) {return(smartplay)} 
    else {return(lotto)} # i.e. input$rng_filters == 4
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
    ### Select relevant columns before predictions
    nCols = 4
    current_df = rng_df() %>% filter(`Draw` < input$rng_drawNo)
    if (input$rng_mode == 'Strike') {
      current_df = current_df %>% select(3:6)
    } else { # input$rng_mode == 'Lotto'
      nCols = 6
      current_df = current_df %>% select(c(3:8, 10))
    }
    ### Make predictions
    preds = NULL
    if (input$rng_method == "single") {
      if (input$rng_model == "rng") { # Uniform/Random Model
        preds = sample(1:40, size=nCols)
        if (input$rng_pb == T & input$rng_mode == 'Lotto') {preds = c(preds, sample(1:10, size=1))}
      } else if (input$rng_model %in% c("exp","rev.exp")) { # Exponential Model
        if (input$rng_pb == T & input$rng_mode == 'Lotto') {
          preds = exp_preds(df_choice=input$rng_filters, model=input$rng_model, nCols=nCols, pb=T)
        } else {
          preds = exp_preds(df_choice=input$rng_filters, model=input$rng_model, nCols=nCols, pb=F)
        }
      } else { # Bayes Model
        temp_df = current_df[,!(names(current_df) == "PB")] # remove PB column for Bayes preds
        preds = bayes_line(df=temp_df, nBalls=nCols, pb=F, ll=input$rng_model)
        if (input$rng_pb == T & input$rng_mode == 'Lotto') {
          pb_pred = bayes_line(df=current_df['PB'], nBalls=1, pb=T, ll=input$rng_model)
          preds = c(preds, pb_pred)
        }
      }
    } 
    else { # input$rng_method == "most.common"
      if (input$rng_model == "rng") {
        preds = most_common_balls(nTimes=input$rng_nTimes, nBalls=nCols, df=NULL, pb=input$rng_pb, ll=NULL, model=input$rng_model)
      } else if (input$rng_model %in% c("exp","rev.exp")) {
        preds = most_common_balls(nTimes=input$rng_nTimes, nBalls=nCols, df=NULL, pb=input$rng_pb, ll=NULL, model=input$rng_model, df_choice=input$rng_filters)
      } else { # bayes
        preds = most_common_balls(nTimes=input$rng_nTimes, nBalls=nCols, df=current_df, pb=input$rng_pb, ll=input$rng_model, model=input$rng_model)
      }
    }
    ### Store predictions
    for (i in 1:nCols) {line[i] = preds[i]} # store predictions
    if (input$rng_pb == T) {line[7] = preds[7]}
    ### Store process information
    line[8] = input$rng_mode
    convert_model = c("prop"="Proportion (Bayes)", "rev.prop"="Reverse Proportion (Bayes)", 
                      "binom"="Binomial (Bayes)", "rng"="Uniform (Random)",
                      "exp"="Exponential", "rev.exp"="Reverse Exponential")
    convert_method = c("most.common"="Most Common", "single"="Singular")
    line[9] = convert_model[input$rng_model]
    line[10] = convert_method[input$rng_method]
    line[11] = c("Exclude", "Bi-Weekly", "Smartplay", "All")[as.integer(input$rng_filters)]
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
  
  ### ODDS INFO PAGE ###
  # Extract the active column from rng_df (for Odds Information)
  rng_generated_nums = reactive({
    req(input$odds_no)
    rng_preds_df() %>% pull(input$odds_no) %>% unique() %>% as.integer()
  })
  # Update the dataframe used for graphs
  odds_df = reactive({
    req(input$odds_no)
    req(input$odds_filters)
    temp = lotto # i.e. input$odds_filters == 4
    if (input$odds_filters == 1) {temp = post600} 
    else if (input$odds_filters == 2) {temp = biweekly} 
    else if (input$odds_filters == 3) {temp = smartplay}
    return(temp %>% select("Draw","Date",input$odds_no))
  })
  # Extract the column vector for graphs (common action)
  odds_df_col = reactive({
    req(input$odds_no)
    return(odds_df() %>% pull(input$odds_no))
  })
  # Horizontal line (shape) function to add to existing graph 
  hline = function(ball_order) {
    convert_ball = c("1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "PB"=31)
    p = 1/(41-convert_ball[ball_order])
    list(type="line", x0=0, x1=1, xref="paper", y0=p, y1=p,
         line=list(color="red", dash="dot"))
  }
  # Latest 100 Draws Line Graph
  output$odds_line = renderPlotly({
    subset_odds_df = odds_df() %>% head(100)
    fig = subset_odds_df %>%
      rename("Number"=input$odds_no) %>% # for sake of easy reference
      plot_ly(x=~`Draw`, y=~`Number`, type="scatter", mode="lines+markers", 
              showlegend=F, hovertemplate=paste("Draw: %{x}<br>Number: %{y}")) %>%
      layout(
        title="Latest 100 Draws"
      )
    for (num in rng_generated_nums()) { # add horizontal line for each generated number
      fig = fig %>%
        add_lines(
          y=num,
          x=range(subset_odds_df$`Draw`),
          showlegend=F,
          line=list(color="black", dash="dot")
        )
    }
    return(fig)
  })
  # Get bar colours for plots
  get_bar_colours = function(nBalls, nums) {
    bar_colours = logical(nBalls)
    bar_colours[nums] = TRUE
    return(bar_colours)
  }
  # Probability bar plot of each ball number
  output$odds_prob_bar = renderPlotly({
    # get bar colours
    nBalls = 40
    if (input$odds_no == "PB") {nBalls = 10}
    bar_colours = get_bar_colours(nBalls, rng_generated_nums())
    # create bar plot of ball number probabilties
    bar = odds_df_col() %>%
      table() %>% prop.table() %>% as.data.frame() %>%
      rename("Number"=".", "Probability"="Freq") %>%
      plot_ly(
        x=~`Number`, y=~`Probability`, type="bar", 
        color=bar_colours,
        showlegend=F,
        hovertemplate=paste("Number: %{x}<br>Probability: %{y}")
      ) %>%
      layout(
        shapes=list(hline(input$odds_no)),
        xaxis=list(title="Number"),
        yaxis=list(title="Probability")
      )
    # create boxplot of ball number distributions
    box = plot_ly(x=~odds_df_col(), type="box") %>%
      layout(yaxis=list(showticklabels=F))
    # combine plots and finalise
    subplot(box, bar, nrows=2, shareX=T, heights = c(0.15, 0.85)) %>%
      layout(
        title="Probability per Ball Number"
      )
  })
  # Bar chart of Odd vs Even Plots
  output$odds_even_plot = renderPlotly({
    bar_colours = logical(2)
    bar_colours[(rng_generated_nums()%%2)+1] = TRUE # need to +1 since index starts at 1
    
    odds_df_col()%%2 %>%
      table() %>% prop.table() %>% as.data.frame() %>%
      rename("Odd/Even"=".", "Probability"="Freq") %>%
      mutate(
        "Odd/Even" = c("Even", "Odd"),
        "Colours" = bar_colours
      ) %>%
      plot_ly(
        x=~`Odd/Even`, y=~`Probability`, type="bar",
        color=~`Colours`, showlegend=F,
        hovertemplate=paste("Type: %{x}<br>Probability: %{y}")
      ) %>%
      layout(
        title="Odd vs. Even Probability",
        shapes=list(
          list(type="line", x0=0, x1=1, xref="paper", y0=0.5, y1=0.5,
               line=list(color="red", dash="dot"))
          )
      )
  })
  output$odds_row_plot = renderPlotly({
    ballRows = rep(8:1, 5)
    ballRowNums = sapply(odds_df_col(), function(num){ballRows[num]}) # convert to row index
    
    bar_colours = logical(8)
    bar_colours[ballRows[rng_generated_nums()]] = TRUE
    
    ballRowNums %>%
      table() %>% prop.table() %>% as.data.frame() %>%
      rename("Row Index"=".", "Probability"="Freq") %>%
      plot_ly(
        x=~`Row Index`, y=~`Probability`, type="bar",
        color=bar_colours, showlegend=F,
        hovertemplate=paste("Row: %{x}<br>Probability: %{y}")
      ) %>%
      layout(
        title="Initial Row Index Probabilities",
        shapes=list(
          list(type="line", x0=0, x1=1, xref="paper", y0=1/8, y1=1/8,
               line=list(color="red", dash="dot")) # horizontal line
        )
      )
  })
  output$odds_col_plot = renderPlotly({
    ballCols = rep(1:5, each=8)
    ballColNums = sapply(odds_df_col(), function(num){ballCols[num]}) # convert to column index
    
    bar_colours = logical(5)
    bar_colours[ballCols[rng_generated_nums()]] = TRUE
    
    ballColNums %>%
      table() %>% prop.table() %>% as.data.frame() %>%
      rename("Column Index"=".", "Probability"="Freq") %>%
      plot_ly(
        x=~`Column Index`, y=~`Probability`, type="bar",
        color=bar_colours, showlegend=F,
        hovertemplate=paste("Column: %{x}<br>Probability: %{y}")
      ) %>%
      layout(
        title="Initial Column Index Probabilities",
        shapes=list(
          list(type="line", x0=0, x1=1, xref="paper", y0=1/5, y1=1/5,
               line=list(color="red", dash="dot")) # horizontal line
        )
      )
  })
  output$odds_init_balls = DT::renderDataTable({
    # design matrix/dataframe
    tbl = matrix(1:40, nrow=8)[8:1, , drop=FALSE] %>% as.data.frame()
    colnames(tbl) = c("1","2","3","4","5")
    # datatable options
    dt_options = list(
      bPaginate=FALSE,
      bFilter=FALSE, # removes search bar
      bLengthChange=FALSE, # removes dropdown option
      columnDefs = list(list(className = 'dt-left', targets = 0:2)) # left-align
    )
    # format datatable (https://stackoverflow.com/questions/31323885/how-to-color-specific-cells-in-a-data-frame-table-in-r)
    init_tbl = DT::datatable(tbl, options=dt_options)
    if (length(rng_generated_nums()) != 0) {
      for (col in c("1","2","3","4","5")) { # formatStyle() only works in column-basis :(
        init_tbl = init_tbl %>%
          formatStyle(
            columns = col,
            background = styleEqual(rng_generated_nums(), c("lightblue"))
          )
      }
    }
    return(init_tbl)
  })
  # plot of wed vs sat line plot of ball number probabilities
  output$odds_wed_sat = renderPlotly({
    # get separate (wed/sat) dataframes
    temp_df = odds_df() %>%
      filter(`Draw` >= 1479) %>% # first Wednesday draw
      mutate(
        `Date` = as.Date(`Date`, format="%Y-%m-%d"), 
        `DoW` = weekdays(`Date`)
      )
    is_wed = temp_df$`DoW` == "Wednesday"
    wed_df = temp_df[is_wed,]
    sat_df = temp_df[!is_wed,]
    # get probabilites for each df and concatenate
    get_probs_df = function(df, dow) {
      df %>%
        pull(input$odds_no) %>%
        table() %>% prop.table() %>% as.data.frame() %>%
        rename("Number"=".", "Probability"="Freq") %>%
        mutate("DoW" = dow)
    }
    wed_df = get_probs_df(wed_df, "Wednesday")
    sat_df = get_probs_df(sat_df, "Saturday")
    all_df = bind_rows(wed_df, sat_df)
    # create plot
    all_df %>%
      plot_ly(x=~`Number`, y=~`Probability`, color=~`DoW`, type="scatter",
              hovertemplate=paste("<br>Number: %{x}<br>Probability: %{y}"),
              mode="lines+markers") %>%
      layout(
        title="Probability per Day Drawn",
        shapes=list(hline(input$odds_no)),
        hovermode="x unified"
      )
  })
  # boxplot of wait times per ball number
  output$odds_wait_plot = renderPlotly({
    # count the wait times for each ball number in that column
    ball_range = 1:40
    if (input$odds_no == "PB") {ball_range = 1:10}
    drawn_indices = lapply(ball_range, function(num) {which(odds_df_col() == num)}) # index of each number's occurrence
    col_wait_times = lapply(ball_range, function(i) {diff(drawn_indices[[i]])}) # differences between indices (i.e. wait times between draws)
    # get bar colours
    bar_colours = get_bar_colours(length(ball_range), rng_generated_nums())
    # create plot
    fig = plot_ly()
    for (i in ball_range) {
      fig = fig %>%
        add_trace(y=col_wait_times[[i]], x=i, type="box", color=bar_colours[i]) %>% # boxplot of wait times
        add_trace(y=drawn_indices[[i]][1], x=i, type="scatter", 
                  mode="markers", color=I('black'), symbol=I("x"), # having I("...") for singular value only works (probably because factor? https://stackoverflow.com/questions/52179471/add-trace-control-the-color)
                  hovertemplate=paste("<br>Number: %{x}<br>Time Last Drawn: %{y}")) # latest time since drawn
    }
    # finalise plot
    fig %>%
      layout(
        showlegend=FALSE,
        title="Time Since Ball Last Drawn",
        yaxis=list(title="Number of Draws"),
        xaxis=list(title="Ball Number")
      )
  })
}

## Old but Useful Code:
# Display dataframe
# output$odds_df = DT::renderDataTable({
#   odds_df()
# }, rownames=F, options=list(
#   bFilter=FALSE, # removes search bar
#   bLengthChange=F, # removes dropdown option
#   columnDefs = list(list(className = 'dt-left', targets = 0:2)) # left-align
#   )
# )
# # Display Rmarkdown of Analysis
# output$analysis_md = renderUI({
#   includeHTML("Lotto-Predictions-using-Bayes.html")
# })