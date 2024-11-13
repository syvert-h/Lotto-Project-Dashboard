#
create_wed_sat_plot = function(df_choice, ball_order, selected_nums) {
  # Setup DataFrame
  df = datasets[[df_choice]]
  if (df_choice %in% c("Lotto","Lotto-Clean")) {df = datasets[["Bi-Weekly"]]}
  # nBalls = ifelse(ball_order == "PB", 10, 40)
  # bar_colours = get_bar_colours(nBalls, selected_nums)
  temp_df = df %>% # given 'Bi-Weekly' dataset
    mutate(`DoW` = weekdays(`Date`)) %>% # attach DoW
    group_by(`DoW`) %>%
    group_modify(~get_dow_probs(.x, ball_order)) %>%
    ungroup() # %>% # attaches grouped column
    # mutate(`Colour` = bar_colours[`Number`])
  # Setup Lines
  if (ball_order == "PB") {selected_nums = selected_nums[selected_nums <= 10]}
  line_shapes = lapply(selected_nums, vline) # lines
  line_shapes = append(line_shapes, list(hline(ball_order))) # append to list of shapes
  # Plot
  line = temp_df %>%
    plot_ly(x=~`Number`, y=~`Probability`, color=~`DoW`, type="scatter", mode="lines+markers",
            hovertemplate=paste("<br>Number: %{x}<br>Probability: %{y}")) %>%
    layout(
      title="Probability Per Day Drawn",
      shapes=line_shapes,
      hovermode="x unified"
    )
  return(line)
}

get_dow_probs = function(grouped_df, ball_order) {
  grouped_df %>% pull(ball_order) %>%
    table() %>% prop.table() %>% as.data.frame() %>%
    rename("Number"=".", "Probability"="Freq")
}