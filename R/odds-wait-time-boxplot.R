#
create_wait_time_boxplot = function(wait_times, current_wait_times, selected_nums) {
  # Setup
  nBalls = length(wait_times)
  bar_colours = get_bar_colours(nBalls, selected_nums) # logical vector length 40
  # Plot
  fig = plot_ly()
  for (i in 1:nBalls) {
    fig = fig %>%
      add_trace(y=wait_times[[i]], x=i, type="box", color=bar_colours[i]) %>% # boxplot of wait times
      add_trace(y=current_wait_times[i], x=i, type="scatter",
                mode="markers", color=I('black'), symbol=I("x"), # having I("...") for singular value only works (probably because factor? https://stackoverflow.com/questions/52179471/add-trace-control-the-color)
                hovertemplate=paste("<br>Number: %{x}<br>Time Last Drawn: %{y}"),
                name="") # latest time since drawn
  }
  fig = fig %>%
    layout(
      showlegend=FALSE,
      title="Time Since Ball Last Drawn",
      yaxis=list(title="Number of Draws"),
      xaxis=list(title="Ball Number")
    )
  return(fig)
}