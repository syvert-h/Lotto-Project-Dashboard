# Output Probability Per Ball Number (Barplot)
create_probs_barplot = function(ball_order_probs_df, ball_order, selected_nums) {
  # setup
  nBalls = ifelse(ball_order == "PB", 10, 40)
  bar_colours = get_bar_colours(nBalls, selected_nums)
  # create bar plot of ball number probabilties
  bar = ball_order_probs_df %>%
    mutate(`Colour` = bar_colours) %>%
    plot_ly(
      x=~`Number`, y=~`Probability`, type="bar", 
      color=~`Colour`,
      showlegend=F,
      hovertemplate=paste("Number: %{x}<br>Probability: %{y}")
    ) %>%
    layout(
      shapes=list(hline(ball_order)),
      xaxis=list(title="Number"),
      yaxis=list(title="Probability"),
      title="Probability per Ball Number"
    )
  return(bar)
}