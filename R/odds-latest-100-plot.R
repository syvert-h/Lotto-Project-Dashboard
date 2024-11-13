# Create Line Plot of Latest 100 Draws
create_latest_100_line = function(df, ball_order, selected_nums) {
  # Setup
  nBalls = ifelse(ball_order == "PB", 10, 40)
  bar_colours = get_bar_colours(nBalls, selected_nums) # logical vector length 40
  latest_df = df %>% select(all_of(c("Draw", ball_order))) %>%
    # mutate(`Roll. Avg.` = round(rollmean(ball_order, k=3, fill=NA, align='left'), 2)) %>%
    head(100) %>% rename("Number"=ball_order) %>% # for sake of easy reference
    mutate(`Colour` = bar_colours[`Number`])
  # Line Plot
  trend = latest_df %>%
    plot_ly(x=~`Draw`, y=~`Number`, type="scatter", 
            mode="lines", showlegend=F, opacity=0.8, color='black') %>%
    add_trace(x=~`Draw`, y=~`Number`, color=~`Colour`, type='scatter', mode='markers',
              inherit=F, showlegend=F, hovertemplate=paste("Draw: %{x}<br>Number: %{y}")) %>%
    # add_trace(x=~`Draw`, y=~`Roll. Avg.`, type='scatter', mode='lines', showlegend=F) %>% # not showing?
    layout(title="Latest 100 Draws")
  # Boxplot Distribution
  box = latest_df %>% plot_ly(y=~`Number`, type='box', name='Number') %>%
    layout(
      xaxis=list(title = "", showticklabels=F)
    )
  # Combine Plots
  plot = subplot(trend, box, nrows=1, widths=c(0.9,0.1), shareY=T, margin=0.0) %>%
    layout(title="Latest 100 Draws")
  return(plot)
}