# Return DataFrame containing Model Probabilities + Ranks of Input Numbers
create_odds_table = function(ball_order_vec, ball_order, selected_nums) {
  if (length(selected_nums) < 1) {
    return(
      data.frame(
        "Number" = "",
        "Model" = "",
        "Probability" = "",
        "Rank" = "",
        row.names=F
      )
    )
  } else {
    # Setup
    ball_idx = c('1'=1, '2'=2, '3'=3, '4'=4, '5'=5, '6'=6, "PB"=7)[ball_order]
    p = c(1/40, 1/39, 1/38, 1/37, 1/36, 1/35, 1/10)[ball_idx]
    nums_models_df = lapply(selected_nums, get_ball_num_models, ball_order_vec, p) %>% # iterate each selected number
      bind_rows() # a DataFrame of models for all ball numbers
    return(nums_models_df)
  }
}

get_ball_num_models = function(ball_num, ball_order_vec, p) {
  # Get Model Probabilities
  models = c("Proportion", "Binomial", "Exponential", "Density", "Multinomial")
  ball_num_models_df = lapply(models, get_model_probs_rank, ball_order_vec, p, ball_num) %>%
    bind_rows() # a DataFrame of models for one ball number
  return(ball_num_models_df)
}
get_model_probs_rank = function(model, ball_order_vec, p, ball_num) {
  # Get Model Probabilities
  models = list(
    "Proportion" = get_proportion_probs(ball_order_vec),
    "Binomial" = get_binomial_probs(ball_order_vec, p),
    "Exponential" = get_exponential_probs(ball_order_vec),
    "Density" = get_density_probs(ball_order_vec),
    "Multinomial" = get_multinom_probs(ball_order_vec, p)
  )
  model_probs = models[[model]]
  # Get Probability Rank
  prob_ranks = rank(model_probs, ties.method='first') # Ranks tied values by their first appearance; higher probs. closer to rank 40; lower probs. closer to 1
  # Return Probability + Rank
  ball_num = as.integer(ball_num) # in-case
  return(
    data.frame(
      "Number" = ball_num,
      "Model" = model,
      "Probability" = round(model_probs[ball_num], 4),
      "Rank" = prob_ranks[ball_num]
    )
  )
}