library(dplyr)
library(aws.s3)

### AWS S3 Bucket Setup
s3BucketName = scan("s3-bucket-name.txt", what="txt")
readRenviron(".Renviron") # read .Renviron file in this directory
Sys.setenv(
  "AWS_ACCESS_KEY_ID"=Sys.getenv("AWS_ACCESS_KEY_ID"),
  "AWS_SECRET_ACCESS_KEY"=Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  "AWS_DEFAULT_REGION"=Sys.getenv("AWS_DEFAULT_REGION")
)
# Read Analysis HTML file from AWS S3
analysis_html_s3 = rawToChar(get_object(object="Lotto-Analysis.html", bucket=s3BucketName))
# Read Lotto CSV file from AWS S3
lotto = s3read_using(
  read.csv,
  check.names=FALSE,
  object='lotto-results-clean.csv',
  bucket=s3BucketName
) %>% select(-1) %>% # ignore first column (row index)
  mutate(`Date`=as.Date(`Date`, format="%Y-%m-%d"))

### Dataset Options
lotto_clean = lotto %>% filter(`Draw` >= 575) # ignore outliers - NEW CHANGE
biweekly = lotto %>% filter(`Draw` >= 1479) # first time bi-weekly draws
smartplay = lotto %>% filter(`Date` >= "2018-05-30") # first implemented - NEW CHANGE
datasets = list(
  'Lotto' = lotto,
  'Lotto-Clean' = lotto_clean,
  'Bi-Weekly' = biweekly,
  'Smartplay' = smartplay
)

### RNG Predictions DataFrame Default
rng_df_default = data.frame(
  "1"=integer(0),  "2"=integer(0), "3"=integer(0), 
  "4"=integer(0), "5"=integer(0), "6"=integer(0), 
  "PB"=integer(0), "Mode"=character(0), "Model"=character(0), 
  "Dataset"=character(0), "Resamples"=integer(0), "Draw"=integer(0),
  check.names=F
)

### RNG Prediction Models and Line Generator
get_pred_line = function(df, mode, nTimes, model) {
  # Iterate each ball order
  preds = numeric(mode)
  for (ball_idx in 1:mode) { # need for-loop to check variable outside loop
    ball_order_vec = df[[ball_idx]]
    preds[ball_idx] = iter_ball_order(ball_idx, ball_order_vec, model, nTimes, preds)
  }
  return(preds)
}
iter_ball_order = function(ball_idx, ball_order_vec, model, nTimes, nums_drawn) {
  # Setup
  ball_range_upper = c(rep(40,6), 10)
  ball_range = 1:(ball_range_upper[ball_idx])
  p = c(1/40, 1/39, 1/38, 1/37, 1/36, 1/35, 1/10)[ball_idx]
  # Get Model Probabilities
  models = list(
    "Proportion" = get_proportion_probs(ball_order_vec),
    "Binomial" = get_binomial_probs(ball_order_vec, p),
    "Exponential" = get_exponential_probs(ball_order_vec),
    "Density" = get_density_probs(ball_order_vec),
    "Multinomial" = get_multinom_probs(ball_order_vec, p),
    "Random" = rep(p, ball_range_upper[ball_idx])
  )
  model_probs = models[[model]]
  if (ball_idx < 7) {
    if (sum(nums_drawn) != 0) { # -ve indexing not work with all-zero vector
      ball_range = ball_range[-nums_drawn] # exclude drawn balls from consideration
      model_probs = model_probs[-nums_drawn] # exclude drawn balls from consideration
    }
  }
  # Get model predictions
  preds = sample(x=ball_range, size=nTimes, replace=T, prob=model_probs)
  final_pred = get_preds_tie_break(preds)
  return(final_pred) # return vector of length 1
}
get_preds_tie_break = function(preds_vec) {
  preds_count = table(preds_vec)
  ball_nums = preds_count[preds_count == max(preds_count)] %>% 
    names() %>% as.integer()
  if (length(ball_nums) > 1) {
    return(sample(x=ball_nums, size=1)) # random tie-break
  } else {
    return(ball_nums) # vector length 1 (doesn't work with sample())
  }
  
}
# Proportion Probabilities
get_proportion_probs = function(ball_order_vec) {
  return(prop.table(table(ball_order_vec)))
}
# Binomial Probabilities
get_binomial_probs = function(ball_order_vec, p) { 
  # p := fixed probability of success
  successes = table(ball_order_vec) # number of successes for each ball number
  num_trials = sum(successes) # each draw occurrence is a trial
  binom_probs = dbinom(x=as.vector(successes), size=num_trials, prob=p)
  binom_probs = binom_probs/sum(binom_probs) # standardise to 0-1 scale
  return(binom_probs)
}
# Generate Wait Times (for Exponential, Poisson, Density models)
get_ball_order_wait_times = function(ball_order_vec) {
  ball_range = range(ball_order_vec, na.rm=T) # Note: as long as sample size large, not an issue
  ball_nums = seq(ball_range[1], ball_range[2])
  # Get time_since_drawn for each ball_num
  time_since_drawn = lapply(ball_nums, function(ball_num){which(ball_order_vec == ball_num)})
  # Get wait time between times drawn
  wait_times = lapply(time_since_drawn, diff)
  # Get the latest wait time
  current_wait_time = sapply(time_since_drawn, function(x){x[1]})
  # Get the second latest wait time
  return(list( # Note: each index represents a ball number (so iterate by ball numbers)
    "current_wait_times" = current_wait_time,
    "wait_times" = wait_times # %>% unlist() # IMPORTANT: by unlisting we assume that each ball order's wait times are IRRESPECTIVE OF INDIVIDUAL BALL NUMBERS (to avoid overfitting)
  ))
}
# Exponential Probabilities
get_exponential_probs = function(ball_order_vec) {
  ball_order_wait_times = get_ball_order_wait_times(ball_order_vec)
  rate = 1/median(unlist(ball_order_wait_times$wait_times)) # Note: using median > mean since wait times are left skew (better measure of center (i.e. typical expected value))
  exp_probs = dexp(ball_order_wait_times$current_wait_times, rate=rate)
  exp_probs = exp_probs/sum(exp_probs) # standardise to 0-1 scale
  return(exp_probs)
}
# Density Probabilities
get_density_probs = function(ball_order_vec) {
  ball_order_wait_times = get_ball_order_wait_times(ball_order_vec)
  density_fit = density(unlist(ball_order_wait_times$wait_times))
  density_probs = sapply(ball_order_wait_times$current_wait_times, function(current_wait_time, density_fit){
    max(
      0, approx(x=density_fit$x, y=density_fit$y, xout=current_wait_time)$y,
      na.rm=T
    )
  }, density_fit=density_fit)
  density_probs = density_probs/sum(density_probs) # standardise to 0-1 scale
  return(density_probs)
}
# Multinomial Probabilities
get_multinom_probs = function(ball_order_vec, p) {
  ball_range = range(ball_order_vec, na.rm=TRUE)
  ball_nums = seq(ball_range[1], ball_range[2])
  prob = rep(p, ball_range[2]) # repeat fixed probability for dmultinom()
  counts = table(ball_order_vec)
  mutlinom_probs = sapply(ball_nums, function(ball_num, counts, prob) {
    # Note: adjustments to global variables inside apply-loop ONLY APPLIES WITHIN INNER-SCOPE
    counts[ball_num] = counts[ball_num] + 1 # by adding one, we're getting the probability that the next ball is this number
    return(dmultinom(x=counts, prob=prob))
  }, counts=counts, prob=prob)
  mutlinom_probs = mutlinom_probs/sum(mutlinom_probs) # standardise to 0-1 range
  return(mutlinom_probs)
}

### Pre-Store All Dataset Wait Times Per Ball Order
general_wait_times = lapply(datasets, function(df){
  lapply(df[, 1:7], get_ball_order_wait_times)
  # Syntax: Wait Times -> [[ball_order]][[wait_times]][[ball_number]]
  # Syntax: Current Wait Times -> [[ball_order]][[current_wait_times]][ball_number]
}) # Syntax: [[df_name]][[ball_order]][[wait_time|current_wait_times]]

### Plot Helper Functions
# Get bar colours for plots
get_bar_colours = function(nBalls, nums) {
  nums = nums[nums <= nBalls] # handles case when nums > nBalls (mainly for PB)
  bar_colours = logical(nBalls)
  bar_colours[nums] = TRUE
  return(bar_colours)
}
# Horizontal line (shape) function to add to existing graph 
hline = function(ball_order) {
  convert_ball = c("1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "PB"=31)
  p = 1/(41-convert_ball[ball_order])
  list(type="line", x0=0, x1=1, xref="paper", y0=p, y1=p,
       line=list(color="red", dash="dot"))
}
# Vertical line (shape) function to add to existing graph (Wed. vs. Sat. Plot)
vline = function(x=0) {
  list(
    type = "line",
    y0 = 0, y1 = 1,
    yref = "paper",
    x0 = x-1, x1 = x-1,
    line = list(color='black', dash="dot", opacity=0.1)
  )
}