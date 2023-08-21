library(dplyr)
library(aws.s3)

### Get file from AWS S3 bucket ###
s3BucketName = scan("s3-bucket-name.txt", what="txt")
readRenviron(".Renviron")
Sys.setenv(
  "AWS_ACCESS_KEY_ID"=Sys.getenv("AWS_ACCESS_KEY_ID"),
  "AWS_SECRET_ACCESS_KEY"=Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  "AWS_DEFAULT_REGION"=Sys.getenv("AWS_DEFAULT_REGION")
)
lotto = s3read_using(
  read.csv, 
  object='lotto-results-clean.csv', 
  bucket=s3BucketName
  ) %>%
  select(-1) %>% # ignore first column
  rename("1"="X1", "2"="X2", "3"="X3", "4"="X4", "5"="X5", "6"="X6", "7"="X7")

# Default predictions dataframe
rng_df_default = data.frame("Mode"=character(0), "1"=integer(0),  "2"=integer(0), 
                            "3"=integer(0), "4"=integer(0), "5"=integer(0), "6"=integer(0), 
                            "PB"=integer(0), "Model"=character(0), #"Restrict"=logical(0), 
                            "Method"=character(0), "All Data"=logical(0), check.names=F)

##### Bayes Theorem #####
general_bayes = function(cols, pb, ll, prev=NULL) {
  ### Prep ###
  ball_i = 1 # i_th ball to predict
  nums = do.call(c, cols) # convert into one vector
  if (!is.null(prev)) { # filter out balls already drawn
    nums = nums[!(nums %in% prev)]
    ball_i = length(prev)+1
  }
  ### Evidence ###
  prior = ifelse(pb == T, 1/10, 1/(41-ball_i)) # prior (fair) probability
  counts = table(nums)
  props = prop.table(counts)
  ### Likelihood ###
  N = sum(counts) # number of trials
  k = counts # number of successes (being drawn) for each ball number
  likelihoods = list(
    "prop" = props,
    "invert.prop" = (1-props)/sum((1-props)),
    "binom" = dbinom(x=k, size=N, prob=prior)
  )
  likelihood = likelihoods[[ll]]
  ### Posterior ###
  prob_h_given_e = prior * likelihood
  posterior = prob_h_given_e / sum(prob_h_given_e)
  ### Predictions ###
  balls = 1:40
  if(pb == T) {balls = 1:10}
  if (!is.null(prev)) {balls = balls[-prev]}
  return(sample(balls, size=1, prob=posterior)) # returns 1 ball
}

# One line of Bayesian Predictions
bayes_line = function(df, nBalls, pb, ll) {
  preds_line = character(nBalls)
  prev = NULL
  if (nBalls == 4) {
    # Note: using for-loop since apply-loops cannot alter variables outside its scope/function
    for (i in 1:nBalls) {
      pred_num = general_bayes(cols=df[i], prev=prev, pb=pb, ll=ll)
      preds_line[i] = pred_num
      if (is.null(prev)) {prev = pred_num} else {prev = c(prev, pred_num)}
    }
  } else { # Lotto or PB -- different from above since use all df and not specific column
    for (i in 1:nBalls) {
      pred_num = general_bayes(cols=df, prev=prev, pb=pb, ll=ll)
      preds_line[i] = pred_num
      if (is.null(prev)) {prev = pred_num} else {prev = c(prev, pred_num)}
    }
  }
  return(preds_line)
}

# Many lines of Random or Bayesian Predictions
get_most_common_nums_helper = function(counts) {
  curr_max = counts[1]
  ball_nums_max = names(counts[counts == curr_max]) # ball(s) with the current max
  if (length(ball_nums_max) > 1) {ball_nums_max = sample(ball_nums_max, size=1)} # randomly sample tie-breaker
  return(ball_nums_max) # ball_nums_max is now one ball number
}
get_most_common_nums = function(tbl, N) {
  balls = numeric(N)
  if (N == 4) { # Strike
    for (i in 1:N) {
      counts = sort(table(tbl[i,]), decreasing=T)
      counts = counts[!(names(counts) %in% balls[1:i])] # remove already selected balls from contention
      chosen_ball_num = get_most_common_nums_helper(counts)
      balls[i] = chosen_ball_num
    }
  } else { # Lotto or PB
    counts = sort(table(tbl), decreasing=T)
    for (i in 1:N) {
      chosen_ball_num = get_most_common_nums_helper(counts)
      counts = counts[names(counts) != chosen_ball_num] # remove ball number from counts
      balls[i] = chosen_ball_num
    }
  }
  return(balls)
}
most_common_balls = function(nTimes, nBalls, df=NULL, pb=F, ll=NULL) {# df = NULL or lotto[1:4] (strike) or lotto[1:6, PB] (lotto)
  nums_tbl = NULL
  pb_tbl = NULL
  if (is.null(ll)) { # Random (Uniform) Methods
    # Generate numbers
    nums_tbl = replicate(nTimes, {sample(1:40, size=nBalls)})
    if (nBalls == 6 & pb == T) { # i.e. generate PB with Lotto
      pb_tbl = replicate(nTimes, {sample(1:10, size=1)})
    }
  } else { # Bayes Methods
    # Generate numbers
    temp_df = df[,!(names(df) %in% c("PB"))] # remove PB column for Bayes preds (does not exist for Strike)
    nums_tbl = replicate(nTimes, {bayes_line(df=temp_df, nBalls=nBalls, pb=F, ll=ll)})
    if (nBalls == 6 & pb == T) { # i.e. generate PB with Lotto
      pb_tbl = replicate(nTimes, {bayes_line(df=df['PB'], nBalls=1, pb=T, ll=ll)})
    }
  }
  # Get most likely numbers
  ball_preds = get_most_common_nums(tbl=nums_tbl, N=nBalls)
  if (!is.null(pb_tbl)) {
    pb_pred = get_most_common_nums(tbl=pb_tbl, N=1)
    ball_preds = c(ball_preds, pb_pred)
  }
  return(ball_preds)
}