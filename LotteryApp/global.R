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
  select(-1) %>% # ignore first column (row index)
  rename("1"="X1", "2"="X2", "3"="X3", "4"="X4", "5"="X5", "6"="X6", "7"="X7")
# # Temporary - local purposes only:
# lotto = read.csv("./data/lotto_clean.csv", check.names=F)
### ###

### Dataset Options ###
post600 = lotto %>% filter(`Draw` > 600) # excludes outlier draws
biweekly = lotto %>% filter(`Draw` >= 1479) # first time bi-weekly draws
smartplay = lotto %>%
  filter(`Date` >= "2018-05-30") %>% # first implemented
  filter(!(`Date` >= "2019-10-22" & `Date` <= "2019-10-29")) %>% # fire event
  filter(!(`Date` >= "2020-03-25" & `Date` <= "2019-04-27")) # L4 lockdown
# Note: Lotto is the whole dataset unfiltered
### ###

### Default Predictions Dataframe Layout ###
rng_df_default = data.frame("1"=integer(0),  "2"=integer(0), "3"=integer(0), 
                            "4"=integer(0), "5"=integer(0), "6"=integer(0), 
                            "PB"=integer(0), "Mode"=character(0),
                            "Model"=character(0), "Method"=character(0), 
                            "Dataset"=character(0), 
                            check.names=F)
### ###

### Exponential Probabilities for All Datasets ###
rev_exp_probs = function(ball_order_vec) { 
  middle = mean(ball_order_vec)
  reverse = middle - (ball_order_vec - middle) # try middle as 1/10 (tbc)
  reverse = reverse + abs(min(reverse) - 1e-4) # shift up incase of negatives (plus some to avoid 0 probability)
  reverse/sum(reverse) # normalise
}
get_exp_probs = function(df, nBalls=40) {
  ## Get the time since drawn for each ball number in each ball column
  time_since_drawn = lapply(df, function(df_col){
    lapply(1:nBalls, function(ball_num){which(df_col == ball_num)})
  }) # Returns a list of ball columns - each ball column list is a list of vectors for each ball number
  ## Get the wait time (difference) between each time since drawn
  wait_times = lapply(time_since_drawn, function(ball_col_lst){
    lapply(ball_col_lst, diff)
  }) # Returns a list of ball columns - each ball column list is a list of vectors for each ball number
  ## Extract the latest wait times for each ball number in each ball column
  latest_time_since_drawn = lapply(time_since_drawn, function(ball_col_lst){
    sapply(ball_col_lst, function(ball_num_vec){ball_num_vec[1]})
  }) # Returns a list of ball columns - each ball column is a VECTOR of length nBalls (10 or 40)
  ## Calculate average wait time rates (lambda) for each ball number in each ball column
  wait_times_rates = lapply(wait_times, function(ball_col_lst) {
    sapply(ball_col_lst, function(ball_num_lst){1/mean(ball_num_lst)})
  }) # Returns a list of ball columns - each ball column is a VECTOR of length nBalls (10 or 40)
  ## Calculate Exponential probability of the latest wait time for each ball number in each ball column
  probs = lapply(1:ncol(df), function(ball_col_i) {
    ball_num_odds = sapply(1:nBalls, function(ball_num) {
      dexp(
        x=latest_time_since_drawn[[ball_col_i]][ball_num],
        rate=wait_times_rates[[ball_col_i]][ball_num]
      )
    })
    ball_num_odds/sum(ball_num_odds) # make probability (0-1 normalise)
  })
  return(probs) # Returns a list of ball columns - each ball column is a VECTOR of exponential probabilities for each ball number
}
# Lotto
exp_probs_lotto = get_exp_probs(df=lotto[,3:8], nBalls=40)
rev_exp_probs_lotto = lapply(exp_probs_lotto, rev_exp_probs)
exp_probs_lotto_pb = get_exp_probs(df=data.frame(lotto[,10]), nBalls=10)
rev_exp_probs_lotto_pb = lapply(exp_probs_lotto_pb, rev_exp_probs)
# Post 600
exp_probs_post600 = get_exp_probs(df=post600[,3:8], nBalls=40)
rev_exp_probs_post600 = lapply(exp_probs_post600, rev_exp_probs)
exp_probs_post600_pb = get_exp_probs(df=data.frame(post600[,10]), nBalls=10)
rev_exp_probs_post600_pb = lapply(exp_probs_post600_pb, rev_exp_probs)
# Bi-Weekly
exp_probs_biweekly = get_exp_probs(df=biweekly[,3:8], nBalls=40)
rev_exp_probs_biweekly = lapply(exp_probs_biweekly, rev_exp_probs)
exp_probs_biweekly_pb = get_exp_probs(df=data.frame(biweekly[,10]), nBalls=10)
rev_exp_probs_biweekly_pb = lapply(exp_probs_biweekly_pb, rev_exp_probs)
# Smartplay
exp_probs_smartplay = get_exp_probs(df=smartplay[,3:8], nBalls=40)
rev_exp_probs_smartplay = lapply(exp_probs_smartplay, rev_exp_probs)
exp_probs_smartplay_pb = get_exp_probs(df=data.frame(smartplay[,10]), nBalls=10)
rev_exp_probs_smartplay_pb = lapply(exp_probs_smartplay_pb, rev_exp_probs)
### ###

### Exponential Predictions ###
# Function to calculate Exponential probabilities
# rev_exp_probs = function(ball_order_vec) {
#   middle = median(ball_order_vec)
#   reverse = middle - (ball_order_vec - middle)
#   reverse = reverse + abs(min(reverse) - 1e-4) # shift up incase of negatives (plus some to avoid 0 probability)
#   reverse/sum(reverse) # normalise
# }
# exp_probs = function(df, nBalls, rev=F, pb=F) {
#   time_since_drawn = lapply(1:nBalls, function(idx){which(df[,idx] == num)}) # Get the times since ball drawn for each ball number
#   wait_times = lapply(time_since_drawn, diff) # Calculate the wait times (difference) between each draw for each ball number
#   latest_time_since_drawn = lapply(time_since_drawn, function(ball_order_times_lst){ball_order_times_lst[1]}) # get the latest wait times for each number in each ball order
#   wait_times_rates = lapply(wait_times, function(ball_order_lst) {
#     sapply(ball_order_lst, function(ball_num_lst){1/mean(ball_num_lst)})
#   }) # i.e. lambda's (average/constant rate of events) for each number in each ball order
#   probs = lapply(1:nBalls, function(ball_order) {
#     nBallNums = 40
#     if (pb == T) {nBallNums = 10}
#     ball_num_odds = sapply(1:nBallNums, function(ball_num) {
#       dexp(
#         x=latest_time_since_drawn[[ball_order]][ball_num],
#         rate=wait_times_rates[[ball_order]][ball_num]
#       )
#     })
#     ball_num_odds/sum(ball_num_odds) # make probability (0-1 normalise)
#   }) # for every ball number in every ball order, get their exponential probability of the latest/current wait times
#   if (rev == T) {probs = lapply(probs, rev_exp_probs)}
#   return(probs) # probs is a list - each list item is a ball order vector - each vector item is the exponential probability for the ball number (index)
# }
exp_preds = function(df_choice, model, nCols, pb) { # Generate one line of predictions
  # Preparation
  datasets = list( # Note: 1=post600, 2=biweekly, 3=smartplay, 4=all
    "1-exp"=exp_probs_post600,
    "1-rev.exp"=rev_exp_probs_post600,
    "1-exp_pb"=exp_probs_post600_pb,
    "1-rev.exp_pb"=rev_exp_probs_post600_pb,
    "2-exp"=exp_probs_biweekly,
    "2-rev.exp"=rev_exp_probs_biweekly,
    "2-exp_pb"=exp_probs_biweekly_pb,
    "2-rev.exp_pb"=rev_exp_probs_biweekly_pb,
    "3-exp"=exp_probs_smartplay,
    "3-rev.exp"=rev_exp_probs_smartplay,
    "3-exp_pb"=exp_probs_smartplay_pb,
    "3-rev.exp_pb"=rev_exp_probs_smartplay_pb,
    "4-exp"=exp_probs_lotto,
    "4-rev.exp"=rev_exp_probs_lotto,
    "4-exp_pb"=exp_probs_lotto_pb,
    "4-rev.exp_pb"=rev_exp_probs_lotto_pb
  )
  key = paste0(df_choice, "-", model)
  exp_df = datasets[[key]]
  # Predictions
  preds = numeric(nCols)
  for (ball_i in 1:nCols) { # Note: exponential datasets only use latest data (result only stored for latest)
    temp_probs = exp_df[[ball_i]]
    temp_probs[preds] = 0 # replace ball numbers already drawn with 0
    preds[ball_i] = sample(1:40, size=1, prob=temp_probs) # Note: sample(..., replace=F) will auto normalise probs to equal 1 if not already (More: https://stackoverflow.com/questions/59918865/what-happens-when-prob-argument-in-sample-sums-to-less-greater-than-1)
  }
  if (pb == T) {
    key = paste0(key, "_pb")
    exp_df_pb = datasets[[key]]
    pb_pred = sample(1:10, size=1, prob=exp_df_pb[[1]])
    preds = c(preds, pb_pred)
  }
  return(preds)
}

##### Bayes Theorem #####
general_bayes = function(cols, pb, ll, prev=NULL) {
  ### Prep ###
  ball_i = length(prev)+1 # (default) i_th ball to predict
  nums = unlist(cols, use.names=F) # convert into one vector
  ### Evidence ###
  prior = ifelse(pb == T, 1/10, 1/(41-ball_i)) # prior (fair) probability
  counts = table(nums)
  if (!is.null(prev)) {counts = counts[-prev]} # filter out balls already drawn
  props = prop.table(counts)
  ### Likelihood ###
  N = sum(counts) # number of trials
  k = counts # number of successes (being drawn) for each ball number
  likelihoods = list(
    "prop" = props,
    "rev.prop" = rev_prop_bayes(props),
    "binom" = dbinom(x=k, size=N, prob=prior)
  )
  likelihood = likelihoods[[ll]]
  ### Posterior ###
  prob_h_given_e = prior * likelihood
  posterior = prob_h_given_e / sum(prob_h_given_e)
  ### Predictions ###
  balls = as.integer(names(counts))
  return(sample(balls, size=1, prob=posterior)) # returns 1 ball
}
# Reverse Proportion for General Bayes
rev_prop_bayes = function(props) { # takes vector of probs (for each number)
  middle = median(props)
  reverse = middle - (props - middle) # take the reverse
  reverse = reverse + abs(min(reverse) - 1e-4) # shift up incase of negatives (plus some to avoid 0 probability)
  reverse/sum(reverse) # normalise
}
# One line of Bayesian Predictions
bayes_line = function(df, nBalls, pb, ll) {
  preds_line = numeric(nBalls)
  prev = NULL
  # if (nBalls == 4) {
  # Note: using for-loop since apply-loops cannot alter variables outside its scope/function
  for (i in 1:nBalls) {
    pred_num = general_bayes(cols=df[i], prev=prev, pb=pb, ll=ll)
    preds_line[i] = pred_num
    prev = preds_line[1:i]
  }
  # } else { # Lotto or PB -- different from above since use all df and not specific column
  #   for (i in 1:nBalls) {
  #     # Note: Lotto pass whole dataframe since ordering does not matter
  #     pred_num = general_bayes(cols=df, prev=prev, pb=pb, ll=ll)
  #     preds_line[i] = pred_num
  #     prev = preds_line[1:i]
  #   }
  # }
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
most_common_balls = function(nTimes, nBalls, df=NULL, pb=F, ll=NULL, model, df_choice=NULL) {# df = NULL or lotto[1:4] (strike) or lotto[1:6, PB] (lotto)
  nums_tbl = NULL
  pb_tbl = NULL
  if (model %in% c("rng","exp","rev.exp")) { # Random (Uniform) or Exponential Methods
    if (model == "rng") { # Random/Uniform Method
      # Generate numbers
      nums_tbl = replicate(nTimes, {sample(1:40, size=nBalls)})
      if (nBalls == 6 & pb == T) { # i.e. generate PB with Lotto
        pb_tbl = replicate(nTimes, {sample(1:10, size=1)})
      }
    } else { # Exponential Method
      if (nBalls == 6 & pb == T) {
        temp_nums_tbl = replicate(nTimes, {exp_preds(df_choice=df_choice, model=model, nCols=nBalls, pb=T)})
        nums_tbl = temp_nums_tbl[1:nBalls,]
        pb_tbl = temp_nums_tbl[nBalls+1,]
      } else {
        nums_tbl = replicate(nTimes, {exp_preds(df_choice=df_choice, model=model, nCols=nBalls, pb=F)})
      }
    }
  } else { # Bayes Methods
    # Generate numbers
    temp_df = df[,!(names(df) == "PB")] # remove PB column for Bayes preds (does not exist for Strike)
    nums_tbl = replicate(nTimes, {bayes_line(df=temp_df, nBalls=nBalls, pb=F, ll=ll)})
    if (nBalls == 6 & pb == T) { # i.e. generate PB with Lotto
      pb_tbl = replicate(nTimes, {bayes_line(df=df['PB'], nBalls=1, pb=T, ll=ll)})
    }
  }
  # Get most likely numbers
  ball_preds = get_most_common_nums(tbl=nums_tbl, N=nBalls)
  if (!is.null(pb_tbl)) {
    pb_pred = get_most_common_nums(tbl=pb_tbl, N=1)
    ball_preds = append(ball_preds, pb_pred)
  }
  return(ball_preds)
}