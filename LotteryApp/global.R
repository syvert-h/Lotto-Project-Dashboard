library(dplyr)
library(rdrop2)

load_data = function() {
  data = drop_read_csv("Datasets/Lotto/lotto_clean.csv", check.names=FALSE)
  return(data)
}

##### Bayes Theorem #####
prepare_df = function(df, ball_i, prev, drawNo, restrict=F) {
  train = df %>% filter(`Draw` < drawNo) # Key - learn on past
  
  if (restrict == F) { ### DO NOT RESTRICT ONLY TO PAST SEQUENCE
    if (ball_i == 1) {
      train = train[,"1"]
    } else { # ball_i > 1
      train = train[,as.character(ball_i)]
      train = train[!(train %in% prev)]
    }
    return(train)
  } else { ### RESTRICT ONLY TO PAST SEQUENCES
    if (!is.null(prev)) {
      lapply(seq(along=prev), function(i){
        train = train %>% filter(!!sym(as.character(i)) == prev[i])
      })
    }
    return(train[,as.character(ball_i)])
  }
}
get_evidence = function(nums, prev) {
  ball_freq = table(nums) # observed counts
  counts = numeric(40)
  names(counts) = 1:40
  if (length(ball_freq) > 0) {counts[names(ball_freq)] = ball_freq} # have past observations
  counts = counts + 1 # add-one smoothing (with no observations, becomes uniform sampling)
  if (!is.null(prev)) {counts = counts[-prev]} # remove previous balls
  return(counts) # add-one count of balls
}
bayes_pred = function(df, drawNo, ball_i, prev=NULL, ll="binom", restrict=F) {
  ### Prepare dataset for Bayes
  train = prepare_df(df=df, ball_i=ball_i, prev=prev, restrict=restrict, drawNo=drawNo)
  prior = 1/(41-ball_i)
  
  ### Evidence
  counts = get_evidence(train, prev)
  ball_prop = prop.table(counts) # count as proportions
  
  ### Likelihood
  n = sum(counts) # number of trials - adjusted due to add-one smoothing
  k = counts # number of successes (of n trials)
  likelihoods = list(
    "prop" = ball_prop,
    "invert.prop" = (1-ball_prop)/sum((1-ball_prop)),
    "binom" = dbinom(x=k, size=n, prob=prior)
  )
  likelihood = likelihoods[[ll]]
  
  ### Posterior
  prob_h_given_e = prior * likelihood
  posterior = prob_h_given_e / sum(prob_h_given_e)
  
  ### Predictions
  balls = 1:40
  if (!is.null(prev)) {balls = balls[-prev]}
  return(sample(balls, size=1, prob=posterior)) # returns 1 ball
}

# One line of predictions using Bayes
bayes_line = function(df, drawNo, N=6, ll="binom", restrict=F) {
  balls = numeric(N)
  for (j in 1:N) {
    if (j == 1) {
      balls[j] = bayes_pred(df=df, drawNo=drawNo, ball_i=j, ll=ll, restrict=restrict)
    } else {
      balls[j] = bayes_pred(df=df, drawNo=drawNo, ball_i=j, prev=balls[1:(j-1)], ll=ll, restrict=restrict)
    }
  } # hassle trying to vectorise - updating balls did not stay after iteration (6 loops is fine)
  return(balls)
}

# One line of predictions using Uniform sampling
rand_line = function(N=6) {sample(1:40, size=N)}

# Function to replicate one line and return most common numbers
get_most_common = function(mat, N=6) {
  preds = numeric(N)
  for (i in 1:N) {
    freq_table = sort(table(mat[,i]), decreasing=T)
    j = 1
    num = as.numeric(names(freq_table)[j])
    while (num %in% preds[1:i]) {
      j = j+1
      num = as.numeric(names(freq_table)[j])
    }
    preds[i] = num
  }
  return(preds)
}
most_common_bayes = function(nTimes=100, df, drawNo, N=6, ll="binom", restrict=F) {
  replicate(nTimes, {bayes_line(df=df, drawNo=drawNo, N=N, ll=ll, restrict=restrict)}) %>%
    t() %>% get_most_common(N=N)
}
most_common_random = function(nTimes=100, N=6) {
  replicate(nTimes, {rand_line(N=N)}) %>%
    t() %>% get_most_common(N=N)
}