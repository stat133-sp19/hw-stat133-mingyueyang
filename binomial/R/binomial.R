#1.1) Private Checker Functions

#Description: a private auxiliary function check_prob()
#  to test if an input prob is a valid probabilityvalue (i.e. 0 <= p <= 1)
#Input
#  prob: probability,numeric value
#Output
#Return TRUE if valid, return error if invalid
check_prob <- function(prob){
  if(!is.numeric(prob)){
    stop("invalid probability value")
  }
  if(length(prob) > 1){
    stop("invalid probability length")
  }
  if(prob < 0 | prob > 1){
    stop("p has to be a number between 0 and 1")
  }
  else{
    TRUE
  }
}


#Description: a private auxiliary function check_trials()
#  to test if an input trials is a valid value
#  for number of trials (i.e. n is a non-negative integer)
#Input
#  trials: number of trials,numeric value,nonnegative integer
#Output
#  Return TRUE if valid, return error if invalid
check_trials <- function(trials){
  if(!is.numeric(trials)){
    stop("invalid trials value")
  }
  if(length(trials) > 1){
    stop("invalid trials length")
  }
  if(trials < 0 | floor(trials) != trials){
    stop("The number of trials has to be a nonnegative integer")
  }
  else{
    TRUE
  }
}


#Description: a private auxiliary function check_success()
#  to test if an input success
#  is a valid value for number of successes (i.e. 0 <= k <= n)
#Input
#  success: number of successes, a vector of nonnegative integers
#Output
#  Return TRUE if valid, return error if invalid
check_success <- function(success,trials){
  if(!is.numeric(success)){
    stop("invalid success value")
  }
  if(any(floor(success) != success) | any(success < 0)){
    stop("success must be a vector of non-negative integers")
  }
  if(any(success > trials)){
    stop("success cannot be greater than trials")
  }
  TRUE
}


#1.2) Private Auxiliary Functions


#Description: a private auxiliary function aux_mean()
#  aux_mean returns the mean of the binomial experiment
#Input
#  trials: number of trials, numeric value
#  prob: probability,numeric value
#Output
#  computed mean value of the binomial experiment
aux_mean <- function(trials,prob){
  mean <- trials*prob
  return(mean)
}


#Description: a private auxiliary function aux_variance()
#  aux_variance returns the variance of the experiment
#Input
#  trials: number of trials, numeric value
#  prob: probability,numeric value
#Output
#  computed variance value of the binomial experiment
aux_variance <- function(trials,prob){
  variance <- trials*prob*(1-prob)
  return(variance)
}


#Description: a private auxiliary function aux_mode()
#  aux_mode returns the mode of the experiment
#Input
#  trials: number of trials, numeric value
#  prob: probability,numeric value
#Output
#  computed mode value of the binomial experiment
aux_mode = function(trials, prob){
  value = (trials + 1) * prob
  if (as.integer(value) == value){
    mode1 = value
    mode2 = mode1 - 1
    return (c(mode1, mode2))
  }
  else{
    mode = floor(value)
    return(mode)
  }
}


#Description: a private auxiliary function aux_skewness()
#  aux_skewness returns the skewness(asymmetry) of a probability distribution
#Input
#  trials: number of trials, numeric value
#  prob: probability,numeric value
#Output
#  computed skewness value of the binomial experiment
aux_skewness <- function(trials,prob){
  skewness <- (1-2*prob)/sqrt(trials*prob*(1-prob))
  return(skewness)
}


#Description: a private auxiliary function aux_kurtosis()
#  aux_kurtosis returns a measure of the tailedness
#  of the probability distribution of a random variable
#Input
#  trials: number of trials, numeric value
#  prob: probability,numeric value
#Output
#  computed kurtosis value of the binomial experiment
aux_kurtosis <- function(trials,prob){
  kurtosis <- (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  return(kurtosis)
}



#1.3) Function bin_choose()

#' @title binomial choose factor
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n numeric value, number of trials
#' @param k numeric vector, number of successes
#' @return computed choose factor
#' @export
#' @examples
#' #5 choose 2
#' bin_choose(n = 5, k = 2)
#'
#' #5 choose 0
#' bin_choose(5, 0)
#'
#' #5 choose 1:3
#' bin_choose(5, 1:3)
bin_choose <- function(n,k){
  if (any(k > n)){
    stop("k cannot be greater than n")
  }
  else{
    value = (factorial(n))/(factorial(k)*factorial(n-k))
    return(value)
  }
}



#1.4) Function bin_probability()

#' @title binomial probability
#' @description computes the probability of getting the desired successes
#' @param success numeric vector, number of successes
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return computed binomial probability
#' @export
#' @examples
#' # probability of getting 2 successes in 5 trials (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)
bin_probability <- function(success,trials,prob){
  if(check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  if(check_prob(prob) != TRUE){
    stop("invalid probability value")
  }
  if(any(check_success(success,trials) != TRUE)){
    stop("invalid success value")
  }
  else{
    probability <- bin_choose(n=trials,k=success)*(prob^success)*(1-prob)^(trials-success)
    return(probability)
  }
}



#1.5) Function bin_distribution()

#' @title binomial distribution
#' @description computes the probability of getting the possible successes
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return a data frame of possible successes and related probability
#' @export
#' @examples
#' #Build a binomial distribution
#' dis1 <- bin_distribution(trials = 5, prob = 0.5)
#' #plot it
#' plot(dis1)
bin_distribution <- function(trials,prob){
  success <- 0:trials
  probability <- bin_probability(success,trials,prob)
  x <- data.frame(success = success, probability = probability)
  class(x) <- c("bindis", "data.frame")
  x
}

#' @export
plot.bindis <- function(x,...){
  barplot(x$probability,width = 0.5,
              xlab = "successes", ylab = "probability",names.arg = c(0:max(x$success)))
}



#1.6) Function bin_cumulative()

#' @title binomial cumulative distribution
#' @description computes the (cumulative) probability of getting the possible successes
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return a data frame of possible successes and related (cumulative) probability
#' @export
#' @examples
#' #build a binomial distribution
#' dis2 <- bin_cumulative(trials = 5, prob = 0.5)
#'#plot it
#'plot(dis2)
bin_cumulative <- function(trials,prob){
  success <- 0:trials
  probability <- bin_probability(success,trials,prob)
  cumulative <- cumsum(probability)
  x <- data.frame(success=success,probability=probability,cumulative=cumulative)
  class(x) <- c("bincum","data.frame")
  x
}

#' @export
plot.bincum <- function(x,...){
  plot(x$success,x$cumulative,type = "b",xlab = "success", ylab = "cumulative probability")
}




#1.7) Function bin_variable()

#' @title binomial random variable
#' @description check if trials and probability are valid
#' and return an object of class "binvar"
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return an object of class "binvar"
#' @export
#' @examples
#' #build a binomial distribution
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' #summarize it
#' binsum1 <- summary(bin1)
#' #print the summary
#' binsum1
bin_variable <- function(trials,prob){
  if(check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  if(check_prob(prob) != TRUE){
    stop("invalid probability value")
  }
  x <- list("number of trials" = trials, "probability of success" = prob)
  class(x) <- "binvar"
  x
}

#' @export
print.binvar <- function(x,...){
  cat('"Binomial variable" \n\n')
  cat("Parameters","\n")
  cat(sprintf("- number of trials:"),x$`number of trials`,"\n")
  cat(sprintf("- prob of success:"),x$`probability of success`)
  invisible(x)
}

#' @export
summary.binvar <- function(x,...){
  parameters <- c("number of trials" = x$`number of trials`,"prob of success" = x$`probability of success`)
  measures <- data.frame(mean = aux_mean(x$`number of trials`,x$`probability of success`),
                         variance = aux_variance(x$`number of trials`,x$`probability of success`),
                         mode = aux_mode(x$`number of trials`,x$`probability of success`),
                         skewness = aux_skewness(x$`number of trials`,x$`probability of success`),
                         kurtosis = aux_kurtosis(x$`number of trials`,x$`probability of success`))
  obj <- list(parameters = parameters, measures = measures)
  class(obj) <- "summary.binvar"
  obj
}


#' @export
print.summary.binvar <- function(x,...){
  cat('"Summary Binomial" \n\n')
  cat("Parameters","\n")
  cat(sprintf("- number of trials:"),x$parameters[1],"\n")
  cat(sprintf("- prob of success:"),x$parameters[2])
  cat('\n\n')
  cat("Measures","\n")
  cat(sprintf("- mean:"),x$measures$mean,"\n")
  cat(sprintf("- variance:"),x$measures$variance,"\n")
  cat(sprintf("- mode:"),x$measures$mode,"\n")
  cat(sprintf("- skewness:"),x$measures$skewness,"\n")
  cat(sprintf("- kurtosis:"),x$measures$kurtosis,"\n")
  invisible(x)
}



#1.8) Functions of measures

#' @title binomial mean
#' @description computes the mean of the binomial experiment
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return a numeric value, mean
#' @export
#' @examples
#' bin_mean(trials=5,prob=0.5)
bin_mean <- function(trials,prob){
  if(check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  if(check_prob(prob) != TRUE){
    stop("invalid probability value")
  }
  mean <- aux_mean(trials,prob)
  return(mean)
}


#' @title binomial variance
#' @description computes the variance of the binomial experiment
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return a numeric value, variance
#' @export
#' @examples
#' bin_variance(trials=5,prob=0.5)
bin_variance <- function(trials,prob){
  if(check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  if(check_prob(prob) != TRUE){
    stop("invalid probability value")
  }
  var <- aux_variance(trials,prob)
  return(var)
}



#' @title binomial mode
#' @description computes the mode of the binomial experiment
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return numeric value, mode
#' @export
#' @examples
#' bin_mode(trials=5,prob=0.5)
bin_mode <- function(trials,prob){
  if(check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  if(check_prob(prob) != TRUE){
    stop("invalid probability value")
  }
  mode <- aux_mode(trials,prob)
  return(mode)
}



#' @title binomial skewness
#' @description computes the skewness of the binomial experiment
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return a numeric value, skewness
#' @export
#' @examples
#' bin_skewness(trials=5,prob=0.5)
bin_skewness <- function(trials,prob){
  if(check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  if(check_prob(prob) != TRUE){
    stop("invalid probability value")
  }
  skew <- aux_skewness(trials,prob)
  return(skew)
}

#' @title binomial kurtosis
#' @description computes the kurtosis of the binomial experiment
#' @param trials numeric value, number of trials
#' @param prob numeric value between 0 and 1, probability of success
#' @return a numeric value, kurtosis
#' @export
#' @examples
#' bin_kurtosis(trials=5,prob=0.5)
bin_kurtosis <- function(trials,prob){
  if(check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  if(check_prob(prob) != TRUE){
    stop("invalid probability value")
  }
  kurt <- aux_kurtosis(trials,prob)
  return(kurt)
}





