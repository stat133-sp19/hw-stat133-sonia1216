#' title check_prob
#' description checks whether input probability is a valid probability value in the range between 0 and 1
#' param prob probability of success
#' return True if prob input is valid and an error if the prob input is not in the valid range
check_prob<-function(prob){
  if (0<=prob & prob<=1){
    return (TRUE)
  }
  else{
  stop("invalid prob value")
  }
}



#' title check_trial
#' description checks whether trials is a valid value for number of trials of being a non-negative integer
#' param trials number of trials
#' return True if trials are valid and an error if trial is invalid
check_trials<-function(trials){
  if (trials>=0 & round(trials)==trials){
    return (TRUE)
  }
  else{
    stop("invalid trial value")
  }
}



#' title check_success
#' description checks whether success is a valid value for number of success, which has to be in between bigger than or equal to 0 and less than or equal to trials
#' param success number of success
#' param trials number of trials
#' return True if number of successes are valid and an error if its not a valid value for number of successes
check_success<-function(success, trials){
  for (i in success){
    if (i>trials | i<0){
      stop('invalid success value')
    }
  }
  return (TRUE)
}



#' title aux_mean
#' description calculates the mean
#' param trial number of trials
#' param prob probability of success
#' return corresponding value from the computed mean
aux_mean<-function(trials, prob){
  return(trials*prob)
}



#' title aux_variance
#' description calculates the variance
#' param trial number of trials
#' param prob probability of success
#' return corresponding value from the computed variance
aux_variance<-function(trials, prob){
  return (trials*prob*(1-prob))
}



#' title aux_mode
#' description calculates the mode
#' param trial number of trial
#' param prob probability of success
#' return corresponding value from the computed mode
aux_mode<-function(trials, prob){
  return (as.integer((trials*prob)+prob))
}



#' title aux_skewness
#' description calculates the skewness
#' param trial number of trial
#' param prob probability of success
#' return corresponding value from the computed skewness
aux_skewness<-function(trials, prob){
  return ((1-(2*prob))/(trials*prob*(1-prob))**0.5)
}



#' title aux_kurtosis
#' description calculates the kurtosis
#' param trial number of trial
#' param prob probability of success
#' return corresponding value from the computed kurtosis
aux_kurtosis<-function(trials, prob){
  return ((1-(6*prob*(1-prob)))/(trials*prob*(1-prob)))
}



#' @title bin_choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of success
#' @return number of combinations if bin_choose output is valid meaning success is less than or equal to trials. If not, return an error message.
#' @export
#' @examples
#' bin_choose(5, 3)
#' bin_choose(4, 1)
bin_choose<-function(n, k){
  result<-c()
  for (i in (1:length(k))){
    if (k[i]>n){
      stop("k cannot be greater than n")
    }
    else{
      result_1<- (factorial(n)/(factorial(n-k[i])*factorial(k[i])))
      result<-c(result, result_1)
    }
  }
  return (result)
}



#' @title bin_probability
#' @description calculates probability of getting k successes in n trials
#' @param success number of success
#' @param trials number of trials
#' @param prob probability of success
#' @return probability if success, probability, and trials inputs are valid. If not, throws an error.
#' @export
#' @examples
#' bin_probability(2, 5, 0.6)
#' bin_probability(0:4, 6, 0.1)
bin_probability<-function(success, trials, prob){
  if (check_trials(trials)!=TRUE){
    stop('invalid trials value')
  }
  if(check_prob(prob)!=TRUE){
    stop('invalid probability value')
  }
  if (check_success(success,trials)!=TRUE){
    stop('invalid success value')
  }
  else{
    result<-c()
    for (i in success){
      result_1<-bin_choose(trials, i)*(prob**i)*((1-prob)**(trials-i))
      result<-c(result, result_1)
    }
    return(result)
  }}



#' @title bin_distribution
#' @description calculates probability distribution and gets an output that has two classes consisting of bindis and data.frame
#' @param trials number of trials
#' @param prob probability of success
#' @return data.frame with probability distribution that has successes in the first column and probability in the second column
#' @export
#' @examples
#' bin_distribution(5, 0.5)
#' bin_distribution(4, 0.6)
bin_distribution<-function(trials, prob){
  result_new<-c()
  success<-(0:trials)
  for (i in success){
    result<-bin_probability(i, trials, prob)
    result_new<-c(result_new, result)
  }

  df <- data.frame(success,'probability'=result_new)
  class(df) <- c("bindis","data.frame")
  return (df)
}



#' @export
plot.bindis<-function(distribution){
  library(ggplot2)
  library(dplyr)
  distribution%>%ggplot(aes(x=success,y=probability))+
    geom_histogram(stat = "identity")
}



#' @title bin_cumulative
#' @description calculates probability distribution and cumulative probabilities gets an output that has two classes consisting of bincum and data.frame
#' @param trials number of trials
#' @param prob probability of success
#' @return data.frame with probability distribution that has successes in the first column and probability in the second column and cumulative in the third column
#' @export
#' @examples
#' bin_cumulative(5, 0.5)
#' bin_cumulative(4, 0.6)
bin_cumulative<-function(trials, prob){
  result_new<-c()
  success<-(0:trials)
  for (i in success){
    result<-bin_probability(i, trials, prob)
    result_new<-c(result_new, result)
  }
  result_2<-c()
  for (i in 1: length(result_new)){
    if (i==1){
    previous<-0
    current<-result_new[i]
    }
    else{
    previous<-current+previous
    current<-result_new[i]
    }
    result_2<-c(result_2, previous+current)
  }
  df_new<-data.frame(success, 'probability'=result_new, 'cumulative'=result_2)
  class(df_new)<-c("bincum", "data.frame")
  return (df_new)
}


#' @export
plot.bincum<-function(distribution){
  library(ggplot2)
  library(dplyr)
  plot<-distribution%>%ggplot(aes(x=success,y=cumulative))+
    geom_point()+geom_line()
  return (plot)

}



#' @title bin_variable
#' @description generates an object that is a list with named elements of trial and probability of success
#' @param trials number of trials
#' @param prob probability of success
#' @export
#' @return object of class binvar if trials and probability inputs are valid if probability and trials are valid
#' @examples
#' bin_variable(5, 0.5)
#' bin_variable(4, 0.1)

bin_variable<-function(trials, prob){
  if (check_trials(trials) & check_prob(prob)){
    binvar<-list(trials = trials , prob= prob)
    class(binvar)<-c("binvar", "list")
    return (binvar)
  }
}
#' @export
print.binvar<-function(var){
  cat('"Binomial variable"\n\n')
  cat("Parameters\n")
  cat("-number of trials:", var$trials)
  cat("\n-prob of success:", var$prob)
}

#' @export
summary.binvar<-function(var){
  trials<-var$trials
  prob<-var$prob
  output<-list(trials=trials, prob=prob, mean=aux_mean(trials, prob), variance=aux_variance(trials, prob), mode=aux_mode(trials, prob), skewness=aux_skewness(trials, prob), kurtosis=aux_kurtosis(trials, prob))
  class(output)<-c("summary.binvar")
  return (output)
}
#' @export
print.summary.binvar<-function(var){
  trials<-var$trials
  prob<-var$prob
  cat('"Summary Binomial"\n\n')
  cat("Parameters\n")
  cat("-number of trials:", trials)
  cat("\n-prob of success:", prob)
  cat("\n\n Measures\n")
  cat("- mean    :", aux_mean(trials, prob))
  cat("\n- variance:", aux_variance(trials, prob))
  cat("\n- mode    :", aux_mode(trials, prob))
  cat("\n- skewness:", aux_skewness(trials, prob))
  cat("\n- kurtosis:", aux_kurtosis(trials, prob))
}
#' @title bin_mean
#' @description calculates mean of having certain trials and certain probability
#' @param trials number of trials
#' @param prob probability of success
#' @return mean if probability and trials inputs are valid. If not, throws an error.
#' @export
#' @examples
#' bin_mean(5, 0.6)
#' bin_mean(6, 0.7)
bin_mean<-function(trials, prob){
  if (check_trials(trials)==TRUE & check_prob(prob)==TRUE){
    return (aux_mean(trials, prob))
  }
}
#' @title bin_variance
#' @description calculates variance of having certain trials and certain probability
#' @param trials number of trials
#' @param prob probability of success
#' @return variance if probability and trials inputs are valid. If not, throws an error.
#' @export
#' @examples
#' bin_variance(5, 0.6)
#' bin_variance(6, 0.7)
bin_variance<-function(trials, prob){
  if (check_trials(trials)==TRUE & check_prob(prob)==TRUE){
    return (aux_variance(trials, prob))
  }
}
#' @title bin_mode
#' @description calculates mode of having certain trials and certain probability
#' @param trials number of trials
#' @param prob probability of success
#' @return mode if probability and trials inputs are valid. If not, throws an error.
#' @export
#' @examples
#' bin_mode(5, 0.6)
#' bin_mode(6, 0.7)
bin_mode<-function(trials, prob){
  if (check_trials(trials)==TRUE & check_prob(prob)==TRUE){
    return (aux_mode(trials, prob))
  }
}
#' @title bin_skewness
#' @description calculates skewness of having certain trials and certain probability
#' @param trials number of trials
#' @param prob probability of success
#' @return skewness if probability and trials inputs are valid. If not, throws an error.
#' @export
#' @examples
#' bin_skewness(5, 0.6)
#' bin_skewness(6, 0.7)
bin_skewness<-function(trials, prob){
  if (check_trials(trials)==TRUE & check_prob(prob)==TRUE){
    return (aux_skewness(trials, prob))
  }
}
#' @title bin_kurtosis
#' @description calculates kurtosis of having certain trials and certain probability
#' @param trials number of trials
#' @param prob probability of success
#' @return kurtosis if probability and trials inputs are valid. If not, throws an error.
#' @export
#' @examples
#' bin_kurtosis(5, 0.6)
#' bin_kurtosis(6, 0.7)
bin_kurtosis<-function(trials, prob){
  if (check_trials(trials)==TRUE & check_prob(prob)==TRUE){
    return (aux_kurtosis(trials, prob))
  }
}
