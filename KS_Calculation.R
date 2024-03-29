# create deciles post outputting probabilities via any classificaiton model


decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i)
  }
  return (ifelse(x<deciles[1], 1,
                 ifelse(x<deciles[2], 2,
                        ifelse(x<deciles[3], 3,
                               ifelse(x<deciles[4], 4,
                                      ifelse(x<deciles[5], 5,
                                             ifelse(x<deciles[6], 6,
                                                    ifelse(x<deciles[7], 7,
                                                           ifelse(x<deciles[8], 8,
                                                                  ifelse(x<deciles[9], 9, 10))))))))))
}


# let probabilities = prob
# change decile number from 1-10
train$deciles <- 11 - decile(train$prob)