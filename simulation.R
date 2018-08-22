#Includes

library(ggplot2)

#Assume that the population consists of two types of people, 50/50 split
#One type, true RR is 2.0, the other type, true RR is 5.0

#Some bias has occured in selection of the sample such that

percentageTypeOne <- 0.7

#Rather than the true value.

#The base rate of outcome in the unexposed is:

baseRate <- 0.10

#First, a function to check if calculated CIs contain the true RR:

isItIn <- function(trueRR, CI){
  if(log(trueRR) < CI[2] && log(trueRR) > CI[1]){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#Make some fake sample populations

OneThousandTypeOnes <- rbind(
cbind(0,
        rbinom(
          n = 500,
          size = 1,
          p = baseRate
        )
),
cbind(1,
      rbinom(
        n = 500,
        size = 1,
        p = baseRate * 2.0
      )
  )
)

OneThousandTypeTwos <- rbind(cbind(0,
                             rbinom(
                               n = 500,
                               size = 1,
                               p = baseRate
                              )
                             ),
                             cbind(1,
                               rbinom(
                                 n = 500,
                                 size = 1,
                                 p = baseRate * 5
                               )
                             )
                          )

#Observed samples of sample sizes 100 to 300
#Will then look like:

percentRight <- 0

for(n in seq(100,500,10)){
  results <- FALSE
  for(i in 1:250){

    sample <- as.data.frame(rbind(
      OneThousandTypeOnes[sample(nrow(OneThousandTypeOnes), (n * percentageTypeOne)),],
      OneThousandTypeTwos[sample(nrow(OneThousandTypeTwos), (n * (1-percentageTypeOne))),]
    ))

    #Fit a GLM to estimate RR from sample

    model <- glm(V2 ~ V1, data = sample, family = binomial(link="log"), start = c(-2,0.5))

    if(model$converged){
      ci <- confint(model, parm = "V1")
      results[i] <- isItIn(3.5,ci)
    }
  }
  percentRight[(n/10)] <- mean(results)
}

plotData <- as.data.frame(cbind(seq(100,500,10),percentRight[10:50]))

ggplot(plotData, aes(x = V1, y = V2)) + geom_point() +
  labs(x = "Sample Size", y = "Percentage of 95% CIs containing true value") +
  geom_smooth(method = lm)
  

