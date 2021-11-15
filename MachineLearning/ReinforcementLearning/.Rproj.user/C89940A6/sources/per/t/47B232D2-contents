# Random Selection

# Importing Data set

#dataset <- read.csv(file = "Datasets/Ads_CTR_Optimisation.csv")


# Implementing Random Selection

N <- 10000
d <- 10

ads_selected <- integer(0)
total_reward = 0

for (n in 1:N) {
  ad <- sample(1:10,1)
  ads_selected <- append(ads_selected, ad)
  reward <- dataset[n,ad]
  total_reward <- total_reward + reward
}


# Histogram the results

hist(x = ads_selected,
     col ="steelblue2",
     main = "Histogram of ads selections",
     xlab = "Ads",
     ylab = "Freq")
