# Muestreo de Thompson

# Dataset
dataset <- read.csv(file = "Datasets/Ads_CTR_Optimisation.csv")

# Implementacion del Muestreo Thompson

N <- 10000
d <- 10 # 10 versiones del mismo anuncio

number_rewards_1 <- integer(d)
number_rewards_0 <- integer(d)


ads_selected <- integer(0)
total_reward <- 0

for(n in 1:N) {
  
  max_random <- 0
  ad <- 0
  
  for(i in 1:d){
    
    random_beta <- rbeta(n = 1,
                         shape1 = number_rewards_1[i] + 1,
                         shape2 = number_rewards_0[i] + 1)
    
    
    if (random_beta > max_random) {
      max_random <- random_beta
      ad <- i
    }
  }
  
  ads_selected <- append(ads_selected, ad)
  reward <- dataset[n, ad]
  
  if (reward == 1) {
    number_rewards_1[ad] <- number_rewards_1[ad] + 1
  }else {
    number_rewards_0[ad] <- number_rewards_0[ad] + 1
  }
  
  total_reward <- total_reward + reward
} 


# Histograma

hist(ads_selected,
     col = 'steelblue2',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Times Selected')

