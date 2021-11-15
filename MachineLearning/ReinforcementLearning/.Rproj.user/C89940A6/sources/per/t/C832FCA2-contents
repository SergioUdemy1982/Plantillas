# Upper Confidence Bound

# Dataset
dataset <- read.csv(file = "Datasets/Ads_CTR_Optimisation.csv")

# Implementacion de UCB

# 1
N <- 10000
d <- 10 # 10 versiones del mismo anuncio
number_of_selections <- integer(d) # Numero de veces que se selecciono el anuncio

sums_rewards <- integer(d) # Guarda la suma de las recompensas

# 2

# Recompensa media y limite superior
ads_selected <- integer(0)

total_reward <- 0

for(n in 1:N) {
  max_upper_bound <- 0
  ad <- 0
  
  for(i in 1:d){
    if (number_of_selections[i] > 0) {
      
      average_reward <- sums_rewards[i] / number_of_selections[i]
      delta_i <- sqrt(3/2 * log(n) / number_of_selections[i])
      upper_bound <- average_reward + delta_i
    }else {
      upper_bound = 1e400
    }
    
    if (upper_bound > max_upper_bound) {
      max_upper_bound <- upper_bound
      ad <- i
    }
  }
  
  ads_selected <- append(ads_selected, ad)
  number_of_selections[ad] <- number_of_selections[ad] + 1
  reward <- dataset[n, ad]
  sums_rewards[ad] <- sums_rewards[ad] + reward
  total_reward <- total_reward + reward
} 


# Histograma

hist(ads_selected,
     col = 'steelblue2',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')

