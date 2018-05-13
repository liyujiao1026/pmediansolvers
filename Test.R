rm(list = ls())
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func1_SA.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func2_GA.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func3_PSO.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func4_Bee.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func5_Fish.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func6_compare.R")

result_path_save <- "~/Desktop/"
#---------------------------------------------#





# # 1. P-median problem settings
N <- 193 # Number of candidates
p <- 5 # Number of facilities
n_costomers <- 187 # Number of customer buildings


# 2. Iteration settings
ni <- 40 # Number of iterations
pop_size <- 50

# 
# # 3. Parameters settings
algorithms <- c("SA","GA","PSO","BEE","FISH")
SA_temparature <- 500 # better the same as iteration time
SA_cool_rate <- 0.95


GA_prob_refine <- 0.9  # When select facilities solution, goodness of fitting lower than 0.05 are discarded.
GA_prob_cross <- 0.8
GA_prob_mutation <- 0.2


PSO_c1 <- 0.7 # updating rate for pbest
PSO_c2 <- 0.3 # updating rate for gbest

BEE_maxtrial <- 100

FISH_crowdness <- 5  # how many peers are allowed in one group at most
FISH_visual_mutual <- 10  # how many common elements in solutions are assumed as one group: p/2
FISH_try_number <- 5


# # 4. Generate distance matrix
weight_customer <- rpois(n = n_costomers, lambda = 30) # Number of people for each customer building
Distance.matrix <- matrix(rnorm(n = n_costomers*N,mean = 100,sd = 10), nrow = n_costomers)


result_compare <- Compare_Algorithm_Func(
            N,p,ni,pop_size,
            Distance.matrix, weight_customer,
            algorithms = c("SA","BEE"),
            SA_temparature,  SA_cool_rate,
            GA_prob_refine,  GA_prob_cross,GA_prob_mutation,
            PSO_c1, PSO_c2,
            BEE_maxtrial,
            FISH_crowdness, FISH_visual_mutual, FISH_try_number,
            path_save = result_path_save)
