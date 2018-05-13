rm(list = ls())

source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func1_SA.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func2_GA.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func3_PSO.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func4_Bee.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func5_Fish.R")
source("https://raw.githubusercontent.com/liyujiao1026/pmediansolvers/master/Func6_compare.R")


# 1. Read IKEA data ===================================================#
library(h5)
loadhdf5data <- function(h5File, dataset) {
            f <- h5file(h5File)
            nblocks <- h5attr(f[dataset], "nblocks")
            data <- do.call(cbind, lapply(seq_len(nblocks) - 1, function(i) {
                        data <- as.data.frame(f[paste0(dataset, "/block", i, "_values")][])
                        colnames(data) <- f[paste0(dataset, "/block", i, "_items")][]
                        data}))
            h5close(f)
            data
}



Distance.matrix <- loadhdf5data("./data/PHD_MatrixDist.h5", "matrix")
# #List of Settlements (X,Y, ID of node). 1938 lines
store_candidates <- read.csv("./data/PHD_Settlements.txt", header = F, sep = " ")
# List of population points : (X,Y, number of persons), 187679 lines
customer <- read.csv("./data/PHD_Pop.txt", header = F, sep = " ") 
weight_customer <- customer[,3]
#-------------------------------------------#


# Check whether data is correct
dim(Distance.matrix) == c(187679, 1938)  #(187679 lines, 1938 columns)
dim(customer) == c(187679, 3) # population points : (X,Y, number_of_persons), 187679 lines

# 2. Settings =========================================#

set.seed(1)
# (1). P-median problem settings
N = nrow(store_candidates) # Number of candidates
p = 19 # Number of selected facilities

# (2). Iteration settings
ni <- 500 # Number of iterations
pop_size <- 100

# (3). Parameters settings
SA_temparature <- 500 # better the same as iteration time
SA_cool_rate <- 0.95


GA_prob_refine <- 0.9  # top 0.9 are selected 
GA_prob_cross <- 0.8
GA_prob_mutation <- 0.2


PSO_c1 <- 0.7 # updating rate for pbest
PSO_c2 <- 0.3 # updating rate for gbest

BEE_maxtrial <- 100

FISH_crowdness <- 5  # how many peers are allowed in one group at most
FISH_visual_mutual <- 10  # how many common elements in solutions are assumed as one group: p/2
FISH_try_number <- 5


# 2. Running algorithms---------------------------------------#


result_compare <- Compare_Algorithm_Func(
            N,p,ni,pop_size,
            Distance.matrix, weight_customer,
            algorithms = c("SA","GA","PSO","BEE","FISH"),
            SA_temparature,  SA_cool_rate,
            GA_prob_refine,  GA_prob_cross,GA_prob_mutation,
            PSO_c1, PSO_c2,
            BEE_maxtrial,
            FISH_crowdness, FISH_visual_mutual, FISH_try_number,
            path_save = result_path_save)




