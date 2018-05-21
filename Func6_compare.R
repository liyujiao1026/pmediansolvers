source("./Algorithm_source/Func1_SA.R")
source("./Algorithm_source/Func2_GA.R")
source("./Algorithm_source/Func3_PSO.R")
source("./Algorithm_source/Func4_Bee.R")
source("./Algorithm_source/Func5_Fish.R")

#===================================================================#



Compare_Algorithm_Fun <- function(
            
            N,p,ni,pop_size,
            Distance.matrix, weight_customer,
            
            algorithms,
            
            SA_temparature, # better the same as iteration time
            SA_cool_rate,
            
            
            GA_prob_refine,  # When select facilities solution, goodness of fitting lower than 0.05 are discarded.
            GA_prob_cross,
            GA_prob_mutation,
            
            
            PSO_c1, # updating rate for pbest 
            PSO_c2, # updating rate for gbest
            
            BEE_maxtrial,
            
            FISH_crowdness,  # how many peers are allowed in one group at most
            FISH_visual_mutual,  # how many common elements in solutions are assumed as one group: p/2
            FISH_try_number,
            path_save = NULL
){
            
            #--------------------------------------------------------------------------------#
            # 1. Simulated Annealing Algorithm 
            
            if("SA" %in% algorithms){ 
                        fitness_SA_result <- SA_optim(N,p,ni,
                                                      Distance.matrix, weight_customer, 
                                                      Tempearture_init = SA_temparature, SA_cool_rate)
                        fitness_SA <- as.numeric(fitness_SA_result$Global_Min)
                        time_SA <- fitness_SA_result$RunningTime
                        location_SA <- fitness_SA_result$Location
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_SA, file = paste0(path_save, "fitness_SA"))
                                    saveRDS(object = location_SA, file = paste0(path_save, "location_SA"))
                                    saveRDS(object = time_SA, file = paste0(path_save, "time_SA"))}
            } else {
                        fitness_SA <- time_SA <- location_SA <-NULL
                        
            }
            #--------------------------------------------------------------------------------#
            
            # 2. Genetic Algorithm
            
            if("GA" %in% algorithms){ 
                        fitness_GA_result <- GA_Optim(N,p,ni,pop_size,
                                                      weight_customer, Distance.matrix,
                                                      prob_cross = GA_prob_cross ,prob_mutation = GA_prob_cross, 
                                                      prob_refine = GA_prob_cross)
                        fitness_GA <- fitness_GA_result$GlobolMin
                        time_GA <- fitness_GA_result$RunningTime
                        location_GA <- fitness_GA_result$Location
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_GA, file = paste0(path_save, "fitness_GA"))
                                    saveRDS(object = location_GA, file = paste0(path_save, "location_GA"))
                                    saveRDS(object = time_GA, file = paste0(path_save, "time_GA"))}
            } else {
                        fitness_GA <- time_GA <- location_GA <-NULL
                        
            }
            #--------------------------------------------------------------------------------#
            
            # 3. PSO algorithm
            
            if("PSO" %in% algorithms){ 
                        fitness_pso_result <- PSO_optim(N, p, ni, pop_size, 
                                                        Distance.matrix, weight_customer,
                                                        c1 = PSO_c1, c2 = PSO_c2)
                        fitness_PSO <- fitness_pso_result$GlobalMin_data$GlobalMin
                        time_PSO <- fitness_pso_result$RunningTime
                        location_PSO <- fitness_pso_result$Location
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_PSO, file = paste0(path_save, "fitness_PSO"))
                                    saveRDS(object = location_PSO, file = paste0(path_save, "location_PSO"))
                                    saveRDS(object = time_PSO, file = paste0(path_save, "time_PSO"))}
            } else {
                        fitness_PSO <- time_PSO <- location_PSO <- NULL
                        
            }
            #--------------------------------------------------------------------------------#
            
            # 4. Bee algorithm
            if("BEE" %in% algorithms){
                        fitness_bee_result <- ABC_Optim(N,p,ni,pop_size,
                                                        max_trial = BEE_maxtrial,
                                                        Distance.matrix, weight_customer)
                        fitness_BEE <- fitness_bee_result$GlobalMin
                        time_BEE <- fitness_bee_result$RunningTime
                        location_BEE <- fitness_bee_result$Location
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_BEE, file = paste0(path_save,"fitness_BEE"))
                                    saveRDS(object = location_BEE, file = paste0(path_save, "location_BEE"))
                                    saveRDS(object = time_BEE, file = paste0(path_save, "time_BEE"))}
            } else {
                        fitness_BEE <- time_BEE <- location_BEE <-NULL
                        
            }       
            
            #--------------------------------------------------------------------------------#
            
            # 5. Fish algorithm
            if("FISH" %in% algorithms){
                        fitness_fish_result <- Fish_Optim(N, p, ni, pop_size= pop_size,
                                                          Distance.matrix, weight_customer,
                                                          try_number = FISH_try_number, crowdness = FISH_crowdness,
                                                          visual_mutual = FISH_visual_mutual)
                        fitness_FISH <- fitness_fish_result$GlobalMin$GlobalMin
                        time_FISH <- fitness_fish_result$RunningTime
                        location_FISH <- fitness_fish_result$Location
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_FISH, file = paste0(path_save,"fitness_FISH"))
                                    saveRDS(object = location_FISH, file = paste0(path_save, "location_FISH"))
                                    saveRDS(object = time_FISH, file = paste0(path_save, "time_FISH"))}
            } else {
                        fitness_FISH <- time_FISH <- location_FISH <- NULL
                        
            }    
            # output ===============================================================#
            fitness_all <- list("fitness_SA" = fitness_SA,
                                "fitness_GA" = fitness_GA,
                                "fitness_PSO" = fitness_PSO,
                                "fitness_BEE" = fitness_BEE,
                                "fitness_FISH" = fitness_FISH)
            compute_time <- list("time_SA" = time_SA,
                                 "time_GA" = time_GA,
                                 "time_PSO" = time_PSO,
                                 "time_BEE" = time_BEE,
                                 "time_FISH" = time_FISH)
            
            locations <- list("location_SA" = location_SA,
                                 "location_GA" = location_GA,
                                 "location_PSO" = location_PSO,
                                 "location_BEE" = location_BEE,
                                 "location_FISH" = location_FISH)
            
            avail <- which(unlist(lapply(fitness_all, function(x){!is.null(x)})) == TRUE)
            fitness_opt <- min((unlist(lapply(fitness_all[avail], min))))
            fitness_error <- lapply(fitness_all, function(x){(x - fitness_opt)/fitness_opt })
            
            if (!is.null(path_save)) {
                        saveRDS(object = compute_time, file = paste0(path_save,"compute_time"))
                        saveRDS(object = fitness_error, file = paste0(path_save,"fitness_error"))}
            
            
            return(list("fitness_error" = fitness_error, 
                        "fitness_all" = fitness_all,
                        "locations" = locations,
                        "compute_time" = compute_time,
                        "fitness_opt" = fitness_opt ))
            
}

library(compiler)
Compare_Algorithm_Func <- cmpfun(Compare_Algorithm_Fun)





# Test========================#

# set.seed(1)
# # # 1. P-median problem settings
# N <- 193 # Number of candidates
# p <- 5 # Number of facilities
# n_costomers <- 187 # Number of customer buildings
# 
# 
# # 2. Iteration settings
# ni <- 40 # Number of iterations
# pop_size <- 50
# 
# # 
# # # 3. Parameters settings
# algorithms <- c("SA","GA","PSO","BEE","FISH")
# SA_temparature <- 500 # better the same as iteration time
# SA_cool_rate <- 0.95
# 
# 
# GA_prob_refine <- 0.9  # When select facilities solution, goodness of fitting lower than 0.05 are discarded.
# GA_prob_cross <- 0.8
# GA_prob_mutation <- 0.2
# 
# 
# PSO_c1 <- 0.7 # updating rate for pbest
# PSO_c2 <- 0.3 # updating rate for gbest
# 
# BEE_maxtrial <- 100
# 
# FISH_crowdness <- 5  # how many peers are allowed in one group at most
# FISH_visual_mutual <- 10  # how many common elements in solutions are assumed as one group: p/2
# FISH_try_number <- 5
# 
# 
# # # 4. Generate distance matrix
# weight_customer <- rpois(n = n_costomers, lambda = 30) # Number of people for each customer building
# Distance.matrix <- matrix(rnorm(n = n_costomers*N,mean = 100,sd = 10), nrow = n_costomers)
# 
# 
# result_compare <- Compare_Algorithm_Func(
#             N,p,ni,pop_size,
#             Distance.matrix, weight_customer,
#             algorithms = c("SA","BEE"),
#             SA_temparature,  SA_cool_rate,
#             GA_prob_refine,  GA_prob_cross,GA_prob_mutation,
#             PSO_c1, PSO_c2,
#             BEE_maxtrial,
#             FISH_crowdness, FISH_visual_mutual, FISH_try_number,
#             path_save = "/Users/Yujiao/Desktop/pascal/p_median_Rcode/")


#plotFun(result_compare)


