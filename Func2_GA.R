"Improve idea: prob_refine can apply same idea as SA which decreasing as iteration goes."




GA_Optim <- function(N,p,ni,pop_size,
                     weight_customer, Distance.matrix, #,alpha_threshol
                     prob_cross,prob_mutation, prob_refine){
            #===================================================#
            # Functions
            # Function 1: calculate fitness given several p-facilities
            
            IndividualFitnessFun <- function(x){
                        closestStore2Customer <- apply( Distance.matrix[,x],1, min) * weight_customer
                        fitness <- sum(closestStore2Customer)
                        return(fitness)}
            
            #-----------------------------------------------#
            
            # Funciton 2: calculate selected facilities based on probability, which depends on fitness
            SelectFun <- function(parent_facilities, prob_refine){
                        fitness_each_candidate <- apply(parent_facilities, 2, FUN = IndividualFitnessFun)
                        
                        #fitness_tran <- max(fitness_each_candidate) - fitness_each_candidate
                        #good <- which(fitness_tran >= quantile(fitness_tran, prob_refine))
                        
                        good <- which(fitness_each_candidate <= quantile(fitness_each_candidate, 
                                                                         prob_refine))
                        parent_facilities_new <- parent_facilities[,good]
                        
                        
                        # Roulet wheel selectioin
                        prob_cal0 <- apply(parent_facilities_new, 2, FUN = IndividualFitnessFun)
                        prob_cal <- 1 / prob_cal0
                        prob_each_candidate <- prob_cal/sum(prob_cal)
                        
                        sample_th <- sample(1:ncol(parent_facilities_new), size = ncol(parent_facilities), 
                                            replace = T,  prob = prob_each_candidate)
                        
                        select_child_facilities <- parent_facilities_new[, sample_th]
                        return(select_child_facilities)
            }
            
            # parent_facilities <- replicate(n = pop_size, sample(1:N, p ,replace=F))
            # SelectFun(parent_facilities, prob_refine = 0.05)
            #-----------------------------------------------#
            
            # Function 3: calculate crossover outcome locations
            CrossOverFun <- function(matrix_cross, prob_cross){
                        p.enter <- runif(1)
                        if (p.enter < prob_cross) { 
                                    cpoint <- sample(1:(nrow(matrix_cross) - 1), 1)
                                    matrix_c <- cbind(matrix_cross, matrix_cross[,1])
                                    for (i in 1:(ncol(matrix_c) - 1)) {
                                                matrix_c[(cpoint + 1):nrow(matrix_c),i] <- matrix_c[(cpoint+1):nrow(matrix_c),i+1]
                                    }
                                    matrix_cross0 <- matrix_c
                                    matrix.cross <- matrix_cross0[,-ncol(matrix_c)]
                        }else{matrix.cross <- matrix_cross}
                        return(matrix.cross)
            }
            
            # parent_facilities <- replicate(n = pop_size, sample(1:N, p ,replace=F))
            # CrossOverFun(SelectFun(parent_facilities, 0.08), prob_cross = 0.9)
            #-----------------------------------------------#
            
            # Function 4: calculate mutation outcome locations
            MutationFun <- function(matrix_mutation, prob_mutation, mutation_candidate){
                        if (runif(1) < prob_mutation) { 
                                    mpoint <- sample(1:(nrow(matrix_mutation)), 1)
                                    matrix_m <- matrix_mutation
                                    
                                    for (i in 1:(ncol(matrix_m))) {
                                                # mutation candidate could be replace by any one in complete set of facilities
                                                matrix_m[mpoint, i] <- sample(setdiff(mutation_candidate,
                                                                                      as.numeric(matrix_m[, i])), 1)
                                    }
                                    matrix.mutation <- matrix_m
                        }else{matrix.mutation <- matrix_mutation}
                        return(matrix.mutation)
            }
            
            # parent_facilities <- replicate(n = pop_size, sample(1:N, p ,replace=F))
            # MutationFun(CrossOverFun(SelectFun(parent_facilities, 0.09), prob_cross = 0.9), prob_mutation = 0.2)
            
            #-----------------------------------------------#
            
            # Function 5: Combine above functions together to run genetic algorithm
            
            FunGA <- function(parent_facilities, prob_refine, 
                              prob_cross, prob_mutation,
                              mutation_candidate){
                        
                        # 1. Select
                        Select_facilities <- SelectFun(parent_facilities, prob_refine)
                        # 2. Cross-over
                        crossover_child_facilities <- CrossOverFun(Select_facilities, prob_cross)          
                        # 3. Mutation
                        mutation_child_facilities <- MutationFun( matrix_mutation = crossover_child_facilities, 
                                                                  prob_mutation = prob_mutation, 
                                                                  mutation_candidate = mutation_candidate)
                        return(mutation_child_facilities)
            }
            
            
            # parent_facilities <- replicate(n = pop_size, sample(1:N, p ,replace=F))
            # FunGA(parent_facilities = parent_facilities,
            #       prob_cross = prob_cross, prob_mutation = prob_mutation, mutation_candidate = 1:N)
            
            # Function 6: Extract results
            GetResult <- function(facilities){
                        fitness_i <- apply(facilities, 2, IndividualFitnessFun)
                        best_fitness_i <- min(fitness_i)
                        best_candidate_i <- facilities[, which.min(fitness_i)]
                        mean_fitness_i <- mean(fitness_i)
                        return(list("best_fitness_i" = best_fitness_i, 
                                    "mean_fitness_i" = mean_fitness_i, 
                                    "best_candidate_i" = best_candidate_i))}
            
            #==========================================================#
            
            Best_i <- list()
            Best_candidate_ever <- matrix(NA, nrow = ni, ncol = p)
            BestFitness_ever <- c()
            
            # Initialize
            parent_facilities <- replicate(n = pop_size, sample(1:N, p ,replace = F))
            Best_i[[1]] <- GetResult(parent_facilities)
            BestFitness_ever[1] <- Best_i[[1]]$best_fitness_i
            Best_candidate_ever[1,] <- Best_i[[1]]$best_candidate_i
            
            start_time <- Sys.time()
            
            # Iteration for GA
            for (i in 2:ni) {
                        cat("GA_progress:",round((i/ni)*100,2),"% \n")
                        child_facilities <- FunGA(parent_facilities = parent_facilities, 
                                                  prob_refine = prob_refine,
                                                  prob_cross = prob_cross,
                                                  prob_mutation = prob_mutation,
                                                  mutation_candidate = 1:N)
                        Best_i[[i]] <- GetResult(child_facilities)
                        
                        
                        # # Extract and save results
                        if (GetResult(child_facilities)$best_fitness_i <= BestFitness_ever[i - 1]) {
                                    BestFitness_ever[i] <-  GetResult(child_facilities)$best_fitness_i
                                    Best_candidate_ever[i,] <- GetResult(child_facilities)$best_candidate_i
                        }else{
                                    BestFitness_ever[i] <-  BestFitness_ever[i - 1]
                                    Best_candidate_ever[i,] <- Best_candidate_ever[i - 1,]
                        }
                        
                        
                        parent_facilities <- child_facilities
                        
                        #iteration stop conditions
                        # alpha <- (Best_i[[i]]$mean_fitness_i / Best_i[[i]]$best_fitness_i )
                        # if (alpha < alpha_threshol) {
                        #             print(paste("iteration stops due to convergence. Total:", i, "generations."))
                        #             break;
                        # }
                        
            }
            
            end_time <- Sys.time()
            RunningTime <- end_time - start_time
            
            
            
            return(list( "GlobolMin" = BestFitness_ever, "RunningTime" = RunningTime,
                         "Location" = Best_candidate_ever))
            
            
            
}


#====================================    TEST     =====================================#
#======================================================================================#

# set.seed(1)
# 
# N <- 100 # Number of candidates
# p <- 19 # Number of facilities
# ni <- 300 # Number of iterations
# 
# prob_cross <- 0.4
# prob_mutation <- 0.3
# pop_size <- 20      # in each GA, how many facilities solution are generated.
# 
# n_costomers <- 200
# weight_customer <- rchisq(n_costomers, df = 3)*10  # p-median problem has weight of each customer point
# 
# prob_refine <- 0.05  # When select facilities solution, top 0.8 goodness of fitting ones are considered.
# alpha_threshol <- 1.001 # The iteration stopping rule is when mean value and max value are very close, i.e. 1.0001 times higher
# 
# #Distance.matrix <- Distance_matrix_h5[1:n_costomers, 1:N]/10000
# Distance.matrix <- matrix(rchisq(n = n_costomers*N, df = 5), nrow = n_costomers)
# dim(Distance.matrix)
# result_GA <- GA_Optim(N,p,ni,pop_size,
#                       weight_customer, Distance.matrix,
#                       prob_cross,prob_mutation, prob_refine,alpha_threshol)
# 
# plot(result_GA$GlobolMin, pch = 20, type="l", main = "GA")


