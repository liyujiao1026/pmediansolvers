
PSO_optim <- function(N, p, ni, pop_size, 
                      Distance.matrix, weight_customer,
                      #, alpha
                      #w, 
                      c1, c2){
            
            # Functions
            # Function 1: calculate fitness given several p-facilities
            
            FitnessFun <- function(x){
                        closestStore2Customer <- apply( Distance.matrix[,x],1, min) * weight_customer
                        fitness <- -sum(closestStore2Customer)
                        return(fitness)}
            
            
            # Function 2: Update solution matrix ----------------------------------------#
            SolutionUpdate <- function(solution, prob){
                        if (runif(1,0,1) < prob) {
                                    random_facility <- sample(1:p, 1)
                                    solution[random_facility] <- sample(setdiff(1:N, solution), 1)}
                        return(solution)
            }
            
            # Test
            # xMat <- t(replicate(n = pop_size, sample(1:N, p ,replace=F)))
            # solution_vector <- xMat[1,]
            # pbest_vector <- xMat[2,]
            # gbest_vector <- xMat[3,]
            # SolutionUpdate(solution_vector, prob = w[2])
            
            # Function 3: Update solution matrix for one solution---------------------------#
            CompareFun <- function(solution_vector, pbest_vector, gbest_vector, wi, c1, c2){
                        S1 <- SolutionUpdate(solution_vector, prob = wi)
                        S2 <- SolutionUpdate(pbest_vector, prob = c1)
                        S3 <- SolutionUpdate(gbest_vector, prob = c2)
                        
                        fitness_all <- sapply(list(S1,S2,S3), FitnessFun)
                        S_best <- which.max(fitness_all)
                        pbest_value <- max(fitness_all)
                        # update solution and pbest
                        solution_update <- list(S1,S2,S3)[[S_best]]
                        solution_fitness <- FitnessFun(solution_update)
                        
                        #pbest_update <- if ( FitnessFun(S2) >= FitnessFun(pbest_vector) ) {S2} else(pbest_vector)
                        
                        pre_pbest_value <- FitnessFun(pbest_vector)
                        
                        if ( fitness_all[2] >= pre_pbest_value ) {
                                    pbest_update <- S2
                                    pbest_value <- fitness_all[2]
                        } else{
                                    pbest_update <- pbest_vector
                                    pbest_value <-  pre_pbest_value
                        }
                        
                        return(list("solution_update" = solution_update, 
                                    "solution_fitness" = solution_fitness, 
                                    
                                    "pbest_update" = pbest_update, 
                                    "pbest_value" = pbest_value))}
            
            # Function 4: Update solution matrix for solution matrix (n particle)----------------#
            CompareFun_population <- function(solution_mat, pbest_mat, gbest_vector, wi, c1, c2){
                        result_pop <- list()
                        for (i in 1:pop_size) {
                                    result_pop[[i]] <- CompareFun(solution_vector = solution_mat[i,], 
                                                                  pbest_vector = pbest_mat[i,],
                                                                  gbest_vector = gbest_vector,
                                                                  wi = wi, c1 = c1, c2 = c2)}
                        return(result_pop)
            }
            
            # Test:
            # xMat <- t(replicate(n = pop_size, sample(1:N, p ,replace=F)))
            # pbestMat <- t(replicate(n = pop_size, sample(1:N, p ,replace=F)))
            # Fitness_allS <- apply(xMat, 1, FitnessFun)
            # gbest_value <- max(Fitness_allS)
            # gbest_vector <- xMat[which.max(Fitness_allS),]
            # 
            # CompareFun_population(solution_mat = xMat, pbest_mat = pbestMat,
            #                       gbest_vector = gbest_vector, wi=0.9, c1, c2)
            # 
            
            
            
            # Function 5: Extract and update PSO results----------------------------------------#
            Extract_PG_best <- function(PSO_result, gbest_value){
                        # 3. Extract and update solution, pbest and gbest
                        # Update solutionMatrix
                        solution_mat_update <- t(sapply(PSO_result, function(x){x$solution_updat}))
                        solution_fitness <- sapply(PSO_result, function(x){x$solution_fitness})
                        
                        # Update pbest
                        pbestMat_update <- t(sapply(PSO_result, function(x){x$pbest_update}))
                        
                        # Update gbest
                        pbest_value_vector <- sapply(PSO_result, function(x){x$pbest_value})
                        
                        
                        gbest_update <- max(pbest_value_vector)
                        
                        if (gbest_update >= gbest_value) {
                                    gbest_value  <- gbest_update
                                    gbest_vector <- pbestMat_update[which.max(pbest_value_vector),]
                        }else{gbest_value <- gbest_value; gbest_vector <- gbest_vector}
                        
                        return(list("solution_mat_update" = solution_mat_update, 
                                    "solution_fitness" = solution_fitness,
                                    
                                    "pbestMat_update" = pbestMat_update, 
                                    "pbest_value_vector" = pbest_value_vector,
                                    
                                    "gbest_value" = gbest_value,
                                    "gbest_vector" = gbest_vector))
            }
            
            #Test
            # xMat <- t(replicate(n = pop_size, sample(1:N, p ,replace=F)))
            # pbestMat <- xMat
            # Fitness_allS <- apply(xMat, 1, FitnessFun)
            # gbest_value <- max(Fitness_allS)
            # gbest_vector <- xMat[which.max(Fitness_allS),]
            # PSO_result <- CompareFun_population(solution_mat = xMat, pbest_mat = pbestMat,
            #                                    gbest_vector = gbest_vector, wi=0.9, c1, c2)
            # Extract_PG_best(PSO_result, gbest_value)
            #===================================================================================#
            
            # 1. Initialization ---------------------------------------------------#
            #Initial solution and pbest solution
            GlobalMin <- c() 
            GlobalSolutions <- list()
            all_results <- list()
            fitness_current <- c()
            
            cat("PSO_progress:",round((1/ni)*100,2),"% \n")
            xMat <- t(replicate(n = pop_size, sample(1:N, p ,replace = F)))
            pbestMat <- xMat
            #Initial gbest 
            Fitness_allS <- apply(xMat, 1, FitnessFun)
            gbest_value <- max(Fitness_allS)
            gbest_vector <- xMat[which.max(Fitness_allS),]
            
            fitness_current[1] <- gbest_value
                        
            GlobalMin[1] <- gbest_value
            GlobalSolutions[[1]] <- gbest_vector
            
            
            #Save results
            
            #-----------------------------------------------------------------------#
            start_time <- Sys.time()
            w <- seq(1,0,length.out = ni) # updating rate for solution matrix
            
            for (i in 2:ni) {
                        cat("PSO_progress:",round((i/ni)*100,2),"% \n")
                        PSO_i <- CompareFun_population(solution_mat = xMat, pbest_mat = pbestMat,
                                                       gbest_vector = gbest_vector, w[i], c1, c2)
                        
                        ExtractResults <- Extract_PG_best(PSO_i, gbest_value)
                        
                        #update xsolution, pbest, gbest
                        xMat <- ExtractResults$solution_mat_update
                        pbestMat <- ExtractResults$pbestMat_update
                        gbest_value <- ExtractResults$gbest_value
                        gbest_vector <- ExtractResults$gbest_vector
                        
                        fitness_current[i] <- max(ExtractResults$solution_fitness)
                        
                        GlobalMin[i] <- gbest_value
                        GlobalSolutions[[i]] <- gbest_vector
                        #all_results[[i]] <- ExtractResults
                        
            }
            
            GlobalMin_data <- data.frame(do.call(rbind, GlobalSolutions))
            colnames(GlobalMin_data) <- paste0("facility_", 1:p)
            GlobalMin_data$GlobalMin <- -1 * GlobalMin
            
            end_time <- Sys.time()
            RunningTime <- end_time - start_time
            
            return(list("GlobalMin_data" = GlobalMin_data, "RunningTime" = RunningTime, 
                        "Location" = GlobalSolutions,
                        "fitness_current" = fitness_current))
}


#====================================    TEST     =====================================#
#======================================================================================#


# set.seed(1)
# 
# N <- 100 # Number of candidates
# p <- 19 # Number of facilities
# ni <- 30 # Number of iterations
# 
# n_costomers <- 20
# pop_size <- 30     # in each PSO, how many facilities solution are generated.
# 
# 
# weight_customer <- rchisq(n_costomers, df = 3)*10  # population of each customer point
# Distance.matrix <- matrix(rchisq(n = n_costomers*N, df = 5), nrow = n_costomers)
# dim(Distance.matrix)
# 
# 
# w <- seq(1,0,length.out = ni) # updating rate for solution matrix
# c1 <- c2 <- 0.5 # updating rate for pbest and gbest
# alpha <- 0.001 # stopping rule
# 
# #---------------------------------#
# result_pso <- PSO_optim(N, p, ni, pop_size, 
#                         Distance.matrix, weight_customer,
#                         w, c1, c2, alpha)
# 
# result_pso
# 
# fit_plot <- result_pso$GlobalMin
# plot(x = 1:length(fit_plot), fit_plot, type = "l", main = "pso", xlab = "Iteration", ylab = "ObejectFun (distance)")
