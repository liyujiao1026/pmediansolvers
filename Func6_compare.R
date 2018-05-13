# compare all algorithms

Compare_Algorithm_Fun <- function(
            
            N,p,ni,pop_size,
            Distance.matrix, weight_customer,
            
            algorithms, # which algorithm is used
            
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
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_SA, file = paste0(path_save, "fitness_SA"))
                                    saveRDS(object = time_SA, file = paste0(path_save, "time_SA"))}
            } else {
                        fitness_SA <- time_SA <- NULL
                        
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
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_GA, file = paste0(path_save, "fitness_GA"))
                                    saveRDS(object = time_GA, file = paste0(path_save, "time_GA"))}
            } else {
                        fitness_GA <- time_GA <- NULL
                        
            }
            #--------------------------------------------------------------------------------#
            
            # 3. PSO algorithm
            
            if("PSO" %in% algorithms){ 
                        fitness_pso_result <- PSO_optim(N, p, ni, pop_size, 
                                                        Distance.matrix, weight_customer,
                                                        c1 = PSO_c1, c2 = PSO_c2)
                        fitness_PSO <- fitness_pso_result$GlobalMin_data$GlobalMin
                        time_PSO <- fitness_pso_result$RunningTime
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_PSO, file = paste0(path_save, "fitness_PSO"))
                                    saveRDS(object = time_PSO, file = paste0(path_save, "time_PSO"))}
            } else {
                        fitness_PSO <- time_PSO <- NULL
                        
            }
            #--------------------------------------------------------------------------------#
            
            # 4. Bee algorithm
            if("BEE" %in% algorithms){
                        fitness_bee_result <- ABC_Optim(N,p,ni,pop_size,
                                                        max_trial = BEE_maxtrial,
                                                        Distance.matrix, weight_customer)
                        fitness_BEE <- fitness_bee_result$GlobalMin
                        time_BEE <- fitness_bee_result$RunningTime
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_BEE, file = paste0(path_save,"fitness_BEE"))
                                    saveRDS(object = time_BEE, file = paste0(path_save, "time_BEE"))}
            } else {
                        fitness_BEE <- time_BEE <- NULL
                        
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
                        
                        if (!is.null(path_save)){
                                    saveRDS(object = fitness_FISH, file = paste0(path_save,"fitness_FISH"))
                                    saveRDS(object = time_FISH, file = paste0(path_save, "time_FISH"))}
            } else {
                        fitness_FISH <- time_FISH <- NULL
                        
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
            
            avail <- which(unlist(lapply(fitness_all, function(x){!is.null(x)})) == TRUE)
            fitness_opt <- min((unlist(lapply(fitness_all[avail], min))))
            fitness_error <- lapply(fitness_all, function(x){(x - fitness_opt)/fitness_opt })
            
            if (!is.null(path_save)){
                        saveRDS(object = compute_time, file = paste0(path_save,"compute_time"))
                        saveRDS(object = fitness_error, file = paste0(path_save,"fitness_error"))}
            
            
            return(list("fitness_error" = fitness_error, 
                        "fitness_all" = fitness_all,
                        "compute_time" = compute_time,
                        "fitness_opt" = fitness_opt ))
            
}

library(compiler)
Compare_Algorithm_Func <- cmpfun(Compare_Algorithm_Fun)

