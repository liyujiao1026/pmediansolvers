

ABC_Optim <- function(N,p,ni,pop_size,
                      max_trial,
                      Distance.matrix, weight_customer){
            
            # reset bees rule: max times of keeping improving nothing
            #max_persistance <- 200 #stop rules: max times of same fitness in the row.
            
            # Function 1: calculate fitness given several p-facilities
            FitnessFun <- function(solution_i){
                        closestStore2Customer <- apply(Distance.matrix[,solution_i],1, min) * weight_customer
                        fitness <- sum(closestStore2Customer)
                        return(fitness)}
            
            
            # Function 2: Update solution matrix ----------------------------------------#
            SolutionUpdate <- function(solution){
                        random_facility <- sample(1:p, 1)
                        solution[random_facility] <- sample(setdiff(1:N, solution), 1)
                        return(solution)}
            
            # SolutionUpdate(sample(1:N, p ,replace=F))
            
            # Function 3: Initialize---------------------------#
            Initialization <- function(){
                        SolutionS <<- t(replicate(n = pop_size, sample(1:N, p ,replace=F)))
                        FitnessSol <<- apply(SolutionS, 1, FitnessFun)
                        GlobalMin <<- min(FitnessSol)
                        GlobalSolution <<- SolutionS[which.min(FitnessSol),]
                        
                        trial <<- rep(0, pop_size)
                        persistance <<- 0
            }
            
            
            # Function 4: Send Employee bee---------------------------#
            SendEmployedBees <- function(){
                        
                        # A randomly chosen solution is used in producing a mutant solution of the solution i
                        
                        # Calculate the fitness of each updated solution (by mutation)
                        # Then compare the new one with old one
                        SolutionS_update <- t(apply(SolutionS, 1, SolutionUpdate))
                        FitnessSol_update <- apply(SolutionS_update, 1, FitnessFun)
                        
                        compare_fit <- (FitnessSol_update < FitnessSol) 
                        
                        sapply(1:pop_size, function(k){
                                    if (compare_fit[k]==1){
                                                SolutionS[k,] <<- SolutionS_update[k,] #(1)update solution
                                                FitnessSol[k] <<- FitnessSol_update[k] #(2)update fitness
                                    }})
                        
                        trial <<- trial + (1 - compare_fit)                            # (3)update trial
            }
            
            
            
            # Function 5: Send Onlooker bee---------------------------#
            SendOnlookerBees <- function(){
                        # which solution should be sampled for next improvement?
                        # It depends on the probability values are calculated by using prob(i)= fitness(i)/sum(fitness)
                        
                        prob_fitness <<- FitnessSol / sum(FitnessSol)
                        update_bee <- sample(x = 1:pop_size, size = pop_size, replace = T, prob = prob_fitness)
                        SolutonS <- SolutionS[update_bee,]
                        
                        # Same procedure as in Employed bee step -------------------------#
                        SolutionS_update <- t(apply(SolutionS, 1, SolutionUpdate))
                        FitnessSol_update <- apply(SolutionS_update, 1, FitnessFun)
                        
                        compare_fit <- (FitnessSol_update < FitnessSol) 
                        
                        sapply(1:pop_size, function(k){
                                    if (compare_fit[k]==1){
                                                SolutionS[k,] <<- SolutionS_update[k,] #(1)update solution
                                                FitnessSol[k] <<- FitnessFun(SolutionS[k,])#(2)update fitness
                                    }})
                        
                        trial <<- trial + (1 - compare_fit)           
            }
            
            
            
            
            # Function 6: memorize optimal value---------------------------#
            MemorizeBestSource <- function() {
                        oldGlobalMin <- GlobalMin
                        
                        if(min(FitnessSol) < GlobalMin){
                                    GlobalMin <<- min(FitnessSol)
                                    GlobalSolution <<- SolutionS[which.min(FitnessSol),]
                                    persistance <<- 0
                        }else{
                                    persistance <<- persistance + 1
                        }
            }
            
            
            # Function 7: Send Scout bee---------------------------#
            
            SendScoutBees <- function(){
                        bee_reset <- which(trial >= max_trial)
                        if(length(bee_reset) > 0){
                                    SolutionS[bee_reset,] <<- sample(1:N, p ,replace = F) #(1)update solution
                                    FitnessSol[bee_reset] <<- FitnessFun(SolutionS[bee_reset,])
                        }
            }
            #=====================================================================#
            
            
            
            # Start to run : main functions
            start_time <- Sys.time()
            
            Initialization()
            global_fitness <- GlobalMin
            for(i in 2:ni){
                        cat("BEE_progress:",round((i/ni)*100,2),"% \n")
                        
                        SendEmployedBees()
                        SendOnlookerBees() 
                        MemorizeBestSource()
                        
                        # if (persistance > max_persistance) break
                        
                        global_fitness <- c(global_fitness, GlobalMin)
                        SendScoutBees()
            }
            
            #=====================================================================#
            end_time <- Sys.time()
            RunningTime <- end_time - start_time
            
            return(list("GlobalSolution" = GlobalSolution,
                        "GlobalMin" = global_fitness,
                        "RunningTime" = RunningTime))
}





#====================================    TEST     =====================================#
#======================================================================================#

# N <- 100 # Number of candidates
# p <- 19 # Number of facilities
# ni <- 300 # Number of iterations
# 
# n_costomers <- 200
# pop_size <- 30 # in each ABC, how many facilities solution are generated.(Number of Bees)
# 
# weight_customer <- rchisq(n_costomers, df = 3)*10  # population of each customer point
# Distance.matrix <- matrix(rchisq(n = n_costomers*N, df = 5), nrow = n_costomers)
# max_trial <- 10 # reset bees rule: max times of keeping improving nothing
# max_persistance <- 20 #stop rules: max times of same fitness in the row.
# 
# 
# result_bee <- ABC_Optim(N,p,ni,pop_size,
#                         Distance.matrix, weight_customer, 
#                         max_trial, max_persistance)
# 
# 
# fit_plot <- result_bee$global_fitness
# plot(x = 1:length(fit_plot), fit_plot, type = "l", main = "bee", xlab = "Iteration", ylab = "ObejectFun (distance)")
















