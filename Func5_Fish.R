# Artifical fish swarm algorithm


Fish_Optim <- function(N, p, ni, pop_size,
                       Distance.matrix, weight_customer,
                       try_number, crowdness, visual_mutual){
            
            # Function 1: calculate fitness given several p-facilities ---------------------#
            FitnessFun <- function(solution_i){
                        closestStore2Customer <- apply(Distance.matrix[,solution_i],1, min) * weight_customer
                        fitness <- sum(closestStore2Customer)
                        return(fitness)}
            
            
            # Function 2: Update solution matrix -------------------------------------------#
            SolutionUpdate <- function(solution){
                        random_facility <- sample(1:p, 1)
                        solution[random_facility] <- sample(setdiff(1:N, solution), 1)
                        return(solution)
            }
            
            # SolutionUpdate(sample(1:N, p ,replace=F))
            
            
            # Function 3: prey -----------------------------------------------------------#
            # Greedy search in try_number times
            FishPrey_Fun <- function(S_i, try_number){
                        S_i_fitness <- FitnessFun(S_i)
                        
                        try_Solutions <- list()
                        for (i in 1:try_number){
                                    try_Solutions[[i]] <- SolutionUpdate(S_i) 
                        }
                        try_fitness <- sapply(X = try_Solutions, FUN = FitnessFun)
                        S_i_new <- unlist(try_Solutions[which.min(try_fitness)])
                        
                        # if (S_i_fitness > min(try_fitness)){
                        #             S_i_new <- unlist(try_Solutions[which.min(try_fitness)])
                        # }else{
                        #             #S_i_new <- sample(setdiff(1:N, S_i), p, replace = F)
                        #             S_i_new <- S_i
                        # }
                        return(S_i_new)
            }
            
            
            # SolutionS <- t(replicate(n = pop_size, sample(1:N, p ,replace=F)))
            # FishPrey_Fun(S_i= SolutionS[1,], try_number=5)
            
            
            
            # Function 4: cluster ------------------------------------------------------#
            # In terms of one solution, which solutions in population are its peers?
            # those solutions who are closer to him.
            
            # After identifying the cluster of this solution,
            # the center of this cluster is selected as new solution if it performs better
            
            FishCluster_Fun <- function(S_i, SolutionS, i, pop_size, crowdness, visual_mutual){
                        
                        # check other fishs (solutions) who belong the same group with fish_i
                        OtherID <- setdiff(1:pop_size, i) 
                        # check the mutual element between other fish and current fish_i
                        count_mutual <- sapply(OtherID, function(x){length(intersect(S_i, SolutionS[x,]))})
                        # those solutions who have at least vistual_mutual elements in common with fish_i 
                        # can be considered as peers
                        peer_ID <- OtherID[which(count_mutual >= visual_mutual)]
                        
                        # if fish_i have too many neighbour peers (crowdness), fish_i should jump out 
                        # to maintain the fish groups evenly distributed and  widely covered solutions
                        if ( length(peer_ID) < crowdness & length(peer_ID) >= 1 ) {
                                    peers_Solutions <- SolutionS[peer_ID,]
                                    group_Solutions <- rbind(S_i, peers_Solutions)
                                    
                                    # chose the best one to represent this fish group, 
                                    
                                    # fitness_group <- apply(X = group_Solutions,MARGIN = 1, FUN = FitnessFun)
                                    # min_th <- which.min(fitness_group)
                                    # S_i.update <- group_Solutions[min_th,]
                                    
                                    
                                    # the solution who has the most common elements should be the center
                                    count_node <- sort(table(group_Solutions))
                                    selectName <- function(node_name){as.numeric(count_node)[which(
                                                names(count_node)==node_name)]}
                                    weightFun <- function(solution_i){sum(sapply(solution_i, 
                                                                                 selectName))}
                                    
                                    weight_solutions <- apply(group_Solutions, 1 , weightFun)
                                    prob_select <- (weight_solutions)^2/sum(weight_solutions^2)
                                    ith <- sample(1:nrow(group_Solutions), 1, prob = prob_select)
                                    
                                    S_i_update <- group_Solutions[ith,]
                                    
                                    
                                    
                                    # S_i_update <- as.numeric(sample(names(count_node), p, replace = F,
                                    #                                 prob = count_node/sum(count_node)))
                                    
                                    # if (FitnessFun(S_i_update) <= FitnessFun(S_i)){
                                    #             S_i.update <- S_i_update
                                    # }else{S_i.update <- FishPrey_Fun(S_i, try_number)}
                                    
                                    
                                    
                        }else{
                                    # if fish_i has no peers, it continues to prey
                                    S_i.update <- FishPrey_Fun(S_i, try_number)
                                    peers_Solutions <- NULL
                        }
                        
                        return(list( "S_i" = S_i.update, "S_peers" = peers_Solutions))
            }
            
            
            # SolutionS <- t(replicate(n = pop_size, sample(1:N, p ,replace=F)))
            # i=1; S_i <- SolutionS[1,];
            # FishCluster_Fun(S_i = S_i, SolutionS = SolutionS, i = 1, pop_size = 4, crowdness = 2, visual_mutual = 2)
            
            # Function 5: Chase ------------------------------------------------------------------------------#
            FishChase_Fun <- function(S_i, S_peers){
                        peers_Solutions <- rbind(S_i, S_peers)
                        
                        if(length(S_peers) == 0){
                                    S_i.update <- FishPrey_Fun(S_i, try_number)
                        }else{
                                    fitness_Solutions <- apply(peers_Solutions, 1, FitnessFun)
                                    prob_calc <- max(fitness_Solutions) - fitness_Solutions
                                    if(sum(prob_calc) == 0){
                                                S_i_update <- SolutionUpdate(GlobalSolution)
                                    }else{
                                                prob_Solutions <- prob_calc / sum(prob_calc)
                                                S_i.update <- peers_Solutions[sample(1:length(prob_Solutions), 
                                                                                     size = 1, prob = prob_Solutions), ]
                                    }
                        }
                        
                        # if ( (FitnessFun(S_i) > FitnessFun(S_i_old)) & (runif(1) > prob_FollowGbest)) {    
                        #             S_i_update <- SolutionUpdate(GlobalSolution)
                        # }else{S_i_update <- S_i}
                        
                        return(S_i.update)
            }
            
            
            # SolutionS <- t(replicate(n = pop_size, sample(1:N, p ,replace = F)))
            # S_i <- SolutionS[1,]; S_i_old = SolutionS[2,]; #GlobalSolution <- SolutionS[2,]; 
            # S_peers <- SolutionS[2:3,]
            # FishChase_Fun(S_i = S_i,S_peers = S_peers)
            
            
            # Function 6: Connect above functions with input of single solution ----------------#
            
            Fish_Optim_Fun <- function(SolutionS,
                                      try_number, pop_size, crowdness, 
                                      visual_mutual, GlobalSolution,GlobalMin
                                      ){ 
                        SolutionS1 <- t(apply(SolutionS, 1, FishPrey_Fun, try_number = try_number))
                        
                        SolutionS2 <- matrix(data = NA,nrow = pop_size, ncol = ncol(SolutionS1))
                        peers <- list()
                        for(i in 1:pop_size){
                                    Sol_i <- FishCluster_Fun(S_i = SolutionS1[i,] , SolutionS = SolutionS1, i = i, 
                                                             pop_size = pop_size, crowdness = crowdness, 
                                                             visual_mutual = visual_mutual)
                                    
                                    SolutionS2[i,] <- FishChase_Fun(S_i = Sol_i$S_i, S_peers = Sol_i$S_peers)
                        }
                        
                        fitness_update <- apply(SolutionS2, 1, FitnessFun)
                        
                        if(min(fitness_update) <= GlobalMin){
                                    GlobalMin <- min(fitness_update)
                                    GlobalSolution <- SolutionS2[which.min(fitness_update),]
                        }
                        
                        return(list("SolutionS" = SolutionS2,
                                    "GlobalMin" = GlobalMin,
                                    "GlobalSolution" = GlobalSolution))
            }
            
            
            
            
           
            # try_number=5; pop_size=4; crowdness=2;
            # SolutionS <- t(replicate(n = pop_size, sample(1:N, p ,replace = F)));
            # visual_mutual=2; GlobalSolution = SolutionS[2,]; GlobalMin = FitnessFun(GlobalSolution )
            # Fish_Optim_Fun(SolutionS, try_number, pop_size, crowdness,
            #                         visual_mutual, GlobalSolution, GlobalMin)

            
            #======================================================================================#
            # Main 
            SolutionS <- t(replicate(n = pop_size, sample(1:N, p ,replace = F)))
            FitnessSol <- apply(SolutionS, 1, FitnessFun)
            GlobalMin <- min(FitnessSol)
            GlobalSolution <- SolutionS[which.min(FitnessSol),]
            
            S <- gbest_solution <- list()
            pbest <- gbest <- c()
            
            start_time <- Sys.time()
            
            for (i in 1:ni) {
                        
                        cat("Fish_progress:",round((i/ni)*100,2),"% \n")
            
                        Solutions_update <- Fish_Optim_Fun(SolutionS = SolutionS, try_number = try_number,
                                                           pop_size = pop_size, crowdness = crowdness, 
                                                           visual_mutual = visual_mutual, 
                                                           
                                                           GlobalSolution = GlobalSolution,
                                                           GlobalMin = GlobalMin)
                        
                        GlobalMin <- Solutions_update$GlobalMin
                        GlobalSolution <- Solutions_update$GlobalSolution
                        
                        S[[i]] <- Solutions_update
                        pbest[i] <- min(apply(Solutions_update$SolutionS, 1, FitnessFun)) #min(FitnessFun())
                        gbest[i] <- GlobalMin
                        gbest_solution[[i]] <- GlobalSolution
                        
            }
            
            
            gbest_solution <- do.call(rbind, gbest_solution)
            GlobalMin_data <- data.frame(gbest_solution)
            colnames(GlobalMin_data) <- paste0("facility_", 1:p)
            GlobalMin_data$GlobalMin <- gbest
            
            end_time <- Sys.time()
            RunningTime <- end_time - start_time
            
            return(list("GlobalMin_data" = GlobalMin_data, "RunningTime" = RunningTime))
}



#====================================    TEST     =====================================#
#======================================================================================#

# N <- 100 # Number of candidates
# p <- 19 # Number of facilities
# ni <- 300 # Number of iterations
# n_costomers <- 200
# pop_size <- 30     # in each ABC, how many facilities solution are generated.(Number of Bees)
# 
# 
# crowdness <- 8
# visual_mutual <-  5
# try_number <- 10
# 
# weight_customer <- rchisq(n_costomers, df = 3)*10  # population of each customer point
# Distance.matrix <- matrix(rchisq(n = n_costomers*N, df = 5), nrow = n_costomers)
# 
# #--------------#
# result_fish <- Fish_Optim(N, p, ni, pop_size,
#                           Distance.matrix, weight_customer,
#                           try_number, crowdness, visual_mutual)
# 
# result_fish
# 
# fit_plot <- result_fish$GlobalMin
# plot(x = 1:nrow(fit_plot), fit_plot$GlobalMin, type = "l", main = "fish", xlab = "Iteration", 
#      ylab = "ObejectFun (distance)")
# # 
# # 
# 
# 






