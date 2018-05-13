
Compare_Algorithm_Fun <- function(

            N,p,ni,pop_size,
            Distance.matrix, weight_customer,


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
            path_save){


            # 1. Simulated Annealing Algorithm
            fitness_SA_result <- SA_optim(N,p,ni,
                                          Distance.matrix, weight_customer,
                                          Tempearture_init = SA_temparature, SA_cool_rate)
            fitness_SA <- as.numeric(fitness_SA_result$Global_Min)
            time_SA <- fitness_SA_result$RunningTime
            saveRDS(object = fitness_SA, file = paste0(path_save, "fitness_SA"))
            saveRDS(object = time_SA, file = paste0(path_save, "time_SA"))
            #--------------------------------------------------------------------------------#

            # 2. Genetic Algorithm
            fitness_GA_result <- GA_Optim(N,p,ni,pop_size,
                                          weight_customer, Distance.matrix,
                                          prob_cross = GA_prob_cross ,prob_mutation = GA_prob_cross,
                                          prob_refine = GA_prob_cross)
            fitness_GA <- fitness_GA_result$GlobolMin
            time_GA <- fitness_GA_result$RunningTime
            saveRDS(object = fitness_GA, file = paste0(path_save, "fitness_GA"))
            saveRDS(object = time_GA, file = paste0(path_save, "time_GA"))
            #--------------------------------------------------------------------------------#

            # 3. PSO algorithm
            fitness_pso_result <- PSO_optim(N, p, ni, pop_size,
                                            Distance.matrix, weight_customer,
                                            c1 = PSO_c1, c2 = PSO_c2)
            fitness_PSO <- fitness_pso_result$GlobalMin_data$GlobalMin
            time_PSO <- fitness_pso_result$RunningTime
            saveRDS(object = fitness_PSO, file = paste0(path_save, "fitness_PSO"))
            saveRDS(object = time_PSO, file = paste0(path_save, "time_PSO"))
            #--------------------------------------------------------------------------------#

            # 4. Bee algorithm
            fitness_bee_result <- ABC_Optim(N,p,ni,pop_size,
                                            max_trial = BEE_maxtrial,
                                            Distance.matrix, weight_customer)
            fitness_BEE <- fitness_bee_result$GlobalMin
            time_BEE <- fitness_bee_result$RunningTime
            saveRDS(object = fitness_BEE, file = paste0(path_save,"fitness_BEE"))
            saveRDS(object = time_BEE, file = paste0(path_save, "time_BEE"))
            #--------------------------------------------------------------------------------#

            # 5. Fish algorithm
            fitness_fish_result <- Fish_Optim(N, p, ni, pop_size= pop_size,
                                              Distance.matrix, weight_customer,
                                              try_number = FISH_try_number, crowdness = FISH_crowdness,
                                              visual_mutual = FISH_visual_mutual)
            fitness_FISH <- fitness_fish_result$GlobalMin$GlobalMin
            time_FISH <- fitness_fish_result$RunningTime
            saveRDS(object = fitness_FISH, file = paste0(path_save,"fitness_FISH"))
            saveRDS(object = time_FISH, file = paste0(path_save, "time_FISH"))
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

            fitness_opt <- min(unlist(lapply(fitness_all, min)))
            fitness_error <- lapply(fitness_all, function(x){(x - fitness_opt)/fitness_opt })

            saveRDS(object = compute_time, file = paste0(path_save,"compute_time"))
            saveRDS(object = fitness_error, file = paste0(path_save,"fitness_error"))


            return(list("fitness_error" = fitness_error,
                        "fitness_all" = fitness_all,
                        "compute_time" = compute_time,
                        "fitness_opt" = fitness_opt ))

}

library(compiler)
Compare_Algorithm_Func <- cmpfun(Compare_Algorithm_Fun)

#=============================== plot =============================#

plotFun <- function(result_compare){

            fit <- result_compare$fitness_all
            error <- result_compare$fitness_error


            PlotFunc <- function(compare_output, lowP, upP, ylab_v){

                        #fitness_SA, fitness_GA, fitness_PSO, fitness_BEE, fitness_FISH
                        mi <- min(unlist(lapply(compare_output, min)))
                        ma <- max(unlist(lapply(compare_output, max)))
                        ylim_range <- c(lowP*mi, upP*ma)


                        plot(x = 1:length(compare_output[[1]]), y = compare_output[[1]], ylim = ylim_range, lwd = 3,
                             type = "l", main = "Comparison of meta-heuristic algorithm",
                             xlab = "Iteration",
                             ylab = ylab_v) #"fitness (distance)"

                        lines(x = 1:length(compare_output[[2]]), y = compare_output[[2]],
                              ylim = ylim_range, col = "red",
                              lty = 1, lwd = 2)

                        lines(x = 1:length(compare_output[[3]]), y = compare_output[[3]],
                              ylim = ylim_range, col = "blue",
                              lty = 1, lwd = 2)

                        lines(x = 1:length(compare_output[[4]]), y = compare_output[[4]],
                              ylim = ylim_range, col = "orange",
                              lty = 1, lwd = 2)

                        lines(x = 1:length(compare_output[[5]]), y = compare_output[[5]],
                              ylim = ylim_range, col = "green",
                              lty = 1, lwd = 2)


                        legend("topright",
                               legend = c("Simulated Annealing","Genetic Algorithm","Particle Swarm",
                                          "Artifical Bee","Artificial Fish"),
                               col = c("black","red","blue","orange","green"),
                               lty = c(1,1,1,1,1), lwd = c(3,2,2,2,2))
            }




            P1.fit <- PlotFunc(fit, ylab_v = "fitness (distance)" ,lowP = 0.99, upP = 1.01)

            P2.error <- PlotFunc(error, ylab_v = "percentile deviation" ,lowP = 0.9, upP = 1.1)

            return(list(P1.fit, P2.error))


}





# Test========================#

# set.seed(1)
# # 1. P-median problem settings
# N <- 193 # Number of candidates
# p <- 5 # Number of facilities
# n_costomers <- 187 # Number of customer buildings
#
#
# # 2. Iteration settings
# ni <- 40 # Number of iterations
# pop_size <- 50
#
#
# # 3. Parameters settings
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
#             SA_temparature,  SA_cool_rate,
#             GA_prob_refine,  GA_prob_cross,GA_prob_mutation,
#             PSO_c1, PSO_c2,
#             BEE_maxtrial,
#             FISH_crowdness, FISH_visual_mutual, FISH_try_number,
#             path_save = "/Users/Yujiao/Desktop/pascal/p_median_Rcode/")
#
#
# plotFun(result_compare)


