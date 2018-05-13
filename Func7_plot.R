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

