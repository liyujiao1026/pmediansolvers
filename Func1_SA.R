
#===================================================#
SA_optim <- function(N,p,ni,
                     Distance.matrix, weight_customer, 
                     Tempearture_init, cool_rate){
            
            heuristic.solution <- matrix(0, nrow=ni,ncol=1)
            heuristic.location <- matrix(0, nrow=p, ncol=1)
            Store.solution <- numeric()
            Best.solution <- numeric()
            Store.location <- matrix(0, nrow=ni, ncol=p)
            Best.location <- numeric()
            
            subset <- numeric(0)
            select.location <- sample(1:N, p, replace=F)
            objective.function <- sum(apply(Distance.matrix[,select.location],1,min) * weight_customer)
            
            Tempearture <- Tempearture_init  #initial parameter setting.
            
            start_time <- Sys.time()
            
            for (i in 1:ni){
                        
                        cat("SA_progress:",round((i/ni)*100,2),"% \n")
                        
                        sam <- sample(1:p,1)
                        
                        # old solution---------------# 
                        substitution <- sample(setdiff(1:N, select.location), 1)
                        
                        
                        # new solution---------------# 
                        # store_Replaced <- select.location[sam]
                        # store_Replaced_coord <- c(x_store[store_Replaced], y_store[store_Replaced])
                        # range_sample <- 40
                        # neighbour_set <- which(x_store < (store_Replaced_coord[1] + range_sample) &
                        #                                    x_store > (store_Replaced_coord[1] - range_sample))
                        # 
                        # store_substitue_set <- setdiff(neighbour_set, select.location)
                        # substitution <- sample(store_substitue_set, 1)
                        # print(substitution)
                        #-----------------------------#
                        
                        
                        
                        
                        store.selection <- select.location
                        select.location[sam] <- substitution
                        
                        
                        updated.objective.function <- sum(apply(Distance.matrix[,select.location],1,min) * weight_customer)
                        
                        if (updated.objective.function <= objective.function){
                                    objective.function <- updated.objective.function
                                    #count <- 0
                        }else{
                                    delta<-updated.objective.function - objective.function
                                    unif.number<-runif(1,0,1)
                                    if (unif.number<exp(-delta/Tempearture)){
                                                objective.function<-updated.objective.function
                                                #beta<-0.5
                                                #count<-0
                                    }else{
                                                #count<-count+1
                                                select.location<-store.selection }
                        }
                       
                        Tempearture <- Tempearture * cool_rate
                        Store.solution[i]<-objective.function
                        Best.solution[i]<-min(Store.solution[1:i])
                        Store.location[i,]<-select.location
                        Best.location <- Store.location[min(which(Store.solution==Best.solution[i])),]
            }
            
            heuristic.solution[,1] <- Best.solution
            heuristic.location[,1] <- Best.location
            
            
            end_time <- Sys.time()
            RunningTime <- end_time - start_time
            
            return(list( "Global_Min" = heuristic.solution, "RunningTime" = RunningTime,
                         "Location" = heuristic.location
                         
                         ))
}



#====================================    TEST     =====================================#
#======================================================================================#

# N <- 100 # Number of candidates
# p <- 19 # Number of facilities
# ni <- 300 # Number of iterations
# n = 1
# 
# n_costomers <- 200
# weight_customer <- rchisq(n_costomers, df = 3)*10  # population of each customer point
# Distance.matrix <- matrix(rchisq(n = n_costomers*N, df = 5), nrow = n_costomers)
# 
# Tempearture_init <- 400
# result_SA <- SA_optim(N,p,ni,n=1,
#          Distance.matrix, weight_customer, Tempearture_init)
# 
# fit_plot.sa <- result_SA
# plot(x = 1:length(fit_plot.sa), y = fit_plot.sa, ylim = c(5000,7000),
#      type = "l", main = "SA", xlab = "Iteration", ylab = "ObejectFun (distance)")
# 




