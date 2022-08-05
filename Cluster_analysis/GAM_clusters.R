library(mgcv)
library(dplyr)


setwd("~/documents/chinook_growth")
# Salmon growth data 





#temp <- function(t){0.5/(1+0.085*t)} # original temp function 
#temp <- function(t){0.8/(1+0.1*t)} 
temp <- function(t){0.5/(1+0.075*t)}


#'logLik_best_mod(df, best_model, cluster)
#'computes the linklihood for the observaitons in df
#'assuming all the observaitons are in cluster cluster
logLik_best_mod<- function(df, best_model, cluster,ncluster,nstock){
  # set cluster
  df$cluster <- cluster
  # get predictions 
  pred <- predict(best_model,df)
  
  return(sum(log(dnorm(df$length, pred, best_model$sig2))))
}

### Run expectation maximization 

#' expectation_maximization(df_means, age, ncluster, num_iter, temp)
#' This function runs the expecation maximization algoriths
#' on dat sets with the structure of "transformed_data/increment_model_data.csv"
#' It takes the data set an age, a number of clusters and parameters
#' for the simulation and returns the data set with a column appended with
#' the cluster assignments and a gam model object fit to the data with these
#' cluster assignents. It also returns a matrix that gives the likelihood 
#' of each stock belonging to each cluster in a matrix 
#' df_means - dat frame with means column names to match  "transformed_data/increment_model_data.csv"
#' age - 3,4 or 5
#' ncluster - integer greater than 1
#' num_iter - number of iterations of EM algorithn
#' temp - amount of randomness intoduced to algorthm. 
expectation_maximization1 <- function(df_means,  ncluster, num_iter, temp, seed){
  ##### process data
  set.seed(seed)
  df_stock <- data.frame(stock = unique(df_means$stock),
                         cluster = sample(1:ncluster, replace = T, length(unique(df_means$stock))) )
  df_means <- merge(df_means, df_stock, by = "stock")
  df_run <- df_means
  best_aic <- 1000000000
  best_clusters <- df_run$cluster
  acc_return <- 0
  acc_diagnos <- c()
  for(step in 1:num_iter){
    acc <- 0
    mod5 <- gam(data=df_run , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))
    aic <- mod5$aic
    mse_acc <- 0
    if(mod5$aic < best_aic){
      print("New maximum value")
      best_aic <- aic
      best_clusters <- df_run$cluster
      acc_return <- 0
    }else{
      acc_return <- acc_return + 1
    }
    for(i in unique(df_run$stock)){
      best <- 10000000000 ## set to a large number
      best_clust <- 0
      for(j in 1:ncluster){
        df <- df_run %>% dplyr::filter(stock == i)
        df$cluster <- rep(j, nrow(df))
        preds <- predict(mod5, df)
        mse <- sum((preds-as.numeric(df$length))^2)
        if(mse < best){
          best_clust <- j
          best <- mse
        }
      }
      mse_acc <- mse_acc + best
      if(temp(step) > runif(1)){
        best_clust <- sample( 1:ncluster,1)
        acc <- acc + 1
      }
      df_run$cluster[df_run$stock == i ] <- rep(best_clust, nrow(df))
    }
    # if(acc_return > 500){
    #   print("back to previous maximum")
    #   acc_return <- 0
    #   df_run$cluster <- best_clusters
    # }
    if(any(sort(unique(df_run$cluster)) != 1:ncluster )){
      print("here")
      print(unique(df_run$cluster))
      stock <- sample(unique(df_run$stock),1)
      df_run$cluster[df_run$stock == stock] <- rep(setdiff(1:ncluster,unique(df_run$cluster)), nrow(df))
    }
    acc_diagnos <- append(acc_diagnos, mse_acc)
    print(paste("itteration:", step, "aic value", aic, "number of deviations:", acc , "best mse:", mse_acc))
  }
  df_run$cluster <- best_clusters
  best_model <- gam(data=df_run ,
                    length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))
  
  # loop over stocks and over clusters to get likelihoods
  # assuming variance between clusters is
  liklihoods <- matrix(0,
                       nrow = length(unique(df_run$stock)),
                       ncol = ncluster)
  for(s in unique(df_run$stock)){
    row <- c()
    total <- 0
    for(cluster in 1:ncluster){
      df <- df_run[df_run$stock==s,]
      p <- logLik_best_mod(df, best_model, cluster,ncluster,length(unique(df_run$stock)))
      row <- append(row, p)
      total <- total + p
    }
    row <- exp(row)/sum(exp(row))
    liklihoods[s,] <- row
  }
  liklihoods <- as.data.frame(liklihoods)
  liklihoods$stock <- unique(df_run$stock)
  return(list(dat = df_run, model = best_model,
              likelihoods = liklihoods,
              covergance = data.frame(x = 1:num_iter,y = acc_diagnos),
              AIC = best_aic))

}


gam_k_means_2 <- function(df_means,  ncluster, num_iter, temp, seed){
  ##### process data
  set.seed(seed)
  df_stock <- data.frame(stock = unique(df_means$stock),
                         cluster = sample(1:ncluster, replace = T, length(unique(df_means$stock))) )
  df_means <- merge(df_means, df_stock, by = "stock")
  df_run <- df_means
  best_total_mse <- 100000000000
  best_clusters <- df_run$cluster

  acc_diagnos <- c()
  for(step in 1:num_iter){ # loop over number of iterations
    
    
    # fit model 
    mod5 <- gam(data=df_run , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))
    aic <- mod5$aic
    
    # get model MSE
    preds <- preds <- predict(mod5, df_run)
    total_mse <- sum((preds-as.numeric(df_run$length))^2)
    
    # if mse is lower than previous maximum update best clusters
    if(total_mse < best_total_mse){
      print("New maximum value")
      best_total_mse <- total_mse
      best_clusters <- df_run$cluster
    }
    
    

    acc <- 0 # number of randomly assigned clusters
    for(i in unique(df_run$stock)){  # loop ovser stocks 
      
      best_mse <- 100000000000 # track best cluster mse
      best_clust <- 0 # best cluster 
      
      for(j in 1:ncluster){# loop over clusters
        # calcualte MSE
        df <- df_run %>% dplyr::filter(stock == i)
        df$cluster <- rep(j, nrow(df))
        preds <- predict(mod5, df)
        mse <- sum((preds-as.numeric(df$length))^2)
        
        # compare to previous best mse, replace if imporved 
        if(mse < best_mse){
          best_clust <- j
          best_mse <- mse
        }
        
      } # end loop over clusters 
  
      # randomply assign some stocks to a cluster 
      if(temp(step) > runif(1)){
        best_clust <- sample( 1:ncluster,1)
        acc <- acc + 1
      }
      
      # updated cluster assignment
      df_run$cluster[df_run$stock == i ] <- rep(best_clust, nrow(df))
      
    }# end loop over stocks 

    
    # check if not stocks were assigned to a cluster
    # sample a random stock and assign it to the empty cluster if so
    if(any(sort(unique(df_run$cluster)) != 1:ncluster )){
      print("Warning: empty cluster")
      print(unique(df_run$cluster))
      stock <- sample(unique(df_run$stock),1)
      df_run$cluster[df_run$stock == stock] <- rep(setdiff(1:ncluster,unique(df_run$cluster)), nrow(df))
    }
    
    
    acc_diagnos <- append(acc_diagnos, total_mse)
    print(paste("itteration:", step, "mse value", total_mse, "number of deviations:", acc , "best mse:", best_total_mse))
  
  } # end interations 
  
  
  df_run$cluster <- best_clusters
  best_model <- gam(data=df_run ,
                    length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))
  
  # Calcualte MSE
  preds <- preds <- predict(best_model, df_run)
  total_mse <- sum((preds-as.numeric(df_run$length))^2)
  
  #  Calcualte "likelihoods" using log sum exponential assuming
  #  clusters ar normally distributed around the mean funciton
  #  With constant equal variances. 
  liklihoods <- matrix(0,
                       nrow = length(unique(df_run$stock)),
                       ncol = ncluster)
  
  for(s in unique(df_run$stock)){
    row <- c()
    total <- 0
    for(cluster in 1:ncluster){
      df <- df_run[df_run$stock==s,]
      p <- logLik_best_mod(df, best_model, cluster,ncluster,length(unique(df_run$stock)))
      row <- append(row, p)
      total <- total + p
    }
    row <- exp(row)/sum(exp(row))
    liklihoods[s,] <- row
  }
  
  liklihoods <- as.data.frame(liklihoods)
  liklihoods$stock <- unique(df_run$stock)
  
  
  return(list(dat = df_run, model = best_model,
              likelihoods = liklihoods,
              covergance = data.frame(x = 1:num_iter,y = acc_diagnos),
              MSE = total_mse))
  
}



# 
# 
# 
# 
# # Compute likelihood for each stock in each group
# # same as able but it saves data from each iteration of the animation to 
# # help visualize the algorithm
# expectation_maximization_steps <- function(df_means,  ncluster, num_iter, temp){
#   ##### process data 
#   df_stock <- data.frame(stock = unique(df_means$stock), 
#                          cluster = sample(1:ncluster, replace = T, length(unique(df_means$stock))) )
#   df_means <- merge(df_means, df_stock, by = "stock")
#   df_run <- df_means
#   best_aic <- 1000000000
#   best_clusters <- df_run$cluster
#   acc_return <- 0
#   acc_diagnos <- c()
#   df_steps <- df_run
#   df_steps$iter <- 1
#   for(step in 1:num_iter){
#     acc <- 0
#     mod5 <- gam(data=df_run , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))
#     aic <- mod5$aic
#     mse_acc <- 0
#     if(mod5$aic < best_aic){
#       print("New maximum value")
#       best_aic <- aic
#       best_clusters <- df_run$cluster
#       acc_return <- 0
#     }else{
#       acc_return <- acc_return + 1
#     }
#     for(i in unique(df_run$stock)){
#       best <- 10000000000
#       best_clust <- 0
#       for(j in 1:ncluster){
#         
#         df <- df_run %>% dplyr::filter(stock == i)
#         df$cluster <- rep(j, nrow(df))
#         preds <- predict(mod5, df)
#         mse <- sum((preds-as.numeric(df$length))^2)
#         
#         if(mse < best){
#           best_clust <- j
#           best <- mse
#         }
#       }
#       
#       mse_acc <- mse_acc + best
#       if(temp(step) > runif(1)){
#         best_clust <- sample( 1:ncluster,1)
#         acc <- acc + 1
#       }
#       
#       df_run$cluster[df_run$stock == i ] <- rep(best_clust, nrow(df))
#     }
# 
#     
#     if(any(sort(unique(df_run$cluster)) != 1:ncluster )){
#       print("here")
#       print(unique(df_run$cluster))
#       stock <- sample(unique(df_run$stock),1)
#       df_run$cluster[df_run$stock == stock] <- rep(setdiff(1:ncluster,unique(df_run$cluster)), nrow(df))
#     }
#     
#     df_steps <- rbind(df_steps, cbind(df_run, data.frame(iter = rep(step, nrow(df_run)))))
#     acc_diagnos <- append(acc_diagnos, mse_acc)
#     print(paste("itteration:", step, "aic value", aic, "number of deviations:", acc , "best mse:", mse_acc))
#   
#   }
#   df_run$cluster <- best_clusters
#   f_steps <- rbind(df_steps, cbind(df_run, data.frame(iter = rep(step, nrow(df_run)))))
#   best_model <- gam(data=df_run , 
#                     length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))
#   # loop over stocks and over clusters to get likelihoods
#   liklihoods <- matrix(0,
#                        nrow = length(unique(df_run$stock)),
#                        ncol = ncluster)
#   for(s in unique(df_run$stock)){
#     row <- c()
#     total <- 0
#     for(cluster in 1:ncluster){
#       df <- df_run[df_run$stock==s,] 
#       p <- logLik_best_mod(df, best_model, cluster,ncluster,length(unique(df_run$stock)))
#       row <- append(row, p)
#       total <- total + p
#     }
#     row <- exp(row)/sum(exp(row))
#     liklihoods[s,] <- row
#   }
#   liklihoods <- as.data.frame(liklihoods)
#   liklihoods$stock <- unique(df_run$stock)
#   return(list(dat_steps = df_steps, dat = df_run, model = best_model, likelihoods = liklihoods, covergance = data.frame(x = 1:num_iter,y = acc_diagnos)))
#   
# }






# gam_k_means(X,  k, num_iter, temp, seed)
# X
gam_k_means <- function(X,  k, num_iter, temp, seed){
  ##### process data 
  set.seed(seed)
  I <- data.frame(i = unique(X$i), 
                         cluster = sample(1:k, replace = T, length(unique(X$i))) )
  X <- merge(X, I, by = "i")
  print(head(X))
  best_aic <- 1000000000
  best_clusters <- X$cluster
  acc_return <- 0
  acc_diagnos <- c()
  for(step in 1:num_iter){
    acc <- 0
    mod <- gam(data=X , x ~ as.factor(cluster) + s(t, by =as.factor(cluster) ))
    aic <- mod$aic
    mse_acc <- 0
    if(mod$aic < best_aic){
      print("New maximum value")
      best_aic <- aic
      best_clusters <- X$cluster
      acc_return <- 0
    }else{
      acc_return <- acc_return + 1
    }
    for(el in unique(X$i)){
      best <- 10000000000 ## set to a large number 
      best_clust <- 0
      # print(paste("i value:",el))
      for(j in 1:k){
        Xi<- X %>% dplyr::filter(i == el)
        Xi$cluster <- rep(j, nrow(Xi))
        preds <- predict(mod, newdata = Xi)
        mse <- sum((preds-Xi$x)^2)
        # print(mse)
        if(mse < best){
          best_clust <- j
          best <- mse
        }
      }
      mse_acc <- mse_acc + best
      if(temp(step) > runif(1)){
        best_clust <- sample( 1:k,1)
        acc <- acc + 1
      }
      # print(best_clust )
      X$cluster[X$i == el ] <- rep(best_clust, length(X$cluster[X$i == el ]))
    }

    if(any(sort(unique(X$cluster)) != 1:k )){
      print("here")
      print(unique(X$cluster))
      i <- sample(unique(X$i),1)
      X$cluster[X$i == i] <- rep(setdiff(1:k,unique(X$cluster)), nrow(Xi))
    }
    acc_diagnos <- append(acc_diagnos, mse_acc)
    print(paste("itteration:", step, "aic value", aic, "number of deviations:", acc , "best mse:", mse_acc))
  }
  X$cluster <- best_clusters
  best_model <- gam(data=X , 
                    x ~ as.factor(cluster) + s(t, by =as.factor(cluster) ))
  # Calcualte MSE

  preds <- preds <- predict(best_model, newdata = X)

  best_mse <- sum((preds-as.numeric(X$x))^2)
  print(best_mse)
  
  return(list(dat = X, model = best_model, 
              covergance = data.frame(x = 1:num_iter,y = acc_diagnos),
              MSE = best_mse))
  
}






