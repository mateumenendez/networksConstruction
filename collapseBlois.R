# Created by Mateu Menéndez-Serra

collapseBlois <- function(table){
  
  order_acc <- c()
  value_acc <- c()
  max_simples_order_variables <- c()
  variables <- unique(tot$var)
  
  for(i in variables){
    temp <- tot[tot$var == i,]
    y <- temp$env_var == "Dispersal limitation and/or Environmental filtering" | temp$env_var == "Environmental filtering"
    z <- sum(y, na.rm = T)
    max_simples_order_variables <- c(max_simples_order_variables, z)
    rm(temp,y,z,i)
  }
  
  order_acc <- c(order_acc, variables[which.max(max_simples_order_variables)])
  temp <- tot[tot$var == variables[which.max(max_simples_order_variables)],]
  j <- temp$env_var == "Dispersal limitation and/or Environmental filtering" | temp$env_var == "Environmental filtering"
  rm(temp)
  value_acc <- c(value_acc, sum(j, na.rm = T))
  variables <- variables[-which.max(max_simples_order_variables)]
  
  while(length(variables) >= 1){
    max <- c()
    for(i in variables){
      temp <- tot[tot$var == i,]
      y <- temp$env_var == "Dispersal limitation and/or Environmental filtering" | temp$env_var == "Environmental filtering"
      w <- (y | j) 
      z <- sum(w, na.rm = T)
      max <- c(max, z)
      rm(temp,i,y,w,z)
    }
    
    order_acc <- c(order_acc, variables[which.max(max)])
    temp <- tot[tot$var == variables[which.max(max)],]
    k <- temp$env_var == "Dispersal limitation and/or Environmental filtering" | temp$env_var == "Environmental filtering"
    j <- (j | k)
    value_acc <- c(value_acc, sum(j, na.rm = T))
    variables <- variables[-which.max(max)]
  }
  
  # Environmental filtering
  env_filter_acc_order <- c()
  for(i in order_acc){
    temp <- tot[tot$var == i,]
    y <- temp$env_var == "Environmental filtering"
    z <- sum(y, na.rm = T)
    env_filter_acc_order <- c(env_filter_acc_order, z)
    rm(temp,z,y,i)
  }
  
  # Environmental filtering and dispersal limitation
  env_disp_acc_order <- c()
  for(i in order_acc){
    temp <- tot[tot$var == i,]
    y <- temp$env_var == "Dispersal limitation and/or Environmental filtering" | temp$env_var == "Environmental filtering"
    z <- sum(y, na.rm = T)
    env_disp_acc_order <- c(env_disp_acc_order, z)
    rm(temp,z,y,i)
  }
  
  #Dispersal limitation
  disp_lim_all <- rep(TRUE, nrow(tot)/length(unique(tot$var)))
  for(i in order_acc){
    temp <- tot[tot$var == i,]
    y <- temp$env_var == "Dispersal limitation"
    disp_lim_all <- c(disp_lim_all & y)
    rm(temp, y)
  }
  disp_lim_all <- sum(disp_lim_all, na.rm = T)
  
  variables <- unique(tot$var)
  
  value_acc <- value_acc/(nrow(tot)/length(unique(tot$var)))
  env_filter_acc_order <- env_filter_acc_order/(nrow(tot)/length(unique(tot$var)))
  disp_lim_all <- disp_lim_all/(nrow(tot)/length(unique(tot$var)))
  env_disp_acc_order <- env_disp_acc_order/(nrow(tot)/length(unique(tot$var)))
  
  to_plot <- data.frame(variables = order_acc, simples = env_disp_acc_order, acumulado = value_acc, env = env_filter_acc_order)
  to_plot$dis_env <- to_plot$simples - to_plot$env
  to_plot$variables <- factor(to_plot$variables, levels = unique(to_plot$variables[order(to_plot$acumulado)]))
  to_plot <- tidyr::gather(to_plot, condition, perc, simples:dis_env, factor_key=TRUE)
  to_plot$condition <- factor(to_plot$condition, levels(as.factor(to_plot$condition))[c(1:2, 4, 3)])
  
  return <- list("for_plotting" = to_plot, 
                 "dispersal_limitation" = disp_lim_all)
  
  return(return)
  
}
