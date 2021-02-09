blois <- function(table, envData, envlist, pairsPositive, pairsNegative, type = "both"){
  
  source("~/Desktop/PhD/R/Blois_functions_v2.R")
  
  #library(Hmisc)
  env <- envData
  env <- env[order(env$site),]
  #env$site <- env[,1]
  
  
  tab <- as.data.frame(t(table))
  tab <- tab[order(rownames(tab)),]
  tab[tab != 0] <- 1
  
  
  if(type == "env"){
    
    env$lat <- sample(1:300, nrow(env))
    env$lon <- sample(1:300, nrow(env))
    
    res <- list()
    
    # Positive
    for(i in envlist){
      
      if(sum(is.na(env[,i])) != 0){
        env[,i] <- as.numeric(impute(env[,i]))
      } else {NULL}
      
      env.doc <- env[,c("site", "lat", "lon", i)] 
      colnames(env.doc)<-c("site", "lat", "long", "variable")
      
      explain.pos <- get_together(veg = data.frame(tab), cooc = pairsPositive, envi = env.doc[,1:4], pos.neg = "pos", m = 3)
      
      res[[paste(i, "_positive", sep = "")]] <- explain.pos
      
    }
    
    # Negative
    for(i in envlist){

      if(sum(is.na(env[,i])) != 0){
        env[,i] <- as.numeric(impute(env[,i]))
      } else {NULL}

      env.doc <- env[,c("site", "lat", "lon", i)]
      colnames(env.doc)<-c("site", "lat", "long", "variable")

      explain.neg <- get_together(veg = data.frame(tab), cooc = pairsNegative, envi = env.doc[,1:4], pos.neg = "neg", m = 3)

      res[[paste(i, "_negative", sep = "")]] <- explain.neg
    }
    
  } else {
    
    res <- list()
    
    # Positive
    for(i in envlist){
      
      if(sum(is.na(env[,i])) != 0){
        env[,i] <- as.numeric(impute(env[,i]))
      } else {NULL}
      
      env.doc <- env[,c("site", "lat", "lon", i)] 
      colnames(env.doc)<-c("site", "lat", "long", "variable")
      
      explain.pos <- get_together(veg = data.frame(tab), cooc = pairsPositive, envi = env.doc[,1:4], pos.neg = "pos", m = 3)
      
      res[[paste(i, "_positive", sep = "")]] <- explain.pos
      
    }
    
    # Negative
    for(i in envlist){

      if(sum(is.na(env[,i])) != 0){
        env[,i] <- as.numeric(impute(env[,i]))
      } else {NULL}

      env.doc <- env[,c("site", "lat", "lon", i)]
      colnames(env.doc)<-c("site", "lat", "long", "variable")

      explain.neg <- get_together(veg = data.frame(tab), cooc = pairsNegative, envi = env.doc[,1:4], pos.neg = "neg", m = 3)

      res[[paste(i, "_negative", sep = "")]] <- explain.neg
    }
    
    
    }
  
  return(res)
}
