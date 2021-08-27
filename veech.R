# Created by Vicente J. Ontiveros

veech <- function(table, minOccurrence, l.s.){
  
  Veech_calc <- function(N, j, N1, N2){
    (choose(N, j) * choose(N - j, N2 - j) * choose(N - N2, N1 - j)) / 
      (choose(N, N2) * choose(N, N1))
  }
  
  
  oc <- table
  oc[oc != 0] <- 1
  
  table <- table[rownames(table) %in% rownames(oc[rowSums(oc) >= minOccurrence,]),]
  table[table != 0] <- 1
  table <- t(table)
  
  occurs <- crossprod((table)) 
  
  res <- matrix(NA, nrow(occurs), ncol(occurs))
  for (i in 1:nrow(occurs)){
    for (j in 2:ncol(occurs)){
      if (j <= i) next
      out <- 0

      cooc <- occurs[i,j]
      for (k in 0:cooc){
        out <- out + Veech_calc(nrow(table), k, occurs[j,j], occurs[i,i])
      }
      res[i, j] <- out
    }
  }

  res.m <- data.frame(res)
  colnames(res.m) <- colnames(table)
  rownames(res.m) <- colnames(table)
  res.m$SP1 <- rownames(res.m)
  res.m <- reshape2::melt(res.m)
  res.m <- res.m[complete.cases(res.m$value),]
  res.m <- res.m[res.m$value < l.s.,]
  res.m$trans_value <- l.s./res.m$value
  res.m$log_value <- log(res.m$trans_value)
  negativeWeights <- res.m

  neg <- which(res < l.s., arr.ind = T) # Index less than expected cooccurrences.

  # The next lines are intended to prepare the results to use it with Blois.
  pairs.neg <- data.frame(matrix(NA, nrow = nrow(neg), ncol = 3))
  colnames(pairs.neg) <- c("pair_sp", "sp1_name", "sp2_name")
  for(i in 1:nrow(neg)){
    pairs.neg[i, 1] <- paste(colnames(table)[neg[i, 1]], colnames(table)[neg[i, 2]])
    pairs.neg[i, 2] <- colnames(table)[neg[i, 1]]
    pairs.neg[i, 3] <- colnames(table)[neg[i, 2]]

  }
  pairsNegative <- pairs.neg
  # 
  # 
  # And now the more than expected cooccurrences.
  res <- matrix(NA, nrow(occurs), ncol(occurs))
  for (i in 1:nrow(occurs)){
    for (j in 2:ncol(occurs)){
      if (j <= i) next
      out <- 0
      cooc <- occurs[i,j] - 1 
      for (k in 0:cooc){ 
        out <- out + Veech_calc(nrow(table), k, occurs[j,j], occurs[i,i]) 
      }
      res[i, j] <- out # out = probability of finding less than k cooccurences. 
    }
  }
  
  res.m <- data.frame(res)
  colnames(res.m) <- colnames(table)
  rownames(res.m) <- colnames(table)
  res.m$SP1 <- rownames(res.m)
  res.m <- reshape2::melt(res.m)
  res.m <- res.m[complete.cases(res.m$value),]
  res.m <- res.m[res.m$value > (1-l.s.),]
  res.m$value <- 1- res.m$value
  res.m[res.m$value <= 0, 3] <- 1.307754e-15
  res.m$trans_value <- l.s./res.m$value
  res.m$log_value <- log(res.m$trans_value)
  positiveWeights <- res.m
  
  
  pos <- which(res > (1 - l.s.), arr.ind = T) # Index more than expected cooccurrences.
  
  # The next lines are intended to prepare the results to use it with Blois.
  pairs.pos <- data.frame(matrix(NA, nrow = nrow(pos), ncol = 3))
  colnames(pairs.pos) <- c("pair_sp", "sp1_name", "sp2_name")
  
  for(i in 1:nrow(pos)){
    pairs.pos[i, 1] <- paste(colnames(table)[pos[i, 1]], colnames(table)[pos[i, 2]])
    pairs.pos[i, 2] <- colnames(table)[pos[i, 1]]
    pairs.pos[i, 3] <- colnames(table)[pos[i, 2]]
  }
  pairsPositive <- pairs.pos
  
  
  output <- list("pairsPositive" = pairsPositive, 
                 "positiveWeights" = positiveWeights,
                 "pairsNegative" = pairsNegative, 
                 "negativeWeights" = negativeWeights)
  
  return(output)
}
