run.fun <- function(k, eps = 1e-6, mu, Sigma,
                    maxWeights, minWeights) {
  # eps is the allowable violation of maxWeights
  include <- sort(as.integer(k[1:5]))
  Twp <- as.numeric(k[6])
  mu0 <- mu[include]
  Sigma0 <- Sigma[include,include]
  if(!(any(mu0 <= Twp) & any(mu0>= Twp)))
    return(c(NA, NA, NA))
  
  Amat <- cbind(1, 
                mu0, 
                -diag(length(mu0)),
                diag(length(mu0)))
  bvec <- c(1, Twp, -maxWeights[include], minWeights[include])
  sol <- solve.QP(Dmat=Sigma0, 
                  dvec = rep(0, length(include)),
                  Amat= Amat,
                  bvec = bvec, 
                  meq = 2)
  
  
  return(
    c("poweroutput"     = mu[include]%*%sol$solution, 
    "WeightConstraint"= as.numeric(all(sol$solution <= maxWeights[include]+eps)),
    "StdDev"          = sqrt(sol$solution %*% Sigma0 %*% sol$solution)))
}

run.5L.optimization <- function(grid,
                                mu, 
                                Sigma, 
                                source = "NVE", 
                                minWeights = rep(0, length(mu)),
                                maxWeights = rep(1,length(mu)),
                                cl = NULL){
  # Function for optimizing selection of 5 locations: 
  
  if(is.null(cl)) {
    warning("Create cluster")
    cl <- makeCluster(1)
  }
  clusterExport(cl, varlist = c("run.fun"))
  clusterEvalQ(cl, expr={

    library(dplyr)
    library(quadprog)
  })
  t1 <- Sys.time()
  portfolios.grid.sd <- t(parApply(cl, X= grid, MARGIN = 1,
                                 function(k, mu, Sigma,
                                          maxWeights, minWeights) {
                                     try(
                                       run.fun(k,
                                        mu = mu, 
                                        Sigma = Sigma,
                                        maxWeights=maxWeights,
                                        minWeights = minWeights), silent =T)
                                   },
                                 mu = mu, 
                                 Sigma = Sigma,
                                 maxWeights = maxWeights,
                                 minWeights = minWeights))
  t2 <- Sys.time()
  print(t2-t1)

  
  k <- sapply(portfolios.grid.sd, function(x) !inherits(x, "try-error"))
  if(all(k)){
    tst <-cbind(1:nrow(portfolios.grid.sd),portfolios.grid.sd) %>% as_tibble()
  }else{
    tst <- t(sapply(which(k), function(x) c(x, portfolios.grid.sd[[x]]), simplify = TRUE)) %>% as_tibble()
  }
  #tst[1:6,1:10]
  
  names(tst) <- c("k","poweroutput","WeightConstraint", "StdDev")
  tst$k <- as.integer(tst$k)
  
  grid.solved <- grid %>% 
    mutate(k = 1:n()) %>%
    as_tibble() %>% 
    left_join(tst, by = "k")
  best <- grid.solved %>% 
    filter(!is.na(poweroutput)) %>% 
    group_by(powertarget) %>% 
    filter(StdDev == min(StdDev)) 
  
  
  portfolios <- tibble(locID = NA, weights=NA, powertarget=NA, source=NA)
  for(k in 1:nrow(best)){
    include <- c(unlist(best[k, 1:5]))
    mu0 <- mu[include]
    Sigma0 <- Sigma[include,include]
    Amat <- cbind(1, 
                  mu0, 
                  -diag(length(mu0)),
                  diag(length(mu0)))
    bvec <- c(1, best[k,6], -maxWeights[include], minWeights[include])
    sol <- solve.QP(Dmat=Sigma0, 
                    dvec = rep(0, length(include)),
                    Amat= Amat,
                    bvec = bvec, 
                    meq = 2)
    portfolios <- portfolios %>% 
      add_row(locID = include, 
              weights = sol$solution, 
              powertarget = best$powertarget[k], 
              source = source)
    
  }
  portfolios <- portfolios %>% filter(!is.na(source))
  return(portfolios)
}


optimize_portfolio <- function(Twp = .6, 
                               mu, 
                               Sigma, 
                               locID = 1:length(mu),
                               minWeights = rep(0, length(mu)),
                               maxWeights = rep(1, length(mu))) {
  # eps is the allowable violation of maxWeights
  if(!(any(mu <= Twp) & any(mu>= Twp)))
    return(NA)
  
  Amat <- cbind(1, 
                mu, 
                -diag(length(mu)),
                diag(length(mu)))
  bvec <- c(1, Twp, -maxWeights, minWeights)
  sol <- solve.QP(Dmat=Sigma, 
                  dvec = rep(0, length(mu)),
                  Amat= Amat,
                  bvec = bvec, 
                  meq = 2)
  
  
  return(
    tibble(locID = locID,
           weights = sol$solution,
           powertarget = Twp,
           std = as.numeric(sqrt(sol$solution%*% Sigma%*%sol$solution)))
  )
}


iterative.build <- function(Twp = .58, start.locs = c(13, 19), mu, Sigma, maxWeights = rep(1,length(mu)),
                            stepsize = 1.5, maxGW = 30, find.initial.weights = FALSE){
  # Start with UN and SN2 
  # 1500 MW at each location
  # add another location or build more 
  step = 1
  candidates <- 1:length(mu)
  locations <- start.locs
  maxWeights2 <- maxWeights*30/3
  if(all(maxWeights == 1))
    maxWeights2 <- maxWeights
  if(find.initial.weights){
    weights <- optimize_portfolio(Twp = Twp,
                       mu= mu[locations],
                       Sigma = Sigma[locations,locations],
                       locID = locations,
                       minWeights = rep(0,2),
                       maxWeights = maxWeights2[locations])$weights
  }else{
    weights <- rep(.5,2)
    }
  total = 3 # 1.5 at each 
  portfolios <- tibble(step = step, 
                       locID = locations, 
                       weights = weights,
                       total = total,
                       powertarget = Twp, 
                       std = as.numeric(sqrt(weights%*% Sigma[locations,locations]%*%weights)))
  minWeights <- weights * total / (total + stepsize)
  
  while(total < maxGW){
    total <- total + stepsize
    if(!all(maxWeights == 1)){
      maxWeights2 <- maxWeights*30/total
    }
    cand <- candidates[-locations]
    port <- list()
    std <- numeric(length(cand)+1)
    port[[1]] <- try(optimize_portfolio(Twp = Twp,
                                    mu= mu[locations],
                                    Sigma = Sigma[locations,locations],
                                    locID = locations,
                                    minWeights = minWeights,
                                    maxWeights = maxWeights2[locations]), 
                 silent = TRUE)
    std[1] <- ifelse(any(inherits(port[[1]], "try-error") | is.na(port[[1]])),
                     NA,
                     port[[1]]$std[1])
    

    for(ind in 1:length(cand)){
      cand.locs <- c(locations, cand[ind])
      port[[1L+ind]] <- try(optimize_portfolio(Twp = Twp,
                         mu= mu[cand.locs],
                         Sigma = Sigma[cand.locs,cand.locs],
                         locID =cand.locs,
                         minWeights = c(minWeights, 0),
                         maxWeights = maxWeights2[cand.locs]), 
          silent = TRUE)
      std[1L+ind] <- ifelse(any(inherits(port[[1L+ind]], "try-error")|is.na(port[[1L+ind]])),
                            NA,
                            port[[1L+ind]]$std[1])
    }
    if(all(is.na(std))){
      stop(paste0("Iteration stopped at ", step, " with total ", total, "GW placed."))
    }
    newcand <- which.min(std)-1
    if(newcand != 0) # newcand = 0 means no new location added
      locations <- c(locations, cand[newcand])
    newweights <- port[[which.min(std)]]$weights
    minWeights <- newweights * total / (total + stepsize)
    step <- step +1
    portfolios <- portfolios %>% add_case(
      step = step, 
      locID = locations, 
      weights = newweights,
      total = total,
      powertarget = Twp, 
      std = std[which.min(std)]
    ) %>% arrange(step, locID)
    }
    return(list(
      "locations" = locations,
      "allsteps" = portfolios,
      "final" = portfolios %>% filter(step == max(step))
      ))
    
}
