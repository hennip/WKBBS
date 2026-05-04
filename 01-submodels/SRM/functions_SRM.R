# Jags data ####
# function to run with a subset of rivers (user selected)
build_jags_data <- function(selected_rivers) {
  
  river_idx <- match(selected_rivers, colnames(IP0))
  # keep only rows belonging to selected rivers
  keep <- age_river_full %in% river_idx
  
  age_count_sub <- age_count_full[keep, , drop = FALSE]
  n_aged_sub    <- n_aged_full[keep]
  age_year_sub  <- age_year_full[keep]
  age_river_sub <- age_river_full[keep]
  # IMPORTANT: remap river indices to local 1:ncol(IP0_sub)
  age_river_sub <- match(age_river_sub, river_idx)
  n_age_obs <- nrow(age_count_sub)
  
  IP0_sub <- IP0[, river_idx, drop = FALSE]
  IP1_sub <- IP1[, river_idx, drop = FALSE]
  IS_sub  <- IS[, river_idx, drop = FALSE]
  CIS_sub <- CIS[, river_idx, drop = FALSE]
  n_sub   <- n[, river_idx, drop = FALSE]
  
  size_sub   <- size[, river_idx, drop = FALSE]
  size_dev_sub   <- size_dev[, river_idx, drop = FALSE]
  size_dev_river_sub   <- size_dev_river[, river_idx, drop = FALSE]
  
  parr0_dev_river_sub   <- parr0_dev_river[, river_idx, drop = FALSE]
  parr0_sub   <- parr0[, river_idx, drop = FALSE]
  
  EA_sub <- EA[river_idx]
  SA_sub <- SA[river_idx]
  RL_sub <- RL[river_idx]
  
  list(
    years = nrow(IP0_sub)-1,
    rivers = ncol(IP0_sub),
    river_idx = river_idx,
    
    EA = EA_sub,
    SA = SA_sub,
    RL = RL_sub,
    
    idx_Morrum_low = match("Mörrumsån_low", selected_rivers),
    idx_Morrum = match("Mörrumsån", selected_rivers),
    idx_Testeboan = match("Testeboån", selected_rivers),
    
    IS  = as.matrix(IS_sub),
    CIS = as.matrix(CIS_sub),
    
    IOP1 = as.matrix(IP1_sub),
    IP0  = as.matrix(IP0_sub),
    n    = as.matrix(n_sub),
    
    size = as.matrix(size_sub),
    size_dev = as.matrix(size_dev_sub),
    size_dev_river = as.matrix(size_dev_river_sub),
    
    parr0 = as.matrix(parr0_sub),
    parr0_dev_river = as.matrix(parr0_dev_river_sub),
    
    age_count = age_count_sub,
    n_aged    = n_aged_sub,
    age_year  = age_year_sub,
    age_river = age_river_sub,
    n_age_obs = n_age_obs,
    
    turbine_prob=turbine_prob,
    turbine_mort=turbine_mort
  )
}

get_sigma<-function(x1,x2,p1,p2){       
  sigma<-(log(x2)-log(x1))/(qnorm(p2)-qnorm(p1))   
  return(sigma)
}

get_tau<-function(x1,x2,p1,p2){       
  sigma<-(log(x2)-log(x1))/(qnorm(p2)-qnorm(p1))
  tau<-1/(sigma^2)   
  return(tau)
}

inits <- function() {
  list(
    aalpha = 0,
    balpha = 0,
    abetas = 1.3,
    bbetas = -1.5,
    
    alpha_parr = rep(0, nrivers),
    beta_parr  = rep(0, nrivers),   # if beta_parr[r]
    logit_p2prop     = rep(0.5, nrivers),
    #beta_parr  = rnorm(1, 0, 0.1),
    #beta_parr = rnorm(nrivers, 0, 0.1),
    #p12prop    = rep(0.5, nrivers),
    
    c2beta = 0.1,
    mugammas = 0.1,
    mugammap = 1,
    mugammap2 = 1,
    mubetap = 1,
    
    P0 = matrix(50, nrow = years, ncol = nrivers)
  )
}