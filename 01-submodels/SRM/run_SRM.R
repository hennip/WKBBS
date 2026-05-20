library(coda)
library(dplyr)
library(tidyr)
library(stringr)
require(runjags)

assessment_year<-2025 # year up to 2024
years<-length(seq(1985:(assessment_year-1))) 
yearlist<-1985:(assessment_year-1)
q1<-0.05
q2<-0.95

source("01-submodels/SRM/functions_SRM.R")
source("01-submodels/SRM/data_SRM.R")
source("01-submodels/SRM/setup_SRM.R")

selected_data <- build_jags_data(c(river_sel)) # modify in setup_SRM or use river names here
river_names[selected_data[["river_idx"]]]

# Check data ####
nrivers <- selected_data[["rivers"]]
stopifnot(all(selected_data$age_count >= 0))
stopifnot(all(rowSums(selected_data$age_count) == selected_data$n_aged))
stopifnot(all(selected_data$age_year >= 1 & selected_data$age_year <= nrow(IP0)))
stopifnot(all(selected_data$age_year >= 1 & selected_data$age_river <= ncol(IP0)))
n_age_obs <- nrow(selected_data$age_count)

identical(colnames(IS), colnames(IP0))
identical(rownames(IS), rownames(IP0))
stopifnot(identical(dim(IP0), dim(IP1)))
stopifnot(identical(dim(IP0), dim(n)))

stopifnot(all(age_count_full >= 0))
stopifnot(all(rowSums(age_count_full) == n_aged_full))
stopifnot(all(!is.na(age_year_full)))
stopifnot(all(!is.na(age_river_full)))


# for tests
n.chains <- 2
adapt   <- 5000
burnin  <- 40000
samples <- 5000
thin    <- 10

# for longer runs
# n.chains <- 4
# adapt <- 10000 # number samples for learning
# burnin <- 40000 # number discarded samples
# samples <- 40000 # number stored samples
# thin <- 5

# select model to run
selected_model <- "01-submodels/SRM/model/model_SRM.R" 
selected_model <- "01-submodels/SRM/model/model_SRM_parrsize.R" 
selected_model <- "01-submodels/SRM/model/model_SRM_logit_betas.R" 
selected_model <- "01-submodels/SRM/model/model_SRM_smoltage.R" 

jm <- jags.model(
  file = selected_model,
  data = selected_data,
  n.chains = 1,
  n.adapt = 0
)

# Run model ####
jm <- jags.model(
  file=selected_model,
  data=selected_data,
  n.chains=n.chains,
  inits=inits
)

update(jm, adapt + burnin) 

chains <- coda.samples(jm, 
                       mon,
                       n.iter=samples,
                       thin=thin)

save(chains,file = "output/benchmark/SRM_benchmark.Rdata") 