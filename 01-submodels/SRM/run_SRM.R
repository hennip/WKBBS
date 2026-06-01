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
samples <- 100000
thin    <- 10

# for longer runs
# n.chains <- 4
# adapt <- 10000 # number samples for learning
# burnin <- 40000 # number discarded samples
# samples <- 40000 # number stored samples
# thin <- 5

# select model to run
#selected_model <- "01-submodels/SRM/model/model_SRM.R" 
#selected_model <- "01-submodels/SRM/model/model_SRM_parrsize.R" 
#selected_model <- "01-submodels/SRM/model/model_SRM_logit_betas.R" 
#selected_model <- "01-submodels/SRM/model/model_SRM_smoltage.R" 
selected_model <- "01-submodels/SRM/model/model_SRM_threshold.R" 
#
#saveRDS(selected_data, "01-submodels/SRM/data/selected_data_treshold.rds")
#readRDS("01-submodels/SRM/data/selected_data_treshold.rds")

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

# update(jm, adapt + burnin) 
# 
# chains <- coda.samples(jm, 
#                        mon,
#                        n.iter=samples,
#                        thin=thin)
# 
# save(chains,file = "../out/benchmark/SRM_benchmark.Rdata") 
#save(chains,file = "output/benchmark/SRM_benchmark.Rdata")

# Runjags
t01<-Sys.time();print(t01)
run0 <- run.jags(selected_model, monitor= mon,
                 data=selected_data,inits = inits,
                 n.chains = n.chains, method = 'parallel', thin=10,
                 burnin =10000, modules = "mix",
                 sample =10000, adapt = 10000,
                 keep.jags.files=F,
                 progress.bar=TRUE, jags.refresh=100)
t02<-Sys.time();print(t02)
print("run0 done");print(difftime(t02,t01))
print("--------------------------------------------------")
run<-run0
save(run,file = "../out/benchmark/SRM_threshold.Rdata") 

t1<-Sys.time();print(t1)
run1 <- extend.jags(run0, combine=T, sample=10000, thin=10, keep.jags.files=F)
t2<-Sys.time();print(t2)
print("run1 done"); print(difftime(t2,t1))
print("--------------------------------------------------")
run<-run1
save(run,file = "../out/benchmark/SRM_threshold.Rdata") 

t1<-Sys.time();print(t1)
run2 <- extend.jags(run1, combine=T, sample=100000, thin=10, keep.jags.files=F)
t2<-Sys.time();print(t2)
print("run2 done"); print(difftime(t2,t1))
print("--------------------------------------------------")
run<-run2
save(run,file = "../out/benchmark/SRM_threshold.Rdata") 

summary(run, var="size_limit")
library(writexl)
res<-as.data.frame(summary(run, var="S[30,1]"))
res<-as.data.frame(summary(run, var="S"))
write_xlsx(res, "../out/benchmark/S_treshold.xlsx")
