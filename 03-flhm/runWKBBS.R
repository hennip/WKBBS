
#river migration survival by stock/AU - currently wild stocks only (check) 
#Only Ume/Vindel currently has reduced spawner survival in some years
#Note that if extra mortality is added for wild stocks 1-4 in years with tagging data, 
#TxWs need to move into a loop where s is stock index, not AU - do something about this - use average surv_migr over stocks in each AU?
#In this version river is survrR or survrW and surv are re-numbered

#Now HrW instead of HrR for reared spawners (Torne and Simo) in population dynamics (tagged and untagged)
#Parr added to NrRsp not NrW (wild)

#flexible indexing not complete!!
library(rjags);library(runjags);library(coda);library(tidyverse);library(reshape2);library(abind)
#setwd("C:/WGBAST15/WGBAST_2026")
PathOut_FLHM <- "results/"

proj_years<-0
maxage<-6
rstocks<-2 #Lule, Dal
stock_indices<-c(1:17)
#NB if run with Torne and Simo they should have positions 1 and 2 because of exceptions for these rivers
stocks<-length(stock_indices)
allstocks<-17 #number of stocks in data files
AUS<-4
assessment_year<-2025

years<-length(seq(1987:assessment_year))

modelName <- "WKBBS_model5"
runName<-modelName
print(runName)
print(paste0(runName,"_data", assessment_year))

# loading data and inits and parnames and model
#datalist <- readRDS("data/dataJags2025_olmos.rds")
load("02-data/flhm-input-files/premade-inputs/data_JAGS_new_spawner_data.RData")    #datalist

initsall <- readRDS("02-data/flhm-input-files/premade-inputs/initsOlmosJags2025_V2.rds")

initsall[[1]]$epsilon_mps <- array(rnorm(years*(stocks+1),0,0.5),dim=c(years,(stocks+1))) 
initsall[[2]]$epsilon_mps <- array(rnorm(years*(stocks+1),0,0.5),dim=c(years,(stocks+1))) 


# initsall[[1]]$alpha_mps <- array(rnorm(years*(AUS+1),0,0.5),dim=c((AUS+1),years)) 
# initsall[[2]]$alpha_mps <- array(rnorm(years*(AUS+1),0,0.5),dim=c((AUS+1),years)) 

#initsall[[1]]$mu_spawn<-cbind(initsall[[1]]$mu_spawn,rbeta(allstocks,mu_sp_alpha[,2],mu_sp_beta[,2]))
#initsall[[2]]$mu_spawn<-cbind(initsall[[2]]$mu_spawn,rbeta(allstocks,mu_sp_alpha[,2],mu_sp_beta[,2]))

#initsall[[1]]$CV_spawn<-cbind(initsall[[1]]$CV_spawn,rbeta(allstocks,CV_sp_alpha[,2],CV_sp_beta[,2]))
#initsall[[2]]$CV_spawn<-cbind(initsall[[2]]$CV_spawn,rbeta(allstocks,CV_sp_alpha[,2],CV_sp_beta[,2]))

initsall[[1]]$mu_spawn<-NULL
initsall[[2]]$mu_spawn<-NULL

initsall[[1]]$CV_spawn<-NULL
initsall[[2]]$CV_spawn<-NULL

parnames <- readRDS("02-data/flhm-input-files/premade-inputs/parnames.rds")
source("03-flhm/model5.R")



## Burn-in
k <- 0
t01<-Sys.time();print(t01)
run0 <- run.jags(WGBAST_model, monitor= parnames,
                 data=datalist,inits = initsall,
                 n.chains = 2, method = 'parallel', thin=1,
                 burnin =10000, modules = "mix",
                 sample =10, adapt = 10000,
                 keep.jags.files=F,
                 progress.bar=TRUE, jags.refresh=100)
t02<-Sys.time();print(t02)
print("run0 done");print(difftime(t02,t01))
print("--------------------------------------------------")

t1<-Sys.time();print(t1)
run1 <- extend.jags(run0, combine=F, sample=500, thin=100, keep.jags.files=F,
                    method = 'parallel')
t2<-Sys.time();print(t2)
print("run1 done"); print(difftime(t2,t1))
print("--------------------------------------------------")
run<-run1
k = k+1
save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,"_",k, ".RData"))

t3<-Sys.time();print(t3)
run2 <- extend.jags(run1, combine=T, sample=500, thin=100, keep.jags.files=F,
                    method = 'parallel')
t4<-Sys.time();print(t4)
print("run2 done");print(difftime(t4,t3))
print("--------------------------------------------------")
run<-run2
k = k+1
save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,"_",k, ".RData"))

t5<-Sys.time();print(t5)
run3 <- extend.jags(run2, combine=T, sample=1000, thin=100, keep.jags.files=F,
                    method = 'parallel')
t6<-Sys.time();print(t6)
print("run3 done");print(difftime(t6,t5))
print("--------------------------------------------------")
run<-run3
k = k+1
save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,"_",k, ".RData"))

t7<-Sys.time();print(t7)
run4 <- extend.jags(run3, combine=T, sample=1000, thin=100, keep.jags.files=F,
                    method = 'parallel',)
t8<-Sys.time();print(t8)
print("run4 done");print(difftime(t8,t7))
print("--------------------------------------------------")

run<-run4
k = k+1
save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,"_",k, ".RData"))

t9<-Sys.time();print(t9)
run5 <- extend.jags(run4, combine=T, sample=1000, thin=100, keep.jags.files=F,method = 'parallel',)
t10<-Sys.time();print(t10)
print("run5 done");print(difftime(t9,t10))
print("--------------------------------------------------")
run<-run5
k = k+1
save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,"_",k, ".RData"))

t11<-Sys.time();print(t11)
run6 <- extend.jags(run5, combine=T, sample=1000, thin=100, keep.jags.files=F,method = 'parallel')
t12<-Sys.time();print(t12)
print("run6 done");print(difftime(t11,t12))
print("--------------------------------------------------")
run<-run6
k = k+1
save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,"_",k, ".RData"))




