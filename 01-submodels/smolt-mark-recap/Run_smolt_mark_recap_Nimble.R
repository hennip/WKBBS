
library(coda)
library(nimble)
library(parallel)
source("00-basics/plotfunctions.r")

# this_cluster määrittää montako ydintä varataan ajoa varten
this_cluster <- makeCluster(4,outfile="")
modelfile<-"BB_final.r"
load("02-data/input-smolt-mark-recap/RData_dump_K_2019.RData")
#View(rdata)

catch<-rdata$c # c: The number of unmarked smolts captured in day j
catch[catch== -9]<-NA

m<-rdata$m # the number of smolts with group-specific marks released in day i
m[m== -9]<-0

r<-rdata$r # the number of smolts with group-specific marks released in day i and recaptured in day j
r[r== -9]<-0 # Replaces NA's with zeros (maybe?)

which(is.na(catch))
r[,which(is.na(catch))]<-NA
N<-length(m)

wt<-rdata$wt # water temperaryr in day j
wl<-rdata$wl # water level or discharge in day j

m[is.na(m)]<-0
rind<-which(m!=0)  #indices with non 0 releases of tagged fish 
rind<-rind[!(rind %in% N)] 

make.inits <- function(){list(sigma_obs = runif(1,1,100),
               nu0 = rnorm(1,0,0.20),
               nu1 = rnorm(1,0,0.20),
               nu2 = rnorm(1,0,0.20),
               omega0 = rnorm(1,0,1),
               omega1 = rnorm(1,0,1),
               omega2 = rnorm(1,0,1),
               psi1 = rnorm(1,0,0.1),
               psi2 = rnorm(1,0,0.1),
               psi0 = rnorm(1,0,0.1),
               pi = rlnorm(1,-0.8,0.20),
               xi = rlnorm(1,-0.69,0.20),
               rho = rlnorm(1,-0.69,0.20),
               logU = runif(1,5.5,14),
               eta=runif(N,-3,3),
               llambda = runif(N,-10,1),
               lphi = runif(N,-10,1),   
               mu_ag=rlnorm(1,log(N/2),0.1),
               lsigma_ag=rlnorm(1,log(0.2),0.50))}  
               


Mconsts<-list(N=N, rind=rind,nrobs=length(rind),mu_mu_ag=log(N/2))
Mdata<-list(m=m ,  swt=(wt[1:N]-mean(wt))/sd(wt), swl=(wl[1:N]-mean(wl))/sd(wl),Ncatch=catch,r=r)  
              
parnames<-c("P",
            "omega0","omega1","omega2",
            "pi", # the standard deviation of random means of log(traveling time) of smolt groups
            "psi0","psi1","psi2",
            "qmu",
            "rho", # the standard deviation of random standard deviations
            "theta",
            "xi",
            "tau",
            "eta",
            "phi1",
            "lambda", # the random effect mean of log(traveling time) of a smolt group released in day i
            "lsigma",
            "nu0","nu1","nu2",
            "qP",
            "cx", # recaptures?
            "CU",
            "ag",
            "rx",
            "sigma_obs") #"mu.c","tau.c",


#single chain (debugging)
#MRModel <- nimbleModel(code = smoltCode,data = Mdata,constants = Mconsts, inits = make.inits(),calculate=FALSE)  #,buildDerivs = TRUE) 
#MRModel$simulate()
#MRModel$calculate()
#
#
#  stnodes <- MRModel$getNodeNames(stochOnly = TRUE, includeData = TRUE)
#  allvars<-MRModel$getVarNames(nodes = stnodes)
#  mvars<-allvars[!(grepl("lifted",allvars))]  
#  
#  #set.seed(794648) 
#  MRModel$simulate(MRModel$getDependencies(stnodes))
#  MRModel$calculate()
#  
##  for(i in 1:length(mvars)){
##  print(paste0(mvars[i]," ",MRModel$calculate(mvars[i]) ))
##  }
##  
#
###nimbleOptions(MCMCenableWAIC = TRUE)
#MRConf <- configureMCMC(MRModel, print=TRUE, useConjugacy = FALSE, monitors = parnames)   #useConjugacy = FALSE
#CMR <- compileNimble(MRModel)   #,dbetabin,rbetabin
#mMCMC <- buildMCMC(MRConf) # uncompiled R code
#CMRMCMC <- compileNimble(mMCMC, project = MRModel)  
#
#results <- runMCMC(CMRMCMC, niter = 200000, thin=10, nburnin=100000)  

##parallel chains
# Mallimääritys tehdään funktioksi jotta voidaan ajaa rinnakkaisilla 
# ketjuilla
run_SmoltCode <- function(seed,smodel,sdata,sconsts,sinits,smonitor) {
library(nimble) 
  smodel<-paste0("01-submodels/smolt-mark-recap/",modelfile)
source(smodel)
  
#seed<-
sdata<-Mdata
sconsts<-Mconsts
sinits<-make.inits()
smonitor<-parnames

# Mallin määrittely
MRModel<- nimbleModel(code = smoltCode, constants = sconsts,  
inits=sinits,data=sdata,calculate=FALSE)

# Ovatko pakollisia? Ehkä testejä joilla voidaan katsoa miten toimii
# Eivät kai tule mitenkään näkyviin täältä funktion sisältä
MRModel$simulate()
MRModel$calculate()

##TRY WITHOUT USE CONJUGACY = FALSE
nimbleOptions(MCMCenableWAIC = TRUE) # laittaa informaatiokriteerin päälle
# Konfiguroidaan mallia ajoa varten
MRConf <- configureMCMC(MRModel, print=TRUE, useConjugacy = FALSE, monitors = smonitor, multivariateNodesAsScalars = TRUE)   #useConjugacy = FALSE
# Jos katsoo MRConf saa näkyviin käytettävät samplerit
MRConf$

# Käännetään C-koodiksi
mMCMC <- buildMCMC(MRConf) # uncompiled R code
# Käytetäänkö tätä mihinkään? 
CMR <- compileNimble(MRModel,dbetabin,rbetabin)  
# Tämän perusteella tehdään MCMC
CMRMCMC <- compileNimble(mMCMC, project = MRModel)  


results <- runMCMC(CMRMCMC, niter =  400000, nburnin = 200000, thin=200, setSeed = seed,WAIC=TRUE)      #1000 per chain
return(results)  

}

# parLapply toteuttaa ajon annetun speksien mukaan, X:n kokoa voi säätää mutta
# ydinten määrä oltava riittävä this_cluster:ssa
chain_output <- parLapply(cl = this_cluster, X = 1:2, 
                          fun = run_SmoltCode,smodel=modelfile,
                          sdata = Mdata,sconsts=Mconsts,sinits=make.inits(),smonitor=parnames)
# Lopuksi vapautetaan ytimien varaus
stopCluster(this_cluster)

proc.time()-ptm

v1 <- mcmc(chain_output[[1]]$samples)
v2 <- mcmc(chain_output[[2]]$samples)
chains<-mcmc.list(list(v1,v2)) 
d<-as.matrix(chains)


#chains<-mcmc(results)
#d<-as.matrix(chains)

dev.new()
par(mfrow=c(4,2),mar=c(3,4,0.1,0.1),oma=c(2,2,0.1,0.1),font=2,font.lab=2,font.axis=2,cex.lab=1,cex.axis=1) 
traceplot(chains[,"CU"])
traceplot(chains[,"qmu[36]"])
traceplot(chains[,"nu0"])
traceplot(chains[,"nu1"])
traceplot(chains[,"nu2"])
traceplot(chains[,"rho"])
traceplot(chains[,"xi"])
traceplot(chains[,"sigma_obs"])



dev.new()
par(mfrow=c(1,1),mar=c(3,4,0.1,0.1),oma=c(2,2,0.1,0.1),font=2,font.lab=2,font.axis=2,cex.lab=1,cex.axis=1) 
plot(density(d[,"CU"]),main="",xlim=c(0,75000),lwd=2)

quantile(d[,"CU"],c(0.025,0.50,0.975))

source("plotfunctions.r")

dev.new()        
par(mfrow=c(1,1),mar=c(3,4,0.1,0.1),oma=c(2,2,0.1,0.1),font=2,font.lab=2,font.axis=2,cex.lab=1,cex.axis=1) 
bx2g_ylim(d,1,N,1,"cx[","]",1,N,0,50,0.25,ylab="Catch")   
points(1:N,catch,pch=17,col="red")


