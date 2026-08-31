
#  modelfile<-  smodel
  
  
  smodel="BB_final.R"
  source(smodel)
  
  sdata = Mdata
  sconsts=Mconsts
  sinits=make.inits()
  smonitor=parnames
  
  #seed<-
  Mdata<-sdata
  Mconsts<-sconsts
  #sinits<-make.inits()
  #smonitor<-parnames
  
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
  #MRConf$
  
  # Käännetään C-koodiksi
  mMCMC <- buildMCMC(MRConf) # uncompiled R code
  # Käytetäänkö tätä mihinkään? 
  CMR <- compileNimble(MRModel,dbetabin,rbetabin)  
  # Tämän perusteella tehdään MCMC
  CMRMCMC <- compileNimble(mMCMC, project = MRModel)  
  
  
  results <- runMCMC(CMRMCMC, niter =  1000, nburnin = 1000, thin=10, #setSeed = seed,
                     WAIC=TRUE)      #1000 per chain
  
as.mcmc(results)
  
  
# parLapply toteuttaa ajon annetun speksien mukaan, 
# X:n kokoa voi säätää mutta
# ydinten määrä oltava riittävä this_cluster:ssa
chain_output <- 
  
  
  
  #parLapply(cl = this_cluster, X = 1:2, 
                          fun = run_SmoltCode,
                          #smodel=paste0("01-submodels/smolt-mark-recap/",modelfile),
                          smodel="BB_final.R",
                          sdata = Mdata,sconsts=Mconsts,sinits=make.inits(),smonitor=parnames)



