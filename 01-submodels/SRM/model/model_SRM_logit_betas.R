# Becky version with parr size and logit betas (17 April 2026) + KM minor adjustments
#expect something like e.g. (1-p[3,r]) for prop of 0+ that does not smoltify (in prediction of 1+ abundance)

model{


		for(r in 1:rivers){
      for(y in 5:(years+1)){
		#smolt abundance
		S[y,r]<-exp(LS[y,r])
		LS[y,r]~dnorm(MS[y,r],TS[y,r])
		MS[y,r]<-log(ES[y,r])-0.5/TS[y,r]
		TS[y,r]<-1/log(mugammas/ES[y,r]+1)
   
 		
     ES[y,r]<-betas[r]*(p[1,r]*P2[y-1,r]+p[2,r]*P1[y-1,r]+p[3,r]*P0[y-1,r])*A[r]/1000    #P1, P2 are densities
     EP[y,r]<-(p[1,r]*P2[y-1,r]+p[2,r]*P1[y-1,r]+p[3,r]*P0[y-1,r])*A[r]/1000 #parr thousands
     
     }
     for(y in 5:years){
 			
     #Density priors 
  	 #2+ parr 
    P2[y,r]~dlnorm(MP2[y,r],TP2[y,r])
    MP2[y,r]<-log(EP2[y,r])-0.5/TP2[y,r]
   	EP2[y,r]<-betap[r]*(q[1,r]*P1[y-1,r]+q[2,r]*P1[y-2,r]+q[3,r]*P1[y-3,r]) #2+ parr belong to age groups 2+ and 3+
  	TP2[y,r]<-1/log(mugammap2/EP2[y,r]+1)

		 #1+ parr 
    P1[y,r]~dlnorm(MP1[y,r],TP1[y,r])
    MP1[y,r]<-log(EP1[y,r])-0.5/TP1[y,r]         #mu
    TP1[y,r]<-1/log(mugammap/EP1[y,r]+1)         #tau
		EP1[y,r]<-P0[y-1,r]*alpha[r]                 #mean comes from 0 year olds in previous year

		 #>1+ parr 
		OP1[y,r]<-P1[y,r]+P2[y,r]             #this goes to measurement eqn

		 #0+ parr 

		P0[y,r]~dlnorm(MP0[y,r],TP0[y,r])       #this goes to measurement eqn
		#MP0[y,r]<-log(10)-0.5/TP0[y,r]
    MP0[y,r]<-log(mean_dens[r])-0.5/TP0[y,r]  # Hierarchical MP0  
    TP0[y,r]<-1/log(100/10+1)


		}
	}
  	for(y in 1:years){
		for(r in 1:rivers){

		#smolt measurements
		IS[y,r]~dnorm(LS[y,r],Tau[y,r])                 #NB this is log(1000s of smolts)!!
 	  Tau[y,r]<-1/log(pow(CIS[y,r],2)+1)							#Distribution of smolt measurement
      
    #0+ parr measurement
		IP0[y,r]~dpois(lambdaIP0[y,r])                 #obs model 0+ parr 2103 checked lambda - similar to obs parr
		lambdaIP0[y,r]~dgamma(aip0[y,r],bip0[y,r])      #lambda, mean parr at all sampling sites (density*nsites*5)
		aip0[y,r]<-n[y,r]/pow(CV[r],2)
 	  bip0[y,r]<-1/(5*P0[y,r]*pow(CV[r],2))

  	 #>1+ parr measurement
		IOP1[y,r]~dpois(lambdaIOP1[y,r])                    #obs model 1+ parr
		lambdaIOP1[y,r]~dgamma(aiop1[y,r],biop1[y,r])       #dgamma(shape,rate)  mean should be aip/bip, var aip/bip^2
		aiop1[y,r]<-n[y,r]/pow(CV[r],2)                     #if x~gamma and y~poisson(x), y is NegBin distributed
 	  biop1[y,r]<-1/(5*OP1[y,r]*pow(CV[r],2))             #5 is the average surface area of a sampling site
                                                       
		 
  	}
 	}

# Priors for the first 4 years

for(y in 1:4){
for( r in 1:rivers){
  OP1[y,r]<-P1[y,r]+P2[y,r]

 	P0[y,r]~dlnorm(MP0[y,r],TP0[y,r])
  MP0[y,r]<-log(mean_dens[r])-0.5/TP0[y,r]  # Hierarchical MP0  
	TP0[y,r]<-1/log(100/10+1)

	P1[y,r]~dlnorm(MP1[y,r],TP1[y,r])
	MP1[y,r]<-log(10)-0.5/TP1[y,r]     
	TP1[y,r]<-1/log(100/10+1)

	P2[y,r]~dlnorm(MP2[y,r],TP2[y,r])
	MP2[y,r]<-log(10)-0.5/TP2[y,r]     
	TP2[y,r]<-1/log(100/10+1)

  S[y,r]<-exp(LS[y,r])
	LS[y,r]~dnorm(MS[y,r],TS[y,r])
	MS[y,r]<-log(10)-0.5/TS[y,r]   
	TS[y,r]<-1/log(100/10+1)
 
  ES[y,r]<-0
  EP[y,r]<-0
  }
}

for(y in 1:(years+1)){
#S_Morrum[y]<-S[y,1]+S[y,2]
#T_downstream[y]<-S[y,4]*downstream_prod
#T_upstream[y]<-S[y,4]*(1-t_prop[y])+S[y,4]*t_prop[y]*(1-t_mort)

# use index:
S_Morrum[y]<- S[y,idx_Morrum]+S[y,idx_Morrum_low] 
T_downstream[y] <- S[y,idx_Testeboan] * downstream_prod
T_upstream[y] <- S[y,idx_Testeboan] * (1-t_prop[y]) + S[y,idx_Testeboan] * t_prop[y] * (1-t_mort) 

T_total[y]<-T_downstream[y]+T_upstream[y]
}

for(y in 1:15){ # updated feb 2022
t_prop[y]<-0
}
for(y in 16:(years+1)){  
t_prop[y]~dbeta(turbine_prob[(y-15),1],turbine_prob[(y-15),2])
}

downstream_prod~dlnorm(-3,100)
t_mort~dbeta(turbine_mort[1],turbine_mort[2])

# Priors for river specific parameters


for( r in 1:rivers){

mean_dens[r]~dlnorm(mu_dens[r],2)
mu_dens[r]~dnorm(3.66,50)  #log(50)-0.5/2  #mu (log of median)

# Production area
A[r]<-exp(AL[r])
AL[r]~dnorm(EA[r]-0.5/Atau[r],Atau[r])
Atau[r]<-1/log(SA[r]^2+1) 

# alpha: survival from 0+ to 1+

alpha[r]~dlnorm(Malpha[r],Talpha)
Malpha[r]<-aalpha+balpha*(AL[r]-mean(AL[]))/sd(AL[])-0.5/Talpha


# betas: parr to smolt survival  
#betas[r]~dlnorm(Mbetas[r],Tbeta)
#Mbetas[r]<-abetas+bbetas*(AL[r]-mean(AL[]))/sd(AL[])+cbetas*(RL[r]-mean(RL[]))/sd(RL[])-0.5/Tbeta

# betas: parr to smolt survival  
lb[r]~dnorm(Mbetas[r],Tbeta)
logit(betas[r])<-lb[r]
##Mbetas[r]<-abetas+bbetas*(A[r]-mean(A[]))/sd(A[])+cbetas*(R[r]-mean(R[]))/sd(R[])
Mbetas[r]<-abetas+bbetas*(A[r]-mean(A[]))/sd(A[])+cbetas*(RL[r]-mean(RL[]))/sd(RL[]) # KM: R -> RL (not log)
################################################################################

# betap: survival of >1+ parr
betap[r]~dlnorm(Mbetap,Tbetap)

CV[r]~dlnorm(Mcv,Tcv)

# version with parr size by year
# Smolt origin probabilities (year-specific)
# Annual within-river deviation in 0+ size affects probability
# that smolts originate from 0+ parr (early smoltification)

#logit(p2prop[r]) ~ dnorm(mean_p2prop,tau_p2prop) # KM replaced (function on left side not allowed)
#  logit_p2prop[r] ~ dnorm(mu_p2prop, tau_p2prop) # KM new, mu instead of mean
#  logit(p2prop[r]) <- logit_p2prop[r] # KM new
  
#alpha_parr[r] ~ dnorm(mu_alpha_parr, tau_alpha_parr)
#beta_parr[r] ~ dnorm(mu_beta_parr, tau_beta_parr)

#for(y in 1:years){ 

    # Probability smolt comes from 0+ parr (early smolt)
#    logit(p[3,y,r]) <- alpha_parr[r] + beta_parr[r] * size_dev[y,r]   

    # Remaining fish split between 1+ and 2+ parr
#    p[2,y,r] <- (1 - p[3,y,r]) * p2prop[r]
#    p[1,y,r] <- (1 - p[3,y,r]) * (1 - p2prop[r])
#}

for(i in 1:3){ 
qr[i,r]~dgamma(aq[i],1)
q[i,r]<-qr[i,r]/sum(qr[1:3,r]) 
}
for(i in 1:3){
pt[i,r]~dgamma(ap[i],1)
p[i,r]<-pt[i,r]/sum(pt[1:3,r])
}
} #r


# Priors for hyperparameters
#mu_alpha_parr ~ dnorm(0,0.01) # added April 2026
#tau_alpha_parr ~ dgamma(0.1,0.1) # added April 2026
#mu_beta_parr ~ dnorm(0,0.01) # added April 2026
#tau_beta_parr ~ dgamma(0.1,0.1) # added April 2026
#mu_p2prop ~ dnorm(0,0.01) # added April 2026
#tau_p2prop ~ dgamma(0.1,0.1) # added April 2026


for(i in 1:3){
ap[i]~dgamma(5,2)
}

for(i in 1:3){
aq[i]~dgamma(1,3)
}

cvalpha~dlnorm(-1.61,5)
cvbeta~dlnorm(-1.61,5)

Talpha<-1/log(cvalpha*cvalpha+1)
Tbeta<-1/log(cvbeta*cvbeta+1)

Mbetap<-log(mubetap)-0.5/Tbetap
Tbetap<-1/log(c2beta*c2beta+1)
Mcv<-log(muCV)-0.5/Tcv
Tcv<-1/log(CCV*CCV+1)
mubetap~dunif(0,100)

mugammas~dunif(0.01,10)     
mugammap~dunif(0.01,10)
mugammap2~dunif(0.01,10)

muCV~dgamma(1,1)
CCV~dlnorm(0,0.001)T(0.01,10)
c2beta~dunif(0.01,2)

for(y in 5:years){
      TotS[y]<-sum(S[y,])
      #Groups
      for(r in 1:rivers){
            pr[y,r]<-S[y,r]/TotS[y]
      }
}

aalpha~dnorm(0,0.0001)
balpha~dnorm(0,0.0001)

abetas~dnorm(0,1) #error message (TS invalid parents) with tau of 0.01 and smaller for these...
bbetas~dnorm(0,1)
cbetas~dnorm(0,1)T( ,0)
}

     







