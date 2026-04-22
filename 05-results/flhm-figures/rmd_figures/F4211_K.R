
nstocks<-17

#source("03-histmodel/figures/modded for rmd/make_JAGS_data_2023.R")
proj_years<-0
AUS<-4


tend<-years
yend<-1987+years-1

e_delay<-c(rep(4,times=13),3,3,4,3)
stocks1<-which(e_delay==4)
stocks2<-which(e_delay==3)

######################################################################################################

#load 2019 R0 prior (JAGS) or take from prior parameters text file...
#load("Assessment results/wgbast_2019_prior_2019-04-02.Rdata")  
#source("03-histmodel/figures/modded for rmd/run_FLHM_priors_2023.R")
v<-as.matrix(chainsP)




# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


par(mfrow=c(2,3))
par(mar=c(3,4,4,2))
d1 <- density(na.omit(d[,"K[1]"]),n=100000)
plot(d1, xlim=c(0,max(d1$x)*1.1), lwd=2, ylim=c(0,max(d1$y)*1.1), main="Tornionjoki, Unit 1", xlab="")
lines(density(exp(rnorm(10000,M_K[1],sqrt(1/tau_K[1]))),bw=100),lwd=2,col="grey")
abline(v=exp(M_K[1]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[1]"])),lty=2,lwd=2)

d2 <- density(na.omit(d[,"K[2]"]),n=100000)
plot(d2, xlim=c(0,max(d2$x)*1.1), lwd=2, ylim=c(0,max(d2$y)*1.1), main="Simojoki, Unit 1")
lines(density(exp(rnorm(10000,M_K[2],sqrt(1/tau_K[2]))),bw=7),lwd=2,col="grey")
abline(v=exp(M_K[2]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[2]"])),lty=2,lwd=2)

d3 <- density(na.omit(d[,"K[3]"]),n=100000)
plot(d3, xlim=c(0,max(d3$x)*1.1), lwd=2, ylim=c(0,max(d3$y)*1.1), main="Kalixälven, Unit 1")
lines(density(exp(rnorm(10000,M_K[3],sqrt(1/tau_K[3]))),bw=20),lwd=2,col="grey")
abline(v=exp(M_K[3]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[3]"])),lty=2,lwd=2)

d4 <- density(na.omit(d[,"K[4]"]),n=100000)
plot(d4,  xlim=c(0,max(d4$x)*1.1), lwd=2, ylim=c(0,max(d4$y)*1.1), main="Råneälven, Unit 1")
lines(density(exp(rnorm(10000,M_K[4],sqrt(1/tau_K[4]))),bw=10),lwd=2,col="grey")
abline(v=exp(M_K[4]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[4]"])),lty=2,lwd=2)

d5 <- density(na.omit(d[,"K[5]"]),n=100000)
plot(d5, xlim=c(0,max(d5$x)*1.1), lwd=2, ylim=c(0,max(d5$y)*1.1), main="Piteälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[5],sqrt(1/tau_K[2]))),bw=3),lwd=2,col="grey")
abline(v=exp(M_K[5]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[5]"])),lty=2,lwd=2)

d6 <- density(na.omit(d[,"K[6]"]),n=100000)
plot(d6,xlim=c(0,max(d6$x)*1.1), lwd=2, ylim=c(0,max(d6$y)*1.1), main="Åbyälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[6],sqrt(1/tau_K[6]))),bw=2),lwd=2,col="grey")
abline(v=exp(M_K[6]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[6]"])),lty=2,lwd=2)

p1 <- recordPlot()
p1

#dev.print()
#plot(p1)

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#windows()
par(mfrow=c(2,3))
par(mar=c(3,4,4,2))

d7 <- density(na.omit(d[,"K[7]"]),n=100000)
plot(d7,xlim=c(0,max(d7$x)*1.1), lwd=2, ylim=c(0,max(d7$y)*1.1), main="Byskeälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[7],sqrt(1/tau_K[7]))),bw=15),lwd=2,col="grey")
abline(v=exp(M_K[7]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[7]"])),lty=2,lwd=2)

d8 <- density(na.omit(d[,"K[8]"]),n=100000)
plot(d8,xlim=c(0,max(d8$x)*1.1), lwd=2, ylim=c(0,max(d8$y)*1.1), main="Rickleån, Unit 2")
lines(density(exp(rnorm(10000,M_K[8],sqrt(1/tau_K[8]))),bw=2),lwd=2,col="grey")
abline(v=exp(M_K[8]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[8]"])),lty=2,lwd=2)

d9 <- density(na.omit(d[,"K[9]"]),n=100000)
plot(d9, xlim=c(0,max(d9$x)*1.1), lwd=2, ylim=c(0,max(d9$y)*1.1), main="Sävarån, Unit 2")
lines(density(exp(rnorm(10000,M_K[9],sqrt(1/tau_K[9]))),bw=0.6),lwd=2,col="grey")
abline(v=exp(M_K[9]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[9]"])),lty=2,lwd=2)

d10 <- density(na.omit(d[,"K[10]"]),n=100000)
plot(d10,xlim=c(0,max(d10$x)*1.1), lwd=2, ylim=c(0,max(d10$y)*1.1), main="Ume/Vindelälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[10],sqrt(1/tau_K[10]))),bw=30),lwd=2,col="grey")
abline(v=exp(M_K[10]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[10]"])),lty=2,lwd=2)

d11 <- density(na.omit(d[,"K[11]"]),n=100000)
plot(d11, xlim=c(0,max(d11$x)*1.1), lwd=2, ylim=c(0,max(d11$y)*1.1), main="Öreälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[11],sqrt(1/tau_K[11]))),bw=2),lwd=2,col="grey")
abline(v=exp(M_K[11]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[11]"])),lty=2,lwd=2)

d12 <- density(na.omit(d[,"K[12]"]),n=100000)
plot(d12,xlim=c(0,max(d12$x)*1.1), lwd=2, ylim=c(0,max(d12$y)*1.1), main="Lögdeälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[12],sqrt(1/tau_K[12]))),bw=5),lwd=2,col="grey")
abline(v=exp(M_K[12]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[12]"])),lty=2,lwd=2)

p2 <- recordPlot()
p2

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
#windows()
par(mfrow=c(2,3))
par(mar=c(3,4,4,2))

d16 <- density(na.omit(d[,"K[16]"]),n=100000)
plot(d16,xlim=c(0,max(d16$x)*1.1),ylim=c(0,max(d16$y)*1.1),main="Kågeälven, Unit 2", lwd=2)
lines(density(exp(rnorm(10000,M_K[16],sqrt(1/tau_K[16]))),bw=5),lwd=2,col="grey")
abline(v=exp(M_K[16]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[16]"])),lty=2,lwd=2)

d13 <- density(na.omit(d[,"K[13]"]),n=100000)
plot(d13,xlim=c(0,max(d13$x)*1.1), lwd=2, ylim=c(0,max(d13$y)*1.1),   main="Ljungan, Unit 3")
lines(density(exp(rnorm(10000,M_K[13],sqrt(1/tau_K[13]))),bw=0.6),lwd=2,col="grey")
abline(v=exp(M_K[13]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[13]"])),lty=2,lwd=2)

d17 <- density(na.omit(d[,"K[17]"]),n=100000)
plot(d17,xlim=c(0,max(d17$x)*1.1), lwd=2, ylim=c(0,max(d17$y)*1.1),   main="Testeboån, Unit 3")
lines(density(exp(rnorm(10000,M_K[17],sqrt(1/tau_K[17]))),bw=0.6),lwd=2,col="grey")
abline(v=exp(M_K[17]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[17]"])),lty=2,lwd=2)

d15 <- density(na.omit(d[,"K[15]"]),n=100000)
plot(d15,xlim=c(0,max(d15$x)*1.1), lwd=2, ylim=c(0,max(d15$y)*1.1), main="Emån, Unit 4")
lines(density(exp(rnorm(10000,M_K[15],sqrt(1/tau_K[15]))),bw=5),lwd=2,col="grey")
abline(v=exp(M_K[15]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[15]"])),lty=2,lwd=2)

d14 <- density(na.omit(d[,"K[14]"]),n=100000)
plot(d14,xlim=c(0,max(d14$x)*1.1), lwd=2, ylim=c(0,max(d14$y)*1.1), main="Mörrumsån, Unit 4")
lines(density(exp(rnorm(10000,M_K[14],sqrt(1/tau_K[14]))),bw=5),lwd=2,col="grey")
abline(v=exp(M_K[14]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[14]"])),lty=2,lwd=2)

p3 <- recordPlot()
p3

