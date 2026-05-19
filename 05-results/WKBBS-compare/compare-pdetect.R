
# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-pdetect
# Model 1:
# =========

for(r in 1:nstocks){
  #r<-1
  df<-boxplot.jags.df2(chains, "p.detect[",str_c(r,"]"),1:(length(YearsB)+1)) #short runs
  #df<-boxplot.jags.df2(chains1, "SmoltW[",str_c(r,"]"),1:(length(YearsB)+3))
  #df<-boxplot.jags.df2(chains1, "SmoltWW[",str_c(r,"]"),1:(length(Years)+3))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.1<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.1

# Model 2: 
# =========
# summary(chains[ ,regexpr("SmoltW",varnames(chains))>0])

for(r in 1:nstocks){
  #r<-1
  #df<-boxplot.nimble.df2(chains, "SmoltW[",str_c(r,"]"),1:(length(Years)+3))
  df<-boxplot.jags.df2(chains1, "p.detect[",str_c(r,"]"),1:(length(Years)+1)) #short runs
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.2<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.2


## ---- graphs-pdetect


# Draw boxplots to compare
# ==========================

#df1<-df.1
#df2<-df.2



#for(r in 1:17){
#r<-1
#df1<-filter(df.1, River==r, Year>1991)
#df2<-filter(df.2, River==r, Year>1991)
df.1$r2 <- factor(df.1$River, labels = Rivername)
df.2$r2 <- factor(df.2$River, labels = Rivername)

# dev.new()
# print(ggplot(df.2, aes(Year, group=Year))+
#         theme_bw()+
#         geom_boxplot(
#           data=df.1,
#           mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
#           stat = "identity",
#           colour="grey", fill="grey95")+
#         geom_boxplot(
#           aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
#           stat = "identity",fill=rgb(1,1,1,0.1))+
#         labs(x="Year", y="Detection probability")+
#         geom_line(aes(Year,q50))+
#         geom_line(data=df.1,aes(Year,q50),col="grey")+  
#         ylim(0,1) +
#         scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#         #facet_wrap(~River,labeller=river_labeller(~River))
#         #facet_wrap(~River, labeller = as_labeller(setNames(Rivername, River)))
#         facet_wrap(~r2) 
# )


for(group in list( c(1:9), c(10:17))){
  
  df1_p <- filter(df.1, Year>1991 & River %in% group )
  df2_p <- filter(df.2, Year>1991 & River %in% group )
  
  #dev.new()
  #tiff(paste("06-misc/olmos_res/spawners_Mps_stock_",kuv, ".tiff", sep =""),  width=2500, height=2000, res=300) 
  print(
    
    ggplot(df2_p, aes(Year, group=Year))+
      theme_bw()+
      geom_boxplot(
        data=df1_p,
        mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
        stat = "identity",
        colour="grey", fill="grey95")+
      geom_boxplot(
        aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
        stat = "identity",fill=rgb(1,1,1,0.1))+
      labs(x="Year", y="Detection probability")+
      geom_line(aes(Year,q50))+
      geom_line(data=df1_p,aes(Year,q50),col="grey")+  
      ylim(0,1) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      #facet_wrap(~River,labeller=river_labeller(~River))
      #facet_wrap(~River, labeller = as_labeller(setNames(Rivername, River)))
      facet_wrap(~r2) 
    
  )
  #dev.off()
  # Facet if you like to have all graphs together, 
  # downside is you cannot easily control ylim and scales are very different
  #  kuv = kuv+1
}

#}

