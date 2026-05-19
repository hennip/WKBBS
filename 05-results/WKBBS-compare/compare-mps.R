
# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-mps

Mps_stocks = 17
# Model 1: 
# =========
Rivername<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage", "Testeboan")
# survMpsW<-array(NA, dim=c(nsampj,length(Years)+1))
# survMpsR<-array(NA, dim=c(nsampj,length(Years)+1))
# ratio<-array(NA, dim=c(nsampj,length(Years)+1))
# 
# for(y in 1:(length(Years)+1)){
#   survMpsW[,y]<-exp(-as.matrix(chains1[,str_c("MpsW[",y,"]")]))
#   survMpsR[,y]<-exp(-as.matrix(chains1[,str_c("MpsR[",y,"]")]))
#   ratio[,y]<-survMpsR[,y]/survMpsW[,y]
# }

survMpsW <- 
  chains1 %>% 
  as.data.frame() %>% 
  select(starts_with("MpsW[")) %>% 
  apply(2, function(x)exp(-x))

survMpsR <- 
  chains1 %>% 
  as.data.frame() %>% 
  select(starts_with("MpsR[")) %>% 
  apply(2, function(x)exp(-x))

ratio = survMpsW/survMpsR


dfW<-boxplot.bugs.df(survMpsW, 1:(length(Years)+1))  #%>%
#   mutate(Type="Wild")


#dfW<-boxplot.bugs.df(survMpsW, 1:(length(Years)))%>%
#    mutate(Rname=rep(Rivername,each=length(Years)))    
#    
#dfR<-boxplot.bugs.df(survMpsR, 1:(length(Years)))%>%
#    mutate(Type="Reared")
#df_ratio<-boxplot.bugs.df(ratio, 1:(length(Years)))%>%
#    mutate(Type="Ratio")
#  
#df<-full_join(dfW,dfR, by=NULL)
##df<-full_join(df,df_ratio, by=NULL)

df.1<-as_tibble(setNames(dfW,c("Year","q5","q25","q50","q75","q95")))%>%
  mutate(Year=Year+yr_start)
df.1


# Model 2: 
# =========
# summary(chains[ ,regexpr("SmoltW",varnames(chains))>0])
#survMpsW<-array(NA, dim=c(dim(v)[1],length(Years),Mps_stocks))
#for(r in 1:Mps_stocks){
#
#  for(y in 1:(length(Years))){
#    survMpsW[,y,r]<-exp(-as.matrix(chains[,str_c("MpsW[",y,", ",r,"]")]))
#  }
#}

# v1<-exp(-v[,grep("MpsW",colnames(v))])
# 
# for(r in 1:Mps_stocks){
#   df<-boxplot.jags.df2(v1, "MpsW[",str_c(r,"]"),1:(length(Years)+1),sep2) #short runs
#   df<-mutate(df, River=r)
#   ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
# }

mpsOLMOS <- 
  chains %>% 
  as.data.frame() %>% 
  select(starts_with("MpsW[")) %>% 
  apply(2, function(x)exp(-x)) %>% 
  apply(2, quantile, c(0.05, 0.25, 0.5, 0.75, 0.95)) %>% 
  t %>% as.data.frame %>% 
  rownames_to_column("ID") %>%
  mutate(
    YEAR  = str_extract(ID, "(?<=\\[)\\d+(?=,)") %>%  as.integer(),
    RIVER = str_extract(ID, "(?<=,)\\d+(?=\\])") %>%  as.integer()
  ) %>%
  mutate(YEAR = YEAR+1986) %>% 
  select("Year" = YEAR, 
         "q5" = `5%`,
         "q25" = `25%`,
         "q50" = `50%`,
         "q75" = `75%`,
         "q95" = `95%`,
         "River" = RIVER)

  

  
# 
# df.2<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
#   select(River, everything())%>%
#   mutate(Year=Year+1986)

df.2 = mpsOLMOS %>% 
  filter(River!=18)# %>% 
  # filter(River == 16 & Year<=2008) %>%
  # filter(River == 17 & Year<=2000)



# df.2[df.2$River==16 & df.2$Year<=2008,3:7]<-0
# df.2[df.2$River==17 & df.2$Year<=2000,3:7]<-0

df.3<-df.2
df.3$q5<-rep(df.1$q5,times=Mps_stocks)
df.3$q25<-rep(df.1$q25,times=Mps_stocks)
df.3$q50<-rep(df.1$q50,times=Mps_stocks)
df.3$q75<-rep(df.1$q75,times=Mps_stocks)
df.3$q95<-rep(df.1$q95,times=Mps_stocks)

df.2 = df.2 %>%
  mutate(across(
    .cols = -c(River, Year), # Valitaan kaikki sarakkeet paitsi River ja Year
    .fns = ~ if_else(
      (River == 16 & Year <= 2008) | (River == 17 & Year <= 2000), 
      0,   # Jos ehto täyttyy, asetetaan 0
      .x   # Muuten pidetään alkuperäinen arvo
    )
  ))

# df.3 = df.3 %>%
#   mutate(across(
#     .cols = -c(River, Year), # Valitaan kaikki sarakkeet paitsi River ja Year
#     .fns = ~ if_else(
#       (River == 16 & Year <= 2008) | (River == 17 & Year <= 2000), 
#       0,   # Jos ehto täyttyy, asetetaan 0
#       .x   # Muuten pidetään alkuperäinen arvo
#     )
#   ))

## ---- graphs-mps

# df.2 = df.2 %>% 
#   filter(!(River == 16 & Year<=2008)) %>%
#   filter(!(River == 17 & Year<=2000))
# 
# df.3 = df.3 %>% 
#   filter(!(River == 16 & Year<=2008)) %>%
#   filter(!(River == 17 & Year<=2000))

if(Mps_stocks==4){
  df.2<-df.2 %>% mutate(Rname= recode(River, "1"="AU 1",
                                      "2"="AU 2",
                                      "3"="AU 3",
                                      "4"="AU 4"))
  
  df.3<-df.3 %>% mutate(Rname= recode(River, "1"="AU 1",
                                      "2"="AU 2",
                                      "3"="AU 3",
                                      "4"="AU 4"))
  #tiff("Mps_AU.tiff",  width=3000, height=2000, res=300)   
  print(ggplot(df.2, aes(Year, group=Year))+
          theme_bw()+
          geom_boxplot(
            data=df.3,
            mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
            stat = "identity",
            colour="grey", fill="grey95")+
          #      geom_boxplot(
          #            data=df1,
          #            mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
          #            stat = "identity",
          #            colour="grey", fill="grey95")+
          geom_boxplot(
            aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
            stat = "identity",fill=rgb(1,1,1,0.1))+
          labs(x="Year", y="Post-smolt survival", title="")+
          geom_line(aes(Year,q50))+
          #geom_line(data=df1,aes(Year,q50),col="grey")+  
          scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
          facet_wrap(~as.factor(Rname),scales="free")
  )
  #}
#  dev.off()  
}else{
  
  df.2<-df.2 %>% mutate(Rname= recode(River, 
                                      "1"="Tornionjoki (AU 1)",
                                      "2"="Simojoki (AU 1)",
                                      "3"="Kalixälven (AU 1)",
                                      "4"="Råneälven (AU 1)",
                                      "5"="Piteälven (AU 2)",
                                      "6"="Åbyälven (AU 2)",
                                      "7"="Byskeälven (AU 2)",
                                      "8"="Rickleån (AU 2)",
                                      "9"="Sävarån (AU 2)",
                                      "10"="Ume/Vindelälven (AU 2)",
                                      "11"="Öreälven (AU 2)",
                                      "12"="Lögdeälven (AU 2)",
                                      "13"="Ljungan (AU 3)",
                                      "14"="Mörrumsån (AU 4)",
                                      "15"="Emån (AU 4)",
                                      "16"="Kågeälven (AU 2)",
                                      "17"="Testeboån (AU 3)"))
  
  df.3<-df.3 %>% mutate(Rname= recode(River, 
                                      "1"="Tornionjoki (AU 1)",
                                      "2"="Simojoki (AU 1)",
                                      "3"="Kalixälven (AU 1)",
                                      "4"="Råneälven (AU 1)",
                                      "5"="Piteälven (AU 2)",
                                      "6"="Åbyälven (AU 2)",
                                      "7"="Byskeälven (AU 2)",
                                      "8"="Rickleån (AU 2)",
                                      "9"="Sävarån (AU 2)",
                                      "10"="Ume/Vindelälven (AU 2)",
                                      "11"="Öreälven (AU 2)",
                                      "12"="Lögdeälven (AU 2)",
                                      "13"="Ljungan (AU 3)",
                                      "14"="Mörrumsån (AU 4)",
                                      "15"="Emån (AU 4)",
                                      "16"="Kågeälven (AU 2)",
                                      "17"="Testeboån (AU 3)"))
  
  # ==========================
  #Re-order
  # ==========================
  df.2p<-transform(df.2,Rname1=factor(Rname,levels=c("Tornionjoki (AU 1)","Simojoki (AU 1)","Kalixälven (AU 1)","Råneälven (AU 1)","Piteälven (AU 2)",
                                                     "Åbyälven (AU 2)","Byskeälven (AU 2)","Kågeälven (AU 2)","Rickleån (AU 2)","Sävarån (AU 2)","Ume/Vindelälven (AU 2)",
                                                     "Öreälven (AU 2)","Lögdeälven (AU 2)","Ljungan (AU 3)","Testeboån (AU 3)","Emån (AU 4)","Mörrumsån (AU 4)")))
  
  df.3p<-transform(df.3,Rname1=factor(Rname,levels=c("Tornionjoki (AU 1)","Simojoki (AU 1)","Kalixälven (AU 1)","Råneälven (AU 1)","Piteälven (AU 2)",
                                                     "Åbyälven (AU 2)","Byskeälven (AU 2)","Kågeälven (AU 2)","Rickleån (AU 2)","Sävarån (AU 2)","Ume/Vindelälven (AU 2)",
                                                     "Öreälven (AU 2)","Lögdeälven (AU 2)","Ljungan (AU 3)","Testeboån (AU 3)","Emån (AU 4)","Mörrumsån (AU 4)")))
  
 # dev.new()
  
  #tiff("06-misc/olmos_res/Mps_stock.tiff",  width=3000, height=2000, res=300)   
  # print(ggplot(df.2p, aes(Year, group=Year))+
  #         theme_bw()+
  #         geom_boxplot(
  #           data=df.3p,
  #           mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
  #           stat = "identity",
  #           colour="grey", fill="grey95")+
  #         #      geom_boxplot(
  #         #            data=df1,
  #         #            mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
  #         #            stat = "identity",
  #         #            colour="grey", fill="grey95")+
  #         geom_boxplot(
  #           aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
  #           stat = "identity",fill=rgb(1,1,1,0.1))+
  #         labs(x="Year", y="Post-smolt survival", title="")+
  #         geom_line(aes(Year,q50))+
  #         #geom_line(data=df1,aes(Year,q50),col="grey")+  
  #         scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  #         facet_wrap(~as.factor(Rname1),scales="free")
  # )
  
  for(group in list( c(1:9), c(10:17))){
  
  df2_p <- filter(df.2p, Year>1991 & River %in% group )
  df3_p <- filter(df.3p, Year>1991 & River %in% group )
  
  #dev.new()
  #tiff(paste("06-misc/olmos_res/spawners_Mps_stock_",kuv, ".tiff", sep =""),  width=2500, height=2000, res=300) 
  print(
  
    ggplot(df2_p, aes(Year, group=Year))+
              theme_bw()+
              geom_boxplot(
                data=df3_p,
                mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
                stat = "identity",
                colour="grey", fill="grey95")+
              #      geom_boxplot(
              #            data=df1,
              #            mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
              #            stat = "identity",
              #            colour="grey", fill="grey95")+
              geom_boxplot(
                aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
                stat = "identity",fill=rgb(1,1,1,0.1))+
              labs(x="Year", y="Post-smolt survival", title="")+
              geom_line(aes(Year,q50))+
              #geom_line(data=df1,aes(Year,q50),col="grey")+
              scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
              facet_wrap(~as.factor(Rname1),scales="free")
    
    )
  #dev.off()
  # Facet if you like to have all graphs together, 
  # downside is you cannot easily control ylim and scales are very different
#  kuv = kuv+1
}
  
  #dev.off()
  
}
# Draw boxplots to compare







##R?ne with median line from combined stocks
#
#
#df2<-filter(df.2, River==4)
#df2$jags<-apply(exp(-d[,grep("MpsW",colnames(d))]),2,median)[2:40]
#  
#  dev.new()
#  print(ggplot(df2, aes(Year, group=Year))+
#          theme_bw()+
#    #      geom_boxplot(
##            data=df1,
##            mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
##            stat = "identity",
##            colour="grey", fill="grey95")+
#          geom_boxplot(
#            aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
#            stat = "identity",fill=rgb(1,1,1,0.1))+
#            geom_line(data=df2, aes(x=Year,y=jags,group=1),size=1.5,col="blue")+
#          labs(x="Year", y="Post smolt survival", title="")+
#          geom_line(aes(Year,q50,group=1))+
#          #geom_line(data=df1,aes(Year,q50),col="grey")+  
#          scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
#  )
##}


