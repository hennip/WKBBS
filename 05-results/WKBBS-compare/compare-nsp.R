
# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-nsp
rivernames<-c("Torne","Simo","Kalix","Rane"
              ,"Pite","Aby","Byske","Rickle","Savaran"
              ,"Ume","Ore","Logde","Ljungan","Morrum"
              ,"Eman", "Kage", "Test")



# Model 1: 
# =========  # Number of spawners per river

for(r in 1:nstocks){
  #r<-1
  #df<-boxplot.jags.df2(d, "NspWtot[",str_c(r,"]"),1:length(YearsB)+1,)
  df<-boxplot.jags.df2(chains1, "NspWtot[",str_c(r,"]"),1:length(Years))
  #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.1<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+yr_start)
df.1
#View(df.1)


# Model 2:                                                                       9/7/2022 11:45:23 A
# =========

# Number of spawners per river
for(r in 1:nstocks){
  #r<-1
  df<-boxplot.jags.df2(chains, "NspWtot[",str_c(r,"]"),1:(length(Years)))
  #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.2<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+yr_start)
df.2
#View(df.2)


# Spawner count datasets
# =================

#"../../WGBAST_shared/data/data_2025/spawner_counts.txt"

# counts<-read_tsv(str_c(PathData,"data_2025/spawner_counts.txt"),skip=9,col_names=T, na="NA") %>%
#   as.data.frame()

counts<-read_table(here("../../WGBAST_shared/data/data_2026/spawner_counts_SimoMSW_2026.txt"),skip=10,col_names=F, na="NA") %>%
  as.data.frame()
colnames(counts) = c("Year", rivernames )
counts = counts %>% column_to_rownames("Year")

# colnames(counts)<-rivernames
# rownames(counts) <-  yr_start+1:nrow(counts)
counts_m <- t(counts) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "rivername") %>% 
  mutate(id  = 1:17) 


counts_long <- counts_m %>% 
  melt(id.vars = c("id", "rivername")) %>% 
  select(-rivername) %>% 
  transmute(
    River = id,
    Year = variable %>% as.character() %>% as.numeric(),
    Count = value/1000
  )


counts2<-read_tsv(here("../../WGBAST_shared/data/data_2026/Spawner_counts_notInJAGS_2026.txt"),col_names=T, na="NA") %>% 
  as.data.frame() %>% column_to_rownames("Year")



counts2_m <- t(counts2) %>% 
  as.data.frame() %>% 
  mutate(id = c(4, 11))

counts2_long <- counts2_m %>% 
  melt(id.vars = c("id")) %>% 
  transmute(
    River = id,
    Year = variable %>% as.character() %>% as.numeric(),
    Count2 = value/1000
  )

counts<-full_join(counts_long, counts2_long, by=c("Year", "River"))
#View(counts2)
df.2<-left_join(df.2,counts, by=c("River", "Year"))


df.1<-df.1%>%
  mutate(Rivername=as.factor(River))%>%#, levels=NULL))%>%
  mutate(Rivername=fct_recode(Rivername, "Torne"="1", "Simo"="2", "Kalix"= "3", "Råne"="4",
                              "Pite"="5", "Åby"="6", "Byske"="7", "Rickleån"="8", "Sävärån"="9",
                              "Ume"="10", "Öre"="11", "Lögde"="12", "Ljungan"="13", "Mörrum"="14", "Emån"="15",
                              "Kåge"="16", "Testeboån"="17" ))


df.2<-df.2%>%
  mutate(Rivername=as.factor(River))%>%#, levels=NULL))%>%
  mutate(Rivername=fct_recode(Rivername, "Torne"="1", "Simo"="2", "Kalix"= "3", "Råne"="4",
                              "Pite"="5", "Åby"="6", "Byske"="7", "Rickleån"="8", "Sävärån"="9",
                              "Ume"="10", "Öre"="11", "Lögde"="12", "Ljungan"="13", "Mörrum"="14", "Emån"="15",
                              "Kåge"="16", "Testeboån"="17" ))




df.1<-df.1 %>% mutate(Rname= recode(River, "1"="Tornionjoki (AU 1)",
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

df.2<-df.2 %>% mutate(Rname= recode(River, "1"="Tornionjoki (AU 1)",
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


#Re-order
# ==========================
df.1p<-transform(df.1,Rname1=factor(Rname,levels=c("Tornionjoki (AU 1)","Simojoki (AU 1)","Kalixälven (AU 1)","Råneälven (AU 1)","Piteälven (AU 2)",
                                                   "Åbyälven (AU 2)","Byskeälven (AU 2)","Kågeälven (AU 2)","Rickleån (AU 2)","Sävarån (AU 2)","Ume/Vindelälven (AU 2)",
                                                   "Öreälven (AU 2)","Lögdeälven (AU 2)","Ljungan (AU 3)","Testeboån (AU 3)","Emån (AU 4)","Mörrumsån (AU 4)")))

df.2p<-transform(df.2,Rname1=factor(Rname,levels=c("Tornionjoki (AU 1)","Simojoki (AU 1)","Kalixälven (AU 1)","Råneälven (AU 1)","Piteälven (AU 2)",
                                                   "Åbyälven (AU 2)","Byskeälven (AU 2)","Kågeälven (AU 2)","Rickleån (AU 2)","Sävarån (AU 2)","Ume/Vindelälven (AU 2)",
                                                   "Öreälven (AU 2)","Lögdeälven (AU 2)","Ljungan (AU 3)","Testeboån (AU 3)","Emån (AU 4)","Mörrumsån (AU 4)")))


#View(df.1)

## ---- graphs-nsp


# Draw boxplots to compare
# ==========================


#for(r in 1:17){
#r<-1
#df1<-filter(df.1, River==r, Year>dat_start)
#df2<-filter(df.2, River==r, Year>dat_start)

# df1<-filter(df.1p, Year>1991 & River %in% c(1:9))
# df2<-filter(df.2p, Year>1991 & River %in% c(1:9))
#dev.new()
#df1<-filter(df.1p, Year>1991 & River %in% c(10:17))
#df2<-filter(df.2p, Year>1991 & River %in% c(10:17))
kuv = 1
for(group in list( c(1:9), c(10:17))){
  
  df_p1 <- filter(df.1p, Year>1991 & River %in% group )
  df_p2 <- filter(df.2p, Year>1991 & River %in% group )
  
  #dev.new()
  #tiff(paste("06-misc/olmos_res/spawners_Mps_stock_",kuv, ".tiff", sep =""),  width=2500, height=2000, res=300) 
  print(
  
    ggplot(df_p2, aes(Year, group=Year))+
    theme_bw()+
    geom_boxplot(
      data=df_p1,
      mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",
      colour="grey", fill="grey95")+
    geom_boxplot(
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.6))+
    labs(x="Year", y="Number of spawners (1000s)") + 
    #title=Rivername_long[r])+
    # title=Rivername[r])+
    geom_line(aes(Year,q50))+
    geom_line(data=df_p1,aes(Year,q50),col="grey")+  
    geom_point(data=df_p2, aes(Year, Count),col="red")+
    geom_point(data=df_p2, aes(Year, Count2),col="blue", shape=17)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    facet_wrap(~Rname1, scales="free") 
    
    )
  #dev.off()
  # Facet if you like to have all graphs together, 
  # downside is you cannot easily control ylim and scales are very different
  kuv = kuv+1
}
# 
# tiff("spawners1_Mps_stock.tiff",  width=2500, height=2000, res=300)   
# print(
#   ggplot(df2, aes(Year, group=Year))+
#     theme_bw()+
#     geom_boxplot(
#       data=df1,
#       mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
#       stat = "identity",
#       colour="grey", fill="grey95")+
#     geom_boxplot(
#       aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
#       stat = "identity",fill=rgb(1,1,1,0.6))+
#     labs(x="Year", y="Number of spawners (1000s)") + 
#     #title=Rivername_long[r])+
#     # title=Rivername[r])+
#     geom_line(aes(Year,q50))+
#     geom_line(data=df1,aes(Year,q50),col="grey")+  
#     geom_point(data=df2, aes(Year, Count),col="red")+
#     geom_point(data=df2, aes(Year, Count2),col="blue", shape=17)+
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#     facet_wrap(~Rname1, scales="free") # Facet if you like to have all graphs together, downside is you cannot easily control ylim and scales are very different
#   
# )
# dev.off()


#
#df2<-filter(df.2, Year>1991)
#ggplot(df2, aes(x=Year, y=Count))+
#  theme_bw()+
#  geom_bar( stat = "identity", fill = "skyblue") +
#    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#  facet_wrap(~Rivername, scales="free") 
#
##}  #river

## ---- graphs-nsp-report


# Draw boxplots to compare
# ==========================

#df1<-filter(df.1, Year>1991)
#df2<-filter(df.2, Year>1991)
#
#plots<-list()
#for(r in 1:17){
##dev.new()
#  #r<-1
#  df1<-filter(df.1, River==r, Year>1991)
#  df2<-filter(df.2, River==r, Year>1991)
#  plot<-ggplot(df2, aes(Year, group=Year))+
#          theme_bw()+
#          geom_boxplot(
#            aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
#            stat = "identity",fill=rgb(1,1,1,0.6))+
#          labs(x="Year", y="1000s spawners", title=Rivername[r])+
#          geom_line(aes(Year,q50))+
#          geom_point(data=df2, aes(Year, Count),col="red")+
#          geom_point(data=df2, aes(Year, Count2),col="blue", shape=17)+
#          scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
#  plots[[r]]<-plot
#  print(plots[[r]])
#}

#res <- 6
#name_figure <- "spawners1.png"
#png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)
#
#grid.arrange(plots[[1]],plots[[2]], plots[[3]],plots[[4]], 
#             plots[[5]],plots[[6]], plots[[7]],plots[[8]], 
#             plots[[9]],ncol=3)
#
#dev.off()
#
#res <- 6
#name_figure <- "spawners2.png"
#png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)
#
#grid.arrange(plots[[10]], plots[[11]],plots[[12]], 
#             plots[[13]],plots[[14]], plots[[15]],plots[[16]],ncol=3)
#
#dev.off()
