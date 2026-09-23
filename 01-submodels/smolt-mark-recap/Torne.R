
library(coda)
library(nimble)
library(parallel)
library(readxl)
library(tidyverse)
library(lubridate)

df_catch<-read_xls("../../01-Projects/WGBAST/smolt-mark-recapture/Torne/Smolttisaalis_2025_AR.xls", 
              sheet="saalis", col_names=T, range="A4:I40")

df_recaps<-read_xlsx("../../01-Projects/WGBAST/smolt-mark-recapture/Torne/Merkinnät_2025.xlsx", 
                    sheet="Yksilödata lohi", col_names=T, guess_max = 5000 )|> 
  rename(rel_date=`vapautus "päivä"`, recap_date=`Takaisin-saanti-"päivä"`)

df<-df_recaps |> 
  #mutate(rel_d=format(as.POSIXct(rel_date,format = '%m/%d/%Y %H:%M:%S'),format = '%m/%d/%Y'),
  #       recap_d=format(as.POSIXct(recap_date, format = '%m/%d/%Y %H:%M:%S'),format = '%m/%d/%Y')) |> 
  select(Nro,rel_date, recap_date) 
df

df1<-df_recaps |> group_by(rel_date, recap_date) |> 
  summarise(n=n()) |> ungroup() |> 
  arrange(recap_date) |> 
  filter(is.na(rel_date)==F)|> 
  ungroup() 


df<-df_recaps |> mutate(rel_yday=yday(rel_date),
                    recap_yday=yday(recap_date)) |> 
  group_by(rel_yday, recap_yday) |> 
  summarise(n=n()) |> ungroup() |> 
  arrange(rel_yday,recap_yday) |> 
  filter(is.na(rel_yday)==F)|> 
  filter(is.na(recap_yday)==F)

# |> 
#   complete(recap_yday=seq(min(recap_yday, na.rm = T), max(recap_yday, na.rm = T), by = 1)) |> 
#   complete(rel_yday=seq(min(rel_yday, na.rm = T), max(rel_yday, na.rm = T), by = 1)) |> 
#   select(rel_yday, recap_yday, n) 
  # pivot_wider(names_from = recap_yday, values_from = n) |> 
  # complete(rel_yday)

  
  
#rel_empty<-seq(min(df$rel_yday, na.rm = T), max(df$rel_yday, na.rm = T), by = 1) 
#recap_empty<-seq(min(df$rel_yday, na.rm = T), max(df$recap_yday, na.rm = T), by =1) 
min_d<-min(df$rel_yday, na.rm = T)
max_d<-max(df$recap_yday, na.rm = T)
c_empty<-seq(min_d, max_d, by =1) 

df_empty<-array(NA, dim=c(length(c_empty),length(c_empty)))
for(i in 1:dim(df)[1]){
  tmp_reld<-df$rel_yday[i]-min_d+1
  tmp_recd<-df$recap_yday[i]-min_d+1
  df_empty[tmp_reld,tmp_recd]<-df$n[i]
}
df_empty
    
# 
    # for(j in 1:dim(df)[1]){
    # if(df$rel_yday[i]==min_d+i-1 &
    #  df$recap_yday[j]==min_d+j-1){
    #   df_empty[i,j]<-df$n[i]
    # }}}
  








df|> mutate(rel_d=as_date(rel_d)) |> 
  complete(rel_date = seq(min(rel_d, na.rm = T), max(rel_d, na.rm = T), by = 1)) |> 
  complete(recap_date = seq(min(recap_d, na.rm = T), max(recap_d, na.rm = T), by = 1))

df2<-df |> group_by(rel_d, recap_d) |> 
  summarise(n=n()) |> ungroup() |> 
  arrange(recap_d) |> 
filter(is.na(rel_d)==F)|> 
  mutate(rel_d=as_date(rel_d))

df2
df2df2|> 
  mutate(rel_d=as.Date(rel_d))
  
View(df)

df3<-df2 |> pivot_wider(names_from = recap_d, values_from = n)
View(df3)  
df4
# complete()

df2 |> complete(rel_date)




