

#df_catch<-read_xls(str_c(path_in,"Smolttisaalis_2025_AR.xls"), 
                    
df_catch<-read_xls("tmp/Smolttisaalis_2025_AR.xls", 
              sheet="saalis", col_names=T, range="A4:I40") |> 
  rename(w_temp=`veden lämpötila/ water temperature`,
         w_height=`vedenkorkeus/ water level`) |> 
  mutate(date_yday=yday(pvm),catch=lohi) |> 
  select(date_yday, everything()) |> 
  complete(date_yday) # Fills NA if a date is missed

#df_recaps<-read_xlsx(str_c(path_in,"Merkinn?t_2025.xlsx"), 
df_recaps<-read_xlsx("tmp/Merkinnät_2025.xlsx",
                    sheet="Yksilödata lohi", col_names=T, guess_max = 5000 )|> 
  rename(rel_date=`vapautus "päivä"`, recap_date=`Takaisin-saanti-"päivä"`)|> 
  mutate(rel_yday=yday(rel_date),
         recap_yday=yday(recap_date))

# Check that start dates are the same
min(df_catch$date_yday)
min(df_recaps$rel_yday, na.rm=T)

# Covariates
wt<-df_catch$w_temp
wl<-df_catch$w_height

# m: merkittyjen m??r? (marked)
# c: rys?saalis (catch)
df_c<-df_catch |> select(date_yday,catch) |> rename(date=date_yday)

df_m<-df_recaps |> group_by(rel_yday) |> rename(date=rel_yday) |> 
  summarise(m=n())

df_mc<-full_join(df_c, df_m) |> 
  filter(is.na(date)==F)
N<-dim(df_mc)[1]

m<-df_mc |> select(m) |> pull()
m[is.na(m)]<-0
rind<-which(m!=0)  #indices with non 0 releases of tagged fish 
rind<-rind[!(rind %in% N)] 


# Calculate r matrix:
df<-  df_recaps |> 
  group_by(rel_yday, recap_yday) |> 
  summarise(n=n()) |> ungroup() |> 
  arrange(rel_yday,recap_yday) |> 
  filter(is.na(rel_yday)==F)|> 
  filter(is.na(recap_yday)==F)

min_d<-min(df$rel_yday, na.rm = T)
min(df_recaps$rel_date, na.rm=T)
max_d<-max(df$recap_yday, na.rm = T)

c_empty<-seq(min_d, max_d, by =1) 
df_r<-array(0, dim=c(length(c_empty),length(c_empty)))
for(i in 1:dim(df)[1]){
  tmp_reld<-df$rel_yday[i]-min_d+1
  tmp_recd<-df$recap_yday[i]-min_d+1
  df_r[tmp_reld,tmp_recd]<-df$n[i]
}
#View(df_r)


# For checkup    
df_recaps$rel_date
x<-df_recaps|> filter(rel_yday==153, is.na(recap_date)==F) |> 
  select(rel_yday, recap_yday, everything())
print(x=x, n=100)
