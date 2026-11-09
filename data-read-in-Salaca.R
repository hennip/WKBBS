
# Salaca data from Janis
df<-read_xlsx("../../01-Projects/WGBAST/smolt-mark-recapture/Salaca/Salaca_smolt mark-recapture experiments 1964-2026.xlsx", 
                    col_names=T, guess_max = 10000)
df;dim(df)

# Rename variables and remove lines indicating that data is interpolated
df1<- df |>   
  rename(catch=`Unmarked fish captured`,
         released=`Number of marked fish released`,
         temp=`Water temperature at the rivermouth`,
         Day=Date) |> 
  filter(is.na(Coment1)==T) |>  # comments indicating catch data is interpolated 
  filter(Year>1986)
df1;dim(df1)

# Separate catch data 
df_catch<-df1 |> select(Year, Month, Day, catch )
df_catch

# Separate environmental covariates
df_env<-df1 |> select(Year, Month, Day, temp )

# Separate mark-recapture data
df_recaps<-df1 |> select(-catch ) |> 
  select(Year,Month,Day,released, `Day 1`:`Day 21`)|> 
  filter(is.na(released)==F) |> 
  mutate(Date=mdy(paste(Month, Day,Year))) |> 
  select(Date, everything()) |> 
  mutate(rel_yday=yday(Date)) |> 
  select(rel_yday, everything())

df_recaps



df_recaps|> 
  pivot_longer(`Day 1`:`Day 21`,names_to = recap_lag)

df_all |> filter(Year==2026) |> 
  
  
  
  
  
  #rename(w_temp=`veden lämpötila/ water temperature`,
  #       w_height=`vedenkorkeus/ water level`) |> 
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
