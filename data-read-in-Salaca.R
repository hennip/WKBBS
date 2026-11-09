
# Salaca data from Janis
df<-read_xlsx("../../01-Projects/WGBAST/smolt-mark-recapture/Salaca/Salaca_smolt mark-recapture experiments 1964-2026.xlsx", 
                    col_names=T, guess_max = 10000)
df;dim(df)

# Rename variables and remove lines indicating that data is interpolated
df1<- df |>   
  rename(catch=`Unmarked fish captured`,
         n_marked=`Number of marked fish released`,
         temp=`Water temperature at the rivermouth`,
         Day=Date) |> 
  mutate(Date=mdy(paste(Month, Day,Year))) |> 
  filter(is.na(Coment1)==T) |>  # comments indicating catch data is interpolated 
  mutate(date_yday=yday(Date)) |> 
  select(Date,date_yday, everything())
  
df1<-df1 |> filter(Year==2025)
df1;dim(df1)

# Environmental covariates
# ==============================
df_env<-df1 |> select(Year, Month, Day, temp )
# Covariates
wt<-df_catch$w_temp
wl<-df_catch$w_height


# Catch and release data 
# ==============================
df_cm<-df1 |> 
  select(Year,date_yday, catch, n_marked ) 
# Remove days from the start with zero catch
min_catch_date<-min(df_c |> filter(catch>0) |> select(date_yday))
df_cm<-df_cm |> filter(date_yday>=min_catch_date)
print(x=df_cm, n=100)

N<-dim(df_cm)[1]

m<-df_cm |> select(n_marked) |> pull()
m[is.na(m)]<-0
rind<-which(m!=0)  #indices with non 0 releases of tagged fish 
rind<-rind[!(rind %in% N)] 


# Mark-recapture data
# ==============================

df_mark_recaps<-df1 |> select(-catch ) |> 
  select(Year,date_yday,n_released, `Day 1`:`Day 21`)|> 
  filter(is.na(n_released)==F) |> 
  rename(rel_yday=date_yday) |> 
  select(Year,rel_yday, n_released, `Day 1`:`Day 21`)

# # Separate releases (number of marked)
# df_rel<-df2 |> 
#   select(Year,rel_yday, released)
# df_rel

# Separate recaptures
df_recaps<-df_mark_recaps |> pivot_longer(`Day 1`:`Day 21`, names_to = "rec_lag") |> 
  rename(n_recap=value) |> 
  mutate(rec_lag2=as.numeric(str_extract(rec_lag, "[0-9]+"))) |> 
  mutate(rec_yday=rel_yday+rec_lag2) |> 
  select(Year, rel_yday, rec_yday, n_recap)

df_recaps


# Check that start dates are the same
min(df_c$date_yday)
min_catch_date # Check that this is the same
min(df_recaps$rel_yday, na.rm=T)


# Build r matrix:
min_d<-min(df_recaps$rel_yday, na.rm = T)
max_d<-max(df_recaps$rec_yday, na.rm = T)

c_empty<-seq(min_d, max_d, by =1) 
df_r<-array(0, dim=c(length(c_empty),length(c_empty)))
for(i in 1:dim(df_recaps)[1]){
  tmp_reld<-df_recaps$rel_yday[i]-min_d+1
  tmp_recd<-df_recaps$rec_yday[i]-min_d+1
  df_r[tmp_reld,tmp_recd]<-df_recaps$n_recap[i]
}
#View(df_r)





# For checkup    
df_recaps$rel_date
x<-df_recaps|> filter(rel_yday==153, is.na(recap_date)==F) |> 
  select(rel_yday, recap_yday, everything())
print(x=x, n=100)
