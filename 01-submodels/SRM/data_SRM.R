# to do, smolt age now in sdat, but now have missing data


library(dplyr)
library(tidyr)


# for data preparation
yearlist.prep<-1985:(assessment_year)
years.prep<-length(seq(1985:(assessment_year)))

# read and subset data ####
ef<-read.table("01-submodels/SRM/data/parr_data.txt",sep="\t",header=T, comment.char = ""); table(ef$site_type)
  dat<-subset(ef, year>1984 & year<assessment_year & 
              !site_type %in% c(
                "All", # keep ordinary, and remove all with additional sites (Sweden) 
                "mixed+wild" # keep wild and mixed rivers
                                ) )  

  
smolt<-read.table("01-submodels/SRM/data/smolt_data.txt",sep="\t",header=T)
  sdat<-subset(smolt,year>1984 & year<assessment_year)
  sdat$S_age1_count <- round(sdat$age1/100 * sdat$n_smolt)
  sdat$S_age2_count <- round(sdat$age2/100 * sdat$n_smolt)
  sdat$S_age3_count <- sdat$n_smolt - sdat$S_age1_count - sdat$S_age2_count

river_meta<-read.table("01-submodels/SRM/data/rivers_EA_SA_RL.txt",sep="\t",header=T)
  

## check dat data
  # table(dat$au,dat$site_type)
  # table(dat$country,dat$site_type)
  # table(dat$river,dat$au)
  # 
  # dat.sum <- dat %>% 
  #   group_by(au,country,river) %>% 
  #   dplyr::summarize(n=n(),n.obs=sum(nsites),
  #                    Salmon0P=mean(density_0,na.rm=T),
  #                    Salmon1P=mean(density_1,na.rm=T))

# Fix data ####
# Full complete years for all rivers and sections
  
  section_meta <- dat %>%
    distinct(river, section, site_type, area)

  full_grid <- section_meta %>%
    tidyr::expand_grid(year = yearlist.prep)

  dat_full <- full_grid %>%
    left_join(
      dat %>%
      select(river, section, site_type, area, year, density_0, density_1, nsites, size_0
             #, S_age1_count,S_age2_count,S_age3_count
             ),
      by = c("river", "section","site_type","area","year")
    ) %>% 
    mutate(
      # if there were no sites sampled (missing year), densities must be NA
      density_0 = ifelse(is.na(density_0), NA_real_, density_0),
      density_1 = ifelse(is.na(density_1), NA_real_, density_1)#,
      # now set n to 1 for model requirement
      # nsites = ifelse(is.na(nsites), 1L, nsites)
   )

# rows with NA, sample size = 0
# rows with extrapolated values, sample size = 1
# ? should projection year have sample size = 1 for each section?
  
## Vasalemma ####
  # zero values for section 2 in Vasalemma (before opening up habitat)
dat_full$nsites[dat_full$river=="Vasalemma" & dat_full$section==2 & is.na(dat_full$nsites)] <- 1
# nsites for missing values -> 0
dat_full$nsites[is.na(dat_full$nsites)] <- 0

dat_full$density_0[dat_full$river=="Vasalemma" & dat_full$section==2 & is.na(dat_full$density_0)] <- 0
dat_full$density_1[dat_full$river=="Vasalemma" & dat_full$section==2 & is.na(dat_full$density_1)] <- 0

dat_full$density_0[dat_full$year==2025]<-NA
dat_full$density_1[dat_full$year==2025]<-NA

## Corrections ####
# Keila size data: size_0 Keila 1997 should be 1999 (check with Martin)
dat_full[dat_full$river=="Keila" & dat_full$year %in% c(1997,1998,1999),] # length when no density
dat_full$size_0[dat_full$river=="Keila" & dat_full$year==1997]  <- NA
dat_full$size_0[dat_full$river=="Keila" & dat_full$year==1999] <- 79

# Testeboån 
dat_full$size_0[ is.na(dat_full$density_0) ]<-NA # Testeboån 2023

## weighted average ####
# for rivers with multiple sections
library(zoo)
ef_river <- dat_full %>%
  group_by(river, year) %>%
  dplyr::summarise(
    total_area = sum(area, na.rm = TRUE),
    
    parr0 = if (all(is.na(density_0))) {
      NA_real_
    } else {
      sum(density_0 * area, na.rm = TRUE) / total_area
    },
    
    parr1 = if (all(is.na(density_1))) {
      NA_real_
    } else {
      sum(density_1 * area, na.rm = TRUE) / total_area
    },
    
    size0 = if (all(is.na(size_0))) {
      NA_real_
    } else {
      #sum(size_0 * area, na.rm = TRUE) / total_area
      sum(size_0 * area, na.rm = TRUE) / sum(area[!is.na(size_0)]) # only include sections with data
    },
    
    nsites = sum(nsites, na.rm = TRUE),
    .groups = "drop"
  )



# parr size (global average)
ef_river <- ef_river %>%
  arrange(river,year) %>%
  mutate(
    
    # river mean
    mean_size0_all = mean(size0, na.rm = TRUE),
    mean_parr0_all = mean(parr0, na.rm = TRUE)
    
  ) %>%
  ungroup()

# replace missing values with 1. moving average, 2. mean values (global means vs river means)
# create river means for parr0 density and parr0 size
ef_river <- ef_river %>%
  arrange(river, year) %>%
  group_by(river) %>%
  mutate(
    # moving average
    size0_ma = rollapply(size0, 3, mean, fill = NA, align = "center", na.rm = TRUE),
    parr0_ma = rollapply(parr0, 3, mean, fill = NA, align = "center", na.rm = TRUE),
    
    # river mean
    mean_size0_river = mean(size0, na.rm = TRUE),
    mean_parr0_river = mean(parr0, na.rm = TRUE),
    
    # fill missing values
    size0_filled = ifelse(
      is.na(size0),
      ifelse(is.na(size0_ma), mean_size0_river, size0_ma),
      size0
    ),
    
    parr0_filled = ifelse(
      is.na(parr0),
      ifelse(is.na(parr0_ma), mean_parr0_river, parr0_ma),
      parr0
    ),
    
    # deviation (what goes into JAGS)
    #size0_dev = size0_filled - mean(size0_filled, na.rm = TRUE)
    size0_dev = size0_filled - mean_size0_all, # global means
    size0_dev_river = size0_filled - mean_size0_river, # river means
    parr0_dev_river = parr0_filled - mean_parr0_river # river means
    
  ) %>%
  ungroup()

ef_river$nsites[ef_river$nsites==0]<-1 # must be 1 for model 


# replace NaN with NA
ef_river[] <- lapply(ef_river, function(x) {
  if (is.numeric(x)) x[is.nan(x)] <- NA
  x
})

table(ef_river$river)

## IP0, IP1, n ####
ef_river <- ef_river %>%
  mutate(
    IP0 = parr0 * nsites * 5,
    IP1 = parr1 * nsites * 5
  )
ef_river <- ef_river %>%
  arrange(year, river)

IP0_wide <- ef_river %>%
  select(year, river, IP0) %>%
  pivot_wider(
    names_from = river,
    values_from = IP0
  ) %>%
    arrange(year) 
  IP0 <- IP0_wide %>% select(-year) %>%
    as.matrix()
  rownames(IP0)  <- IP0_wide$year

IP1_wide <- ef_river %>%
  select(year, river, IP1) %>%
  pivot_wider(
    names_from = river,
    values_from = IP1
  ) %>%
    arrange(year) 
  IP1 <- IP1_wide %>%   select(-year) %>%
    as.matrix()
  rownames(IP1) <- IP1_wide$year

n_wide <- ef_river %>%
  select(year, river, nsites) %>%
  pivot_wider(
    names_from = river,
    values_from = nsites
  ) %>%
  arrange(year) 
n <- n_wide %>%   select(-year) %>%
  as.matrix()
rownames(n)  <- n_wide$year

## Parr0 size ####
# untransformed size data
size_wide <- ef_river %>%
  select(year, river, size0_filled) %>%
  pivot_wider(
    names_from = river,
    values_from = size0_filled
  ) %>%
  arrange(year) 
size <- size_wide %>%   select(-year) %>%
  as.matrix()
rownames(size) <- size_wide$year

# size deviation from global mean
size_dev_wide <- ef_river %>%
  select(year, river, size0_dev) %>%
  pivot_wider(
    names_from = river,
    values_from = size0_dev
  ) %>%
  arrange(year) 
size_dev <- size_dev_wide %>%   select(-year) %>%
  as.matrix()
rownames(size_dev) <- size_dev_wide$year

# size deviation from river mean
size_dev_river_wide <- ef_river %>%
  select(year, river, size0_dev_river) %>%
  pivot_wider(
    names_from = river,
    values_from = size0_dev_river
  ) %>%
  arrange(year) 
size_dev_river <- size_dev_river_wide %>%   select(-year) %>%
  as.matrix()
rownames(size_dev_river) <- size_dev_river_wide$year

## Parr density (for density model) ####
# untransformed parr densities
parr0_wide <- ef_river %>%
  select(year, river, parr0_filled) %>%
  pivot_wider(
    names_from = river,
    values_from = parr0_filled
  ) %>%
  arrange(year) 
parr0 <- parr0_wide %>%   select(-year) %>%
  as.matrix()
rownames(parr0) <- parr0_wide$year

# parr density river deviation
parr0_dev_river_wide <- ef_river %>%
  select(year, river, parr0_dev_river) %>%
  pivot_wider(
    names_from = river,
    values_from = parr0_dev_river
  ) %>%
  arrange(year) 
parr0_dev_river <- parr0_dev_river_wide %>%   select(-year) %>%
  as.matrix()
rownames(parr0_dev_river) <- parr0_dev_river_wide$year

#years_vec <- parr0_dev_river_wide$year
#selected_data$age_year <- match(selected_data$year, years_vec)



## River index  ####
# (order all rivers in data)
river_names <- colnames(IP0) # important! to ensure same order for smolt data, habitat size and CV and RL
nrivers<- length(river_names) 


## IS and CIS ####
smolt_full <- expand_grid(
  year  = yearlist.prep,
  river = river_names
) %>%
  left_join(sdat, by = c("river", "year"))

smolt_full <- smolt_full %>%
  mutate(
    CIS = ifelse(is.na(CIS), 0.2, CIS)
  )
IS_wide <- smolt_full %>%
  select(year, river, IS) %>%
  pivot_wider(names_from = river, values_from = IS) %>%
  arrange(year)
IS  <- IS_wide  %>% select(-year) %>% as.matrix()
rownames(IS)  <- IS_wide$year

CIS_wide <- smolt_full %>%
  select(year, river, CIS) %>%
  pivot_wider(names_from = river, values_from = CIS) %>%
  arrange(year)
CIS <- CIS_wide %>% select(-year) %>% as.matrix()
rownames(CIS) <- CIS_wide$year

## Smolt Age ####
# aligned with model
age_obs <- sdat %>%
  filter(
    !is.na(S_age1_count),
    !is.na(S_age2_count),
    !is.na(S_age3_count)
  ) %>%
  mutate(
    S_age1_count = round(S_age1_count),
    S_age2_count = round(S_age2_count),
    S_age3_count = round(S_age3_count)
  )

age_obs$age_year  <- match(age_obs$year, yearlist.prep)
age_obs$age_river <- match(age_obs$river, river_names)

age_obs <- age_obs %>%
  filter(!is.na(age_year), !is.na(age_river))

# smolt age variables for model
age_count_full <- as.matrix(age_obs[, c("S_age1_count","S_age2_count","S_age3_count")])
n_aged_full    <- rowSums(age_count_full)
age_year_full  <- age_obs$age_year
age_river_full <- age_obs$age_river


## EA, SA, RL ####
river_meta <- river_meta[match(river_names, river_meta$river), ]
stopifnot(all(river_meta$river == river_names))

EA <- log(river_meta$EA * 100)
SA <- river_meta$SA
RL <- river_meta$RL

length(EA) == ncol(IP0)
length(SA) == ncol(IP0)
length(RL) == ncol(IP0)

dim(IP0) == dim(size_dev)
dim(size_dev)



## Testeboån turbine mortality ####
# extra 5% production occurs downstream (before turbine mortality applied)

require(rjags)
library(rriskDistributions)
require(plotrix)

# Lower and upper quantiles (likely 5% and 95%) for turbine passage probability
# Each element corresponds to one year (or time period)
# this will be converted into beta distribution
turbine_qlow<-c(0.23,0.15,0.51,0.32,0.70,
                0.75,0.15,0.75,0.52,0.59,
                0.19,0.75,0.40,0.37,0.75,
                0.27,0.24,0.40,0.00,0.001,
                0.40,0.30,0.30,0.10,0.00,
                0.18)
turbine_qhigh<-c(0.63,0.55,0.91,0.72,1.00,
                 1.00,0.55,1.00,0.92,0.99,
                 0.59,1.00,0.80,0.77,1.00,
                 0.67,0.64,0.80,0.37,0.002,
                 0.80,0.70,0.70,0.50,0.40,
                 0.58)

# Create a matrix to store Beta distribution parameters
# Rows = years (or time steps)
# Columns = shape parameters (alpha, beta)
turbine_prob<-array(NA,dim=c(length(turbine_qhigh),2))
for(i in 1:length(turbine_qlow)){
  turbine_prob[i,]<-get.beta.par(p=c(0.05,0.95),q=c(turbine_qlow[i],turbine_qhigh[i]))
}
turbine_mort<-numeric(2)
turbine_mort<-get.beta.par(c(0.05,0.95),c(0.5,0.9))






