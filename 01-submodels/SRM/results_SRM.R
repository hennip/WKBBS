library(coda)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)
library(readr)

# check difference between two models ####
# library(diffobj)
# diffFile(
#        "01-submodels/SRM/model/model_SRM_logit_betas.R",
#        "01-submodels/SRM/model/model_SRM_parrsize_logit_betas.R"
#   )

# plot age groups ####
theme_report <- theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


# matrix from model run
d<-as.matrix(chains)

# or load one chain
# load("01-submodels/SRM/output/SRM_benchmark_baseline.Rdata"); d<-as.matrix(chains)
# load("01-submodels/SRM/output/SRM_benchmark_parr_dev.Rdata"); d<-as.matrix(chains)
# load("01-submodels/SRM/output/SRM_benchmark_logit_betaS.Rdata"); d<-as.matrix(chains)
# load("01-submodels/SRM/output/SRM_benchmark_logit_betaS_parr_dev.Rdata"); d<-as.matrix(chains)

## extract variables (need to run 'run_SRM.R' first) ####
used_rivers <- river_names[selected_data[["river_idx"]]]

p_vars  <- grep("^p\\[", colnames(d), value = TRUE)
P0_vars <- grep("^P0\\[", colnames(d), value = TRUE)
P1_vars <- grep("^P1\\[", colnames(d), value = TRUE)
S_vars <- grep("^S\\[", colnames(d), value = TRUE)
ES_vars <- grep("^ES\\[", colnames(d), value = TRUE)
EP_vars <- grep("^EP\\[", colnames(d), value = TRUE)
beta_vars <- grep("beta_parr\\[", colnames(d), value = TRUE)
betas_vars <- grep("^betas\\[", colnames(d), value = TRUE)
S_main <- grep("^S\\[", colnames(d), value = TRUE) # S_Morrum, T_total
S_morrum <- grep("^S_Morrum\\[", colnames(d), value = TRUE)
S_testebo <- grep("^T_total\\[", colnames(d), value = TRUE)
alpha <- grep("^alpha\\[", colnames(d), value = TRUE)




# summary smolt estimates ####
df_main <- as.data.frame(d[, S_main]) |>
  dplyr::mutate(iter = 1:n()) |>
  tidyr::pivot_longer(-iter, names_to = "param", values_to = "value") |>
  tidyr::extract(param,
                 into = c("year","river"),
                 regex = "S\\[(\\d+),(\\d+)\\]",
                 convert = TRUE) 

df_main$river_name <- factor(
  df_main$river,
  levels = 1:length(selected_data[["river_idx"]]),
  labels = used_rivers
)

df_morrum <- as.data.frame(d[, S_morrum]) |>
  dplyr::mutate(iter = 1:n()) |>
  tidyr::pivot_longer(-iter, names_to = "param", values_to = "value") |>
  tidyr::extract(param,
                 into = "year",
                 regex = "S_Morrum\\[(\\d+)\\]",
                 convert = TRUE) |>
  dplyr::mutate(river_name = "Mörrumsån total")

df_testebo <- as.data.frame(d[, S_testebo]) |>
  dplyr::mutate(iter = 1:n()) |>
  tidyr::pivot_longer(-iter, names_to = "param", values_to = "value") |>
  tidyr::extract(param,
                 into = "year",
                 regex = "T_total\\[(\\d+)\\]",
                 convert = TRUE) |>
  dplyr::mutate(river_name = "Testeboån adjusted")

S_all <- bind_rows(df_main, df_morrum, df_testebo)
S_all <- S_all |>
  mutate(year_real = yearlist.prep[year])



summary_smolt <- S_all |>
  group_by(year, year_real, river_name) |>
  dplyr::summarise(
    mean   = mean(value),
    median = median(value),
    mu = log(median(value)),
    lwr    = quantile(value, q1),
    upr    = quantile(value, q2),
    sigma  = get_sigma(lwr, upr, q1, q2),
    tau    = get_tau(lwr, upr, q1, q2),
    sd     = sd(value),
    var     = var(value),
    .groups = "drop"
  )

summary_smolt %>% group_by(river_name) %>% 
  dplyr::summarize(mean=mean(median),sd=sd(median),max=max(median))




# Smolt estimates plots ####
theme_report <- theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


smolt_AR<-read.table("01-submodels/SRM/data/Smolt_AnnualReport.txt",sep="\t",header=T)
smolt_AR_long <- smolt_AR |>
  pivot_longer(
    -Year,                      # keep year column
    names_to = "river_name",
    values_to = "smolt_AR"
  )
K_AR<-read.table("01-submodels/SRM/data/rivers_EA_SA_RL.txt",sep="\t",header=T)


river_au <- tibble::tibble(
  river_name = c("Ljungan","Testeboån adjusted" # AU3
                 ,"Emån","Mörrumsån total" # AU4
                 ,"Salaca","Vitrupe","Pēterupe","Irbe","Užava","Saka", # AU5 wild
                 "Aģe" # AU5 potential
                 ,"Vasalemma","Keila","Kunda" # AU6 wild
                 ,"Vääna", "Pirita", "Jägala", "Valgejõgi","Loobu","Selja", "Purtse"  # AU6 mixed
  ),
  AU = c("AU3","AU3",
         "AU4","AU4",
         "AU5","AU5","AU5","AU5","AU5","AU5",
         "AU5",
         "AU6","AU6","AU6",
         "AU6","AU6","AU6","AU6","AU6","AU6","AU6")
)

summary_smolt2 <- summary_smolt |>
  left_join(river_au, by = "river_name")
plotdata_smolt <- summary_smolt2 %>% 
  filter(!river_name %in% c("Testeboån","Mörrumsån","Mörrumsån_low"))
K_AR$river_name <- K_AR$river
plotdata_K <- K_AR %>% 
  filter(river_name %in% unique(plotdata_smolt$river_name)) %>% 
  filter(!river_name %in% c("Ljungan","Emån"))|>
  left_join(river_au, by = "river_name")
plotdata_prevsmolt <- smolt_AR_long %>% 
  filter(river_name %in% unique(plotdata_smolt$river_name))  |>
  left_join(river_au, by = "river_name")

ggplot(
  plotdata_smolt,
  aes(x = year_real, y = median)
) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  
  geom_hline(
    data = plotdata_K, aes(yintercept = PSPC), colour = "blue",
    linetype = "dashed",  size = 1, inherit.aes = FALSE ) +
  
  # Add AR method points
  geom_line(
    data = plotdata_prevsmolt,
    aes(x = Year, y = smolt_AR),
    colour = "red",
    #shape = 17,
    size = 1,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(values = c("Estimated from parr" = "red")) +
  facet_wrap(~ AU+river_name, ncol = 4, scales = "free_y") +
  labs(x = "Year", y = "Smolt estimate (median)") +
  theme_bw()




---  --- --- --- --- --- --- --- ---   
#    COMPARE MULTIPLE MODELS  #####
---  --- --- --- --- --- --- --- ---


---  --- --- --- --- --- --- --- --- 
## Select and name models #  ----
---  --- --- --- --- --- --- --- --- 

model_files <- tibble(
  model = c("baseline", 
            "parr_size", 
            "logit_betas", 
            "parr_size_logit"),
  file  = c(
    "01-submodels/SRM/output/SRM_benchmark_baseline.Rdata",
    "01-submodels/SRM/output/SRM_benchmark_parr_dev.Rdata",
    "01-submodels/SRM/output/SRM_benchmark_logit_betaS.Rdata",
    "01-submodels/SRM/output/SRM_benchmark_logit_betaS_parr_dev.Rdata"
    )
)

# river_sel must be in the same order as in the JAGS model
river_lookup <- tibble(
  river = seq_along(river_sel),
  river_name = river_sel
)

# --- --- --- --- --- --- ---
# Helper: load saved chains
# --- --- --- --- --- --- --- 

load_chain <- function(file) {
  e <- new.env()
  load(file, envir = e)
  
  # adjust if your object name is different
  if ("chains" %in% ls(e)) return(e$chains)
  if ("run" %in% ls(e)) return(as.mcmc.list(e$run))
  if ("run1" %in% ls(e)) return(as.mcmc.list(e$run1))
  
  stop("No recognised chain object found in: ", file)
}

chains_list <- model_files %>%
  mutate(chains = map(file, load_chain))

# --- --- --- --- --- --- ---
# Convert chains to dataframe
# --- --- --- --- --- --- ---

chains_to_df <- function(chains) {
  as.data.frame(as.matrix(chains)) %>%
    mutate(draw = row_number()) %>%
    pivot_longer(
      cols = -draw,
      names_to = "param",
      values_to = "value"
    )
}

all_draws <- chains_list %>%
  transmute(model, draws = map(chains, chains_to_df)) %>%
  unnest(draws)

--- --- --- --- --- --- --- ---
## Extract betas[r] ####
--- --- --- --- --- --- --- ---

beta_df <- all_draws %>%
  filter(str_detect(param, "^betas\\[")) %>%
  tidyr::extract(
    param,
    into = "river",
    regex = "betas\\[(\\d+)\\]",
    convert = TRUE
  ) %>%
  left_join(river_lookup, by = "river")

beta_summary <- beta_df %>%
  group_by(model, river_name) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    l95 = quantile(value, 0.025),
    u95 = quantile(value, 0.975),
    sd = sd(value),
    .groups = "drop"
  )

# table
beta_summary
#write_csv(beta_summary, "beta_summary_four_models.csv")

# figure
fig_beta <- beta_summary %>%
  ggplot(aes(x = river_name, y = median, colour = model)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbar(
    aes(ymin = l95, ymax = u95),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  coord_flip() +
  labs(
    x = "River",
    y = expression(beta),
    colour = "Model",
    title = "Comparison of parr-to-smolt survival among models"
  ) +
  theme_bw()

fig_beta




--- --- --- --- --- --- --- ---
## Extract S[year, river] ####
--- --- --- --- --- --- --- ---

S_df <- all_draws %>%
  filter(str_detect(param, "^S\\[")) %>%
  tidyr::extract(
    param,
    into = c("year", "river"),
    regex = "S\\[(\\d+),(\\d+)\\]",
    convert = TRUE
  ) %>%
  left_join(river_lookup, by = "river")

# Optional: convert model year index to real year
# years_vec should be your real year vector, e.g. 1985:2025
years_vec <- c(1985:2025)
S_df <- S_df %>%
  mutate(year_real = years_vec[year])

S_summary <- S_df %>%
  group_by(model, river_name, year, year_real) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    l95 = quantile(value, 0.025),
    u95 = quantile(value, 0.975),
    sd = sd(value),
    .groups = "drop"
  )

#write_csv(S_summary, "S_summary_annual_four_models.csv")

fig_S <- S_summary %>%
  ggplot(aes(x = year_real, y = median, colour = model, fill = model)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.15, colour = NA) +
  facet_wrap(~ river_name, scales = "free_y") +
  labs(
    x = "Year",
    y = "Smolt abundance",
    colour = "Model",
    fill = "Model",
    title = "Annual smolt estimates by model"
  ) +
  theme_bw()

fig_S











