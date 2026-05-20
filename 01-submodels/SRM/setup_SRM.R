# Select rivers for model ####
# wild rivers + Pirita in stock annex and min 3 ha size
river_sel <- c(
  # AU3 Sweden
  "Ljungan","Testeboån", 
  
  # AU4 Sweden
  "Emån","Mörrumsån","Mörrumsån_low", 
  
  # AU5 Latvia
  "Salaca","Vitrupe","Pēterupe", #"Saka",
  #"Irbe","Užava", # <2 ha
  
  # AU6 Estonia 
  "Kunda",
  "Keila","Vasalemma",
  "Pirita" # mixed river but with smolt trap
  
  # all rivers in data:
  # [1] "Mörrumsån"     "Mörrumsån_low" "Emån"          "Testeboån"     "Ljungan"       "Vasalemma"    
  # [7] "Keila"         "Vääna"         "Pirita"        "Jägala"        "Valgejõgi"     "Loobu"        
  # [13] "Selja"         "Kunda"         "Purtse"        "Salaca"        "Vitrupe"       "Pēterupe"     
  # [19] "Irbe"          "Užava"         "Saka"          "Aģe"           "Saria"         "Mera"         
  # [25] "Zeimena"       "Neris"         "Šventoji"      "Siesartis"     "Virinta"       "Vilnia"       
  # [31] "Voke"          "Musė"          "B. Šventoji"   "Debysa"        "Minija"        "Dūkšta"       
  # [37] "Kena"          "Širvinta"
  
  )



# # check data
# selected_data$size
# selected_data$size_dev
# selected_data$size_dev_river
# selected_data$parr0


mon=c(
  "alpha",
  "betas",
  "betap",
  "aalpha",
  "balpha",
  "abetas",
  "bbetas",
  "cbetas",
  "c2beta",
  "cvbeta",
  "muCV",
  "CCV",
  "EP",
  "ES",
  "Malpha",
  "Mbetas",
  "mugammap",
  "mugammas",
  "mugammap2",
  "mubetap",
  "S",
  "S_Morrum",
  "P0",
  "P1",
  "P2",
  "ap",
  "aq",
  "p",
  "q",
  "alpha_parr",
  "beta_parr",
  "Talpha",
  "Tbeta",
  "AL",
  "A",
  "IOP1",
  "IP0",
  "OP1",
  #"Mbeta_rep",
  "T_total"
)


