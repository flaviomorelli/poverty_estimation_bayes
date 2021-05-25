library(tidyverse)

# start <- proc.time()

# Abstract household data cleaning

clean_hog <- function(data){
  data %>% 
    select(
      # Categorical variables 
      id_viv,
      jpea, 
      jsector, 
      id_men, 
      trabinf, 
      trabadulmay, 
      jsexo, 
      remesas, 
      ayuotr, 
      bengob, 
      est_calidad_vivienda, 
      pob_ind, 
      discap_hog, 
      # Metric values
      jexp, 
      jedad, 
      actcom, 
      bienes, 
      pcpering, 
      pcocup, 
      jaesc, 
      tam_hog, 
      muj_hog, 
      nalfab,
      rururb,
      factor, 
      mun,
      tam_loc
    ) %>% 
    # Clean categorical variables
    mutate(trabinf = ifelse(str_detect(trabinf, "Con trabajo"), TRUE, FALSE),
           trabadulmay = ifelse(str_detect(trabadulmay, "Con trabajo"), TRUE, FALSE),
           trabinusual = trabinf | trabadulmay,
           ingresoext = remesas=="Recibe" | ayuotr=="Recibe" | bengob=="Recibe",
           id_men = ifelse(str_detect(id_men, "Con"), TRUE, FALSE),
           pob_ind = ifelse(pob_ind == "Si", TRUE, FALSE),
           discap_hog = ifelse(str_detect(discap_hog, "Con"), TRUE, FALSE)
    ) %>%
    # Percentages instead of absolute values
    mutate(pcmuj = muj_hog / tam_hog * 100, 
           pcalfab = nalfab / tam_hog * 100,
           actcom_pc = actcom / tam_hog * 100, 
           bienes_pc = bienes/ tam_hog * 100
    ) %>% 
    select(-trabinf, 
           -trabadulmay, 
           -remesas, 
           -ayuotr, 
           -bengob)
}

# Load raw data sets

mcs_per_raw <- readstata13::read.dta13("data/mexico/guerrero_mcs_per.dta", 
                                       nonint.factors = TRUE) %>% 
  as_tibble() %>% 
  mutate(id_viv = str_c(folioviv, foliohog)) %>% 
  select(-folioviv, -foliohog)

mcs_hog_raw <- readstata13::read.dta13("data/mexico/guerrero_mcs_hog.dta",
                                       nonint.factors = TRUE) %>% 
  as_tibble() %>% 
  mutate(id_viv = str_c(folioviv, foliohog)) %>% 
  mutate(mun = as.numeric(str_sub(est_dis, 3, 4)))%>% 
  select(-folioviv, -foliohog, -est_dis)

census_per_raw <- readstata13::read.dta13("data/mexico/guerrero_censo_per.dta",
                                          nonint.factors = TRUE) %>% 
  as_tibble()

census_hog_raw <- readstata13::read.dta13("data/mexico/guerrero_censo_hog.dta",
                                          nonint.factors = TRUE) %>% 
  as_tibble()

# MCS data sets

mcs_per <- mcs_per_raw %>% 
  filter(numren == "01") %>% #Get only first person in household
  select(id_viv, starts_with("ic"))

mcs_hog <- clean_hog(mcs_hog_raw)

mcs <- mcs_hog %>% inner_join(mcs_per, by = "id_viv") %>% 
  select(-id_viv, 
         -ic_segsoc,
         -ic_ali)

# Census data sets
census_per <- census_per_raw %>% 
  filter(parentesco == "01") %>% #Get only first person in household
  select(id_viv, starts_with("ic"))

census_hog <- clean_hog(census_hog_raw)

census <- census_hog %>% inner_join(census_per, by = "id_viv") %>% 
  select(-id_viv)%>% 
  mutate(mun = as.numeric(mun))

rm(mcs_hog_raw, mcs_hog,
   mcs_per_raw, mcs_per,
   census_hog_raw, census_hog,
   census_per_raw, census_per,
   clean_hog)

# Ca. 20 seconds to load
# elapsed <- proc.time() - start
