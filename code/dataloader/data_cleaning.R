# start <- proc.time()

# TODO dummies for categorical variables

sector_lookup <- function(sector){
  if(is.na(sector))
    return(NA)
  if(str_detect(sector, "terciario"))
      return("Terciario")
  if(str_detect(sector, "secundario"))
      return("Secundario")
  if(str_detect(sector, "primario"))
      return("Primario")
  if(str_detect(sector, "PNEA|desocupado|menor")) 
      return("Desempleado")
  else stop(str_c("No match for sector: ", sector, ".\n"))
}


ic_lookup <- function(ic){
  if(is.na(ic))
    return(NA)
  if(str_detect(ic, "Presenta carencia"))
    return(1)
  if(str_detect(ic, "Con carencia"))
    return(1)
  if(str_detect(ic, "No presenta carencia"))
    return(0)
  if(str_detect(ic, "Sin carencia"))
    return(0)
  else stop(str_c("No match for sector: ", ic, ".\n"))
}


clean_hog <- function(data){
  data %>% 
    dplyr::select(
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
    dplyr::mutate(trabinf = ifelse(str_detect(trabinf, "Con trabajo"), TRUE, FALSE),
           trabadulmay = ifelse(str_detect(trabadulmay, "Con trabajo"), TRUE, FALSE),
           trabinusual = trabinf | trabadulmay,
           ingresoext = remesas=="Recibe" | ayuotr=="Recibe" | bengob=="Recibe",
           id_men = ifelse(str_detect(id_men, "Con"), TRUE, FALSE),
           pob_ind = ifelse(pob_ind == "Si", TRUE, FALSE),
           discap_hog = ifelse(str_detect(discap_hog, "Con"), TRUE, FALSE)
    ) %>%
    # Percentages instead of absolute values
    dplyr::mutate(pcmuj = muj_hog / tam_hog * 100, 
           pcalfab = nalfab / tam_hog * 100,
           actcom_pc = actcom / tam_hog, 
           bienes_pc = bienes / tam_hog
    ) %>% 
    # Recode sector variable
    dplyr::mutate(jsector = purrr::map_chr(jsector, sector_lookup)) %>% 
    dplyr::select(-trabinf, 
           -trabadulmay, 
           -remesas, 
           -ayuotr, 
           -bengob)
}

# Load raw data sets

message("Loading MCS data...")

mcs_per_raw <- readstata13::read.dta13("data/mexico/guerrero_mcs_per.dta", 
                                       nonint.factors = TRUE) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(id_viv = str_c(folioviv, foliohog)) %>% 
  dplyr::select(-folioviv, -foliohog)

mcs_hog_raw <- readstata13::read.dta13("data/mexico/guerrero_mcs_hog.dta",
                                       nonint.factors = TRUE) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(id_viv = str_c(folioviv, foliohog)) %>% 
  dplyr::mutate(mun = as.numeric(str_sub(ubica_geo, 4, 5))) %>% 
  dplyr::select(-folioviv, -foliohog, -est_dis, -ubica_geo)

message("Loading census data...")

census_per_raw <- readstata13::read.dta13("data/mexico/guerrero_censo_per.dta",
                                          nonint.factors = TRUE) %>% 
  dplyr::as_tibble()

census_hog_raw <- readstata13::read.dta13("data/mexico/guerrero_censo_hog.dta",
                                          nonint.factors = TRUE) %>% 
  dplyr::as_tibble()

message("Cleaning the data sets...")

# MCS data sets

mcs_per <- mcs_per_raw %>% 
  dplyr::filter(numren == "01") %>% # Get only first person in household
  dplyr::select(id_viv, starts_with("ic")) %>%
  dplyr::mutate(across(starts_with("ic_"), ~purrr::map_dbl(.x, ic_lookup)))

mcs_hog <- clean_hog(mcs_hog_raw)

mcs <- mcs_hog %>% 
  dplyr::inner_join(mcs_per, by = "id_viv") %>% 
  dplyr::select(-id_viv, 
         -ic_segsoc,
         -ic_ali) %>% 
  dplyr::mutate(ictpc_corr = ifelse(ictpc < 1, 
                             ictpc + runif(1, 0.6, 10), 
                             ictpc)) %>% 
  dplyr::mutate(strat_idx = str_c(as.numeric(rururb == "Rural"), 
                                  ic_rezedu, ic_asalud, ic_sbv, ic_cv))

# Census data sets
census_per <- census_per_raw %>% 
  dplyr::filter(parentesco == "01") %>% # Get only first person in household
  dplyr::select(id_viv, starts_with("ic")) %>%
  dplyr::mutate(across(starts_with("ic_"), ~purrr::map_dbl(.x, ic_lookup)))

census_hog <- clean_hog(census_hog_raw)

census <- census_hog %>% 
  dplyr::inner_join(census_per, by = "id_viv") %>% 
  dplyr::select(-id_viv)%>% 
  dplyr::mutate(mun = as.numeric(mun)) %>% 
  dplyr::mutate(strat_idx = str_c(as.numeric(rururb == "Rural"), 
                                  ic_rezedu, ic_asalud, ic_sbv, ic_cv))

message("Creating MCS list for Stan...")
X <- mcs %>% select(jsector, jsexo, jexp, jedad,
                    id_men, trabinusual, pcocup, pcpering, ingresoext,
                    pcmuj, pcalfab, actcom_pc, bienes_pc, pob_ind, rururb)

domain <- sapply(mcs$mun, function(x) which(unique(mcs$mun) == x))
mcs_stan <- list(N = nrow(mcs),
                 K = ncol(X), 
                 D = length(unique(mcs$mun)), 
                 y = mcs$ictpc, 
                 X = X, 
                 domain = domain)

message("Cleaning the workspace...")
rm(mcs_hog_raw, mcs_hog,
   mcs_per_raw, mcs_per,
   census_hog_raw, census_hog,
   census_per_raw, census_per,
   clean_hog, sector_lookup, 
   X, domain, ic_lookup)

message("Mexico data loaded!")

# Ca. 20 seconds to load
# elapsed <- proc.time() - start
