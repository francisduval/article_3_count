source("renv/activate.R")

# Librairies ====================================================================================================================
library(targets)
library(tarchetypes)
library(tidyverse)
library(tidymodels)
library(glmnet)
library(here)
library(fs)
library(qs)
library(lubridate)
library(fastDummies)
library(dtplyr)
library(hms)
library(stringr)
library(dtplyr)
library(conflicted)
library(embed)
library(glue)
library(R6)
library(torch)
library(luz)
library(poissonreg)
library(vip)
library(magrittr)
library(surveillance)
library(MASS)
library(xaringan)
library(kableExtra)
library(tictoc)
library(Hmisc)
library(innsight)
library(iml)
library(ggExtra)
library(grid)

# Conflits ======================================================================================================================
conflicted::conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  hms::hms,
  purrr::set_names
)

# Options et thème ==============================================================================================================
options(scipen = 999)
theme_set(theme_bw())

# Lire les fonctions ============================================================================================================
walk(dir_ls("R/Autres"), source)
walk(dir_ls("R/Poisson models"), source)
walk(dir_ls("R/NB2 models"), source)
walk(dir_ls("R/MVNB models"), source)
walk(dir_ls("R/Preprocessing"), source)
walk(dir_ls("R/Permutation tests"), source)
walk(dir_ls("R/Partial Dependence"), source)

