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
library(gee)
library(multilevelmod)
library(geepack)
library(embed)
library(glue)
library(R6)
library(torch)

# Conflits ======================================================================================================================
conflicted::conflicts_prefer(
  dplyr::filter,
  hms::hms
)

# Options et thème ==============================================================================================================
options(scipen = 999)
theme_set(theme_bw())

# Lire les fonctions ============================================================================================================
walk(dir_ls("R"), source)
