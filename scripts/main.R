# main.R

library(readr)
library(tidyverse)
library(ggplot2)
library(patchwork)

rm(list = ls())

source("scripts/01_load_data.R")
source("scripts/02_features_new.R")
source("scripts/03_final_four.R")

source("scripts/03_OLS_linear_new.R")
