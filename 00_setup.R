##setup
#libraries
library(renv)
library(dplyr)
library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(janitor)
library(gtsummary)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(ggbreak)
library(DescTools)
library(pairwiseCI)
#file paths

source(paste0(here::here(), "/000.file_paths.R"))

singletons_path <- paste0(data_path,"1_original_data/Trends in preterm birth data_singletons_final.xlsx" )

multiples_path <- paste0(data_path,"1_original_data/Corrected_Trends in preterm birth data_multiples_03102022.xlsx" )

#functions
reldiffci <- function(x,z,cv1, cv2 ){
  uci = (x+1)*(1+ (z* sqrt(cv1^2+cv2^2 - z^2*cv1^2*cv2^2)) /1-z*cv1^2) -1
  lci = (x+1)*(1- (z* sqrt(cv1^2+cv2^2 - z^2*cv1^2*cv2^2))/1-z*cv1^2) -1
  result <- as.data.frame(cbind(x, uci, lci))
  print(result)
}
