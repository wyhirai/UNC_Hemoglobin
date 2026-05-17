# 1. Libraries ------------------------------------------------------------------

#libraries
library(magrittr); library(dplyr); library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(ggrepel)
library(gtsummary)
library(lme4); library(lmerTest) 
library(ggeffects); library(emmeans)
library(readxl)
library(lubridate)


# 2. Functions Stats ---------------------------------------------------------------------------------------

my_stats <- function(data, variable, ...) {
  range <- max(data[[variable]], na.rm = T) - min(data[[variable]], na.rm = T)
  IQR <- as.numeric(quantile(data1$age_years_2)[4]) - as.numeric(quantile(data1$age_years_2)[2])
  dplyr::tibble(
    Range = range,
    IQR = IQR
  )
}

# 3. Import Data -------------------------------------------------------------------------------------------

# 3.1 Plan: Y25M02D23_BD - Sheet: Pacient ------------------------------------------------------------------

Final_Data <- read_excel("./input/Y25M02D23_BD.xlsx", sheet = 'Demographics')

tab1_names <- 
  data.frame(Names = names(Final_Data),
             Code_Names = janitor::make_clean_names(names(Final_Data)))

data1 <- Final_Data
names(data1) <- tab1_names$Code_Names

data1_filter <- 
  data1 %>% 
  #incresing new variable
  dplyr::mutate(rbc_antibodies = case_when(
    allo_ab_1_yes_0_no == '1' | h_o_rbc_autoantibodies == 'Yes'  ~ 'Yes',
    .default = 'No'
  )) %>% 
  #excluided patient
  dplyr::filter(patient_1 != '52') %>% 
  dplyr::mutate(rbc_antibodies = if_else(patient_1 %in% c(32, 34), 'Yes', rbc_antibodies)) %>% 
  dplyr::mutate(race_4 = case_when(race_4 == 'Hispanic' ~ 'other', .default = race_4)) %>% 
  dplyr::mutate(
    SCD_genotype_2CAT = case_when(
      scd_genotype_7 %in% c("SS", 'SB0 thal') ~ 'Group 1 - SS/SBO thal',
      scd_genotype_7 %in% c('SC') ~ 'Group 2 - SC',
      scd_genotype_7 %in% c('Other', 'HbS beta Indianapolis',
                            'sickle cell beta plus thalassemia') ~ NA)
    
  )

write.csv(tab1_names, './output/names_var_Pacient.csv', row.names = F)

# 3.2 Plan: Y25M02D23_BD - Sheet: Mester RCE Info ---------------------------------------------------------

BD_RCE <- read_excel("./input/Y25M02D23_BD.xlsx", sheet = 'Master RCE Info')

tab2_names <- 
  data.frame(Names = names(BD_RCE),
             Code_Names = janitor::make_clean_names(names(BD_RCE)))

data2 <- BD_RCE 
names(data2) <- tab2_names$Code_Names

data2_filter <- 
  data2 %>% 
  dplyr::filter(patient_id != '52')

tab2_names <- 
  tab2_names %>% 
  tibble::add_row(
    Names = 'difference between procedure times',
    Code_Names = 'DIFF_Between_RCE'
  )

write.csv(tab2_names, './output/names_var_RCE.csv', row.names = F)

# 4. Selected: Unit Observation - ID and Procedure ------------------------------------------------------

data1_filter %>% dplyr::select(patient_1)

data2_filter %>% 
  dplyr::select(patient_id, rce_procedure_number, date_of_rce)


# 4. Import End --------------------------------------------------------------------------------------------

envir_end <- c('my_stats', 'data1_filter', 'data2_filter') 

rm(list = setdiff(ls(), envir_end))






































