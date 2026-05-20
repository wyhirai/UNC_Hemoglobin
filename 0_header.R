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

wyh_theme <- 
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(colour = 'black'), legend.position = 'top')

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
    
  ) %>% 
  #new variable
  dplyr::mutate(
    rbc_antibodies_new = case_when(
      (h_o_rbc_alloantibodies == 'Yes' | h_o_rbc_autoantibodies == 'Yes') & allo_ab_1_yes_0_no == 0 ~ 'Group 1',
      h_o_rbc_alloantibodies == 'No' & h_o_rbc_autoantibodies == 'No' & allo_ab_1_yes_0_no == 1 ~ 'Group 2',
      (h_o_rbc_alloantibodies == 'Yes' | h_o_rbc_autoantibodies == 'Yes') & allo_ab_1_yes_0_no == 1 ~ 'Group 3',
      h_o_rbc_alloantibodies == 'No' & h_o_rbc_autoantibodies == 'No' & allo_ab_1_yes_0_no == 0 ~ 'Group 4',
    )
  ) 

# write.csv(tab1_names, './output/names_var_Pacient.csv', row.names = F)

# 3.2 Plan: Y25M02D23_BD - Sheet: Master RCE Info ---------------------------------------------------------

BD_RCE <- read_excel("./input/Y25M02D23_BD.xlsx", sheet = 'Master RCE Info')

tab2_names <- 
  data.frame(Names = names(BD_RCE),
             Code_Names = janitor::make_clean_names(names(BD_RCE)))

data2 <- BD_RCE 
names(data2) <- tab2_names$Code_Names

data2_filter <- 
  data2 %>% 
  dplyr::filter(patient_id != '52') %>% 
  dplyr::mutate(
    goal_target_post_rce_hct_percent = as.numeric(goal_target_post_rce_hct_percent),
    post_rce_hct_percent = as.numeric(post_rce_hct_percent),
    dhtr_likelihood_nomogram_results = dhtr_likelihood_nomogram_results %>% as.factor() %>% forcats::fct_na_level_to_value('N/A')
  ) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ .x %>% stringr::str_to_lower())) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ if_else(.x == 'not assessed', NA, .x))) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ .x %>% as.numeric()))

tab2_names <- 
  tab2_names %>% 
  tibble::add_row(
    Names = 'difference between procedure times',
    Code_Names = 'DIFF_Between_RCE'
  )

# write.csv(tab2_names, './output/names_var_RCE.csv', row.names = F)

# 3.3 Plan: DataAdditional - Sheet: ED Coding --------------------------------------------------------
# Emergency Department (ED)
BD_EDCodding <- read_excel("./input/DataAdditional.xlsx", sheet = "ED Coding") 

tab6_names <- 
  data.frame(Names = names(BD_EDCodding),
             Code_Names = janitor::make_clean_names(names(BD_EDCodding)))

names(BD_EDCodding) <- tab6_names$Code_Names

data_addi2 <- 
  BD_EDCodding %>% 
  dplyr::mutate(data_of_rce = lubridate::ymd(data_of_rce)) %>% 
  tidyr::fill(patient_id, .direction = 'down') %>% 
  dplyr::mutate(across(!c(patient_id, data_of_rce), ~ .x %>% as.character())) %>% 
  tidyr::pivot_longer(!c(patient_id, data_of_rce)) %>% 
  dplyr::mutate(value = if_else(value == 'TRUE', '1', value)) 


# 3.4 Plan: Y25M02D23_BD - Sheet: Hospital Admissions ----------------------------------------------------

BD4_HAC <- read_excel("./input/Y25M02D23_BD.xlsx", sheet = 'Hospital Admissions Coding')

tab4_names <- 
  data.frame(Names = names(BD4_HAC),
             Code_Names = janitor::make_clean_names(names(BD4_HAC)))

data4_HAC <- BD4_HAC 
names(data4_HAC) <- tab4_names$Code_Names

data4_filter <- 
  data4_HAC %>% 
  tidyr::fill(patient_id, .direction = 'down') %>% 
  dplyr::filter(patient_id != '52')


# 3.5 Plan: DataAdditional - Sheet: Age Unit -----------------------------------------------------------------

### Age Unit

DataAdditional <- 
  read_excel("./input/DataAdditional.xlsx", sheet = "Age Unit") %>% 
  dplyr::mutate(TMS_DATEOFTRNSFSN = lubridate::ymd(TMS_DATEOFTRNSFSN))

tab5_names <- 
  data.frame(Names = names(DataAdditional),
             Code_Names = janitor::make_clean_names(names(DataAdditional)))

data_addi <- DataAdditional
names(data_addi) <- tab5_names$Code_Names

# 4. Import End --------------------------------------------------------------------------------------------

envir_end <- c('my_stats', 'wyh_theme',
               'data1_filter', 'data2_filter', 'data4_filter', 'data_addi2') 

# rm(list = setdiff(ls(), envir_end))






































