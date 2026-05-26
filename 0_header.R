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
    goal_target_post_rce_hb_s_percent = as.numeric(goal_target_post_rce_hb_s_percent),
    post_rce_hct_percent = as.numeric(post_rce_hct_percent),
    dhtr_likelihood_nomogram_results = dhtr_likelihood_nomogram_results %>% as.factor() %>% forcats::fct_na_level_to_value('N/A')
  ) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ .x %>% stringr::str_to_lower())) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ if_else(.x == 'not assessed', NA, .x))) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ .x %>% as.numeric()))  %>% 
  dplyr::group_by(patient_id) %>% 
  tidyr::drop_na(pre_a_percent, post_a_percent) %>% 
  dplyr::mutate(RCE_Procedure = row_number()) %>%
  dplyr::mutate(RCE_Procedure = paste('Procedure', RCE_Procedure, sep = '')) %>% 
  dplyr::mutate(RCE_Procedure = factor(RCE_Procedure, 
                                       levels = paste('Procedure', 1:53, sep = ''))) %>% 
  dplyr::mutate(DIFF_Between_RCE = as.numeric(date_of_rce - lag(date_of_rce))) %>%  #cálculo entre data de RCE para cada paciente
  # Calculate Delta HbA (%) ====
dplyr::mutate(
  DeltaHbA = ((post_a_percent - lead(pre_a_percent))/lead(DIFF_Between_RCE))*28
) %>% 
  dplyr::ungroup()

data2_filter %>% 
  dplyr::filter(alloab_1_yes_0_no == 1)


# Calculated RBC Alloantibodies -------------------------------------------------------------------------
calc_RBC_Alloantibodies <- 
  data2_filter %>% 
  dplyr::select(patient_id, 
                pre_rce_type_screen_new_rbc_alloantibodies) %>% 
  dplyr::group_by(patient_id) %>% 
  dplyr::mutate(
    ID_RBC_Alloantibodies = if_else(pre_rce_type_screen_new_rbc_alloantibodies == 'Yes',
                                    1, 0)
  ) %>% 
  dplyr::summarise(
    Count_Yes = sum(ID_RBC_Alloantibodies)
  ) %>% 
  dplyr::mutate(
    Identif_RBC_Alloantiboies = if_else(Count_Yes != 0, 'Yes', 'No')
  )

data1_filter <- 
  data1_filter %>% 
  dplyr::left_join(calc_RBC_Alloantibodies,
                   by = join_by(patient_1 == patient_id)) %>% 
  #new variable
  dplyr::mutate(
    rbc_antibodies_new = case_when(
      (h_o_rbc_alloantibodies == 'Yes' | h_o_rbc_autoantibodies == 'Yes') & Identif_RBC_Alloantiboies == 'No' ~ 'Group 1',
      h_o_rbc_alloantibodies == 'No' & h_o_rbc_autoantibodies == 'No' & Identif_RBC_Alloantiboies == 'Yes' ~ 'Group 2',
      (h_o_rbc_alloantibodies == 'Yes' | h_o_rbc_autoantibodies == 'Yes') & Identif_RBC_Alloantiboies == 'Yes' ~ 'Group 3',
      h_o_rbc_alloantibodies == 'No' & h_o_rbc_autoantibodies == 'No' & Identif_RBC_Alloantiboies == 'No' ~ 'Group 4',
    )
  ) 

# data2_filter %>% 
#   dplyr::filter(patient_id == 12) %>% 
#   dplyr::select(patient_id, RCE_Procedure, DeltaHbA) %>% 
#   View()

tab2_names <- 
  tab2_names %>% 
  tibble::add_row(
    Names = 'difference between procedure times',
    Code_Names = 'DIFF_Between_RCE'
  )


var_selected <- c(
  'pre_rce_hb_g_d_l', # Pre-RCE Hb (g/dL)
  'pre_rce_hb_a1_percent', #Pre-RCE HbA1 (%)
  'pre_rce_hb_a2_percent', #RCE HbA2 (%)
  'pre_rce_hb_c_percent', #Pre-RCE HbC (%)
  
  'post_rce_hb_g_d_l', #Post-RCE Hb (g/dL)
  'post_rce_hb_a1_percent', #Post-RCE HbA1 (%)
  'post_rce_hb_a2_percent', #Post-RCE HbA2 (%)
  'post_rce_hb_c_percent' #Post-RCE HbC (%)
)

data2_filter_gdL <- 
  data2 %>% 
  dplyr::filter(patient_id != '52') %>% 
  dplyr::mutate(
    goal_target_post_rce_hct_percent = as.numeric(goal_target_post_rce_hct_percent),
    goal_target_post_rce_hb_s_percent = as.numeric(goal_target_post_rce_hb_s_percent),
    post_rce_hct_percent = as.numeric(post_rce_hct_percent),
    dhtr_likelihood_nomogram_results = dhtr_likelihood_nomogram_results %>% as.factor() %>% forcats::fct_na_level_to_value('N/A')
  ) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ .x %>% stringr::str_to_lower())) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ if_else(.x == 'not assessed', NA, .x))) %>% 
  dplyr::mutate(across(c(pre_a_percent, post_a_percent), ~ .x %>% as.numeric()))  %>% 
  dplyr::group_by(patient_id) %>% 
  tidyr::drop_na(pre_a_percent, post_a_percent) %>% 
  dplyr::mutate(RCE_Procedure = row_number()) %>%
  dplyr::mutate(RCE_Procedure = paste('Procedure', RCE_Procedure, sep = '')) %>% 
  dplyr::mutate(RCE_Procedure = factor(RCE_Procedure, 
                                       levels = paste('Procedure', 1:53, sep = ''))) %>% 
  dplyr::mutate(DIFF_Between_RCE = as.numeric(date_of_rce - lag(date_of_rce))) %>%  #cálculo entre data de RCE para cada paciente
  # Calculate Delta HbA (g/DL) ====
dplyr::mutate(across(c(var_selected),
                     ~ .x %>% as.numeric())) %>%
  dplyr::mutate(
    HbA_gdL_Pre = pre_rce_hb_g_d_l * (pre_rce_hb_a1_percent/100 + pre_rce_hb_a2_percent/100 + pre_rce_hb_c_percent/100),
    HbA_gdL_Post = post_rce_hb_g_d_l * (post_rce_hb_a1_percent/100 + post_rce_hb_a2_percent/100 + post_rce_hb_c_percent/100)
  ) %>% 
  dplyr::mutate(
    # cálculo do Delta HbA por g/dL
    DeltaHbA_gdL = ((HbA_gdL_Post - lead(HbA_gdL_Pre))/lead(DIFF_Between_RCE))*28
  ) %>% 
  dplyr::ungroup() 

data2_filter_gdL %>% 
  dplyr::filter(patient_id == 12) %>% 
  dplyr::select(patient_id, RCE_Procedure, DeltaHbA_gdL) %>% 
  View()


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
  dplyr::mutate(value = if_else(value == 'TRUE', '1', value)) %>% 
  dplyr::mutate(value = if_else(value %in% c('10', '10a', '10b', '10c', '10d', '10e', '10f', '10g', '11', '12'), 
                                NA, value)) 

# 3.4 Plan: DataAdditional - Sheet: Hospital Admission ---------------------------------------------------
# Hospital Admission
BD_HosAdm <- read_excel("./input/DataAdditional.xlsx", sheet = "Hospital Admission") 

tab7_names <- 
  data.frame(Names = names(BD_HosAdm),
             Code_Names = janitor::make_clean_names(names(BD_HosAdm)))

names(BD_HosAdm) <- tab7_names$Code_Names

data_Hos_Admi <- 
  BD_HosAdm %>% 
  dplyr::mutate(data_of_rce = lubridate::ymd(data_of_rce)) %>% 
  tidyr::fill(patient_id, .direction = 'down') %>% 
  dplyr::mutate(across(!c(patient_id, data_of_rce), ~ .x %>% as.character())) %>% 
  tidyr::pivot_longer(!c(patient_id, data_of_rce)) %>% 
  dplyr::mutate(value = if_else(value == 'TRUE', '1', value)) %>% 
  dplyr::mutate(value = if_else(value %in% c('10', '10a', '10b', '10c', '10d', '10e', '10f', '10g', '11', '12'), 
                                NA, value)) 

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

data_inner_age <- 
  data_addi %>% 
  dplyr::right_join(data2_filter %>% 
                      dplyr::select(patient_id, date_of_rce),
                    by = join_by(tms_patnum == patient_id,
                                 tms_dateoftrnsfsn == date_of_rce)) 

# 4. Import End --------------------------------------------------------------------------------------------

envir_end <- c('my_stats', 'wyh_theme',
               'data1_filter', 'data2_filter', 'data4_filter', 'data_addi2') 

# rm(list = setdiff(ls(), envir_end))






































