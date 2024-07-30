
## Clear environment, if needed
rm(list = ls())
setwd("/Users/irmasirutyte/Desktop/SEIS 2024/Hungary")


## Libraries
library(haven)
library(tidyverse)
library(readxl)
library(srvyr)
library(ggplot2)
library(labelled)
library(remotes)
library(dm)
library(janitor)
library(dplyr)
library(DiagrammeR)
library(Hmisc)
library(xlsx)
library(writexl)
library(expss)
library(unhcrthemes)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(readxl)

library(robotoolbox)
library(labelled)


# Hungary
df_hh_hungary <- read.xlsx("Hungary_-_Socio-Economic_Insights_Survey_SEIS_2024_-_latest_version_-_False_-_2024-07-02-09-30-08_RAW_DATA_v009.xlsx", sheet = "Hungary - Socio-Economic Ins...")
df_hh_hungary$country <- "Hungary"
df_hh_hungary$unique_hh_index <- paste(df_hh_hungary$country, df_hh_hungary$"_index", sep = "_")

view(df_hh_hungary)

df_ind_hungary <- read.xlsx("Hungary_-_Socio-Economic_Insights_Survey_SEIS_2024_-_latest_version_-_False_-_2024-07-02-09-30-08_RAW_DATA_v009.xlsx", sheet = "Info")
df_ind_hungary$country <- "Hungary"
df_ind_hungary$unique_ind_index <- paste(df_ind_hungary$country, df_ind_hungary$"_index", sep = "_")



# remove hungary - specific columns
df_hh_hungary <- df_hh_hungary %>%
  select(-c(
    INT04_SS_LOC_ADM1_NOTE, 
    DR3_NUM_LANG
  ))



# Convert the column to character type in df_hh_hungary
df_hh_hungary$SHL01_TXT_ACCOM_TYP_OTH <- as.character(df_hh_hungary$SHL01_TXT_ACCOM_TYP_OTH)

# Convert the column to character type in df_hh_hungary
df_hh_hungary$SHL01.1_TXT_ACCOM_ARR_OTH <- as.character(df_hh_hungary$SHL01.1_TXT_ACCOM_ARR_OTH)

df_hh_hungary$PSEA2.1_TXT_REASON_OTH <- as.character(df_hh_hungary$PSEA2.1_TXT_REASON_OTH)



### 1. hungary
# List of columns to convert to character type
columns_to_convert <- c("CP02_TXT_RISK_G_OTH", 
                        "PRT02.3_TXT_REASON_UNABLE_REN_OTH", 
                        "PRT04_TXT_FAMILY_COMP_OTH", 
                        "SE2.11c_TXT_BEN_UKR_OTH", 
                        "SHL01_TXT_ACCOM_TYP_OTH", 
                        "SHL01.1_TXT_ACCOM_ARR_OTH", 
                        "PRT04_SM_FAMILY_COMP_CHALLENGES",
                        "EOS5A_SS_INTENTION_OTH",
                        "SHL06.1_TXT_REASON_MOVE_OTHER")

# Convert specified columns to character type
df_hh_hungary <- df_hh_hungary %>%
  mutate(across(all_of(columns_to_convert), as.character))


################################################################################   
##############             APPEND THE DATASET                     ##############    
################################################################################                 

df_hh_appended <- df_hh_hungary

library(quantmod)

view(df_hh_appended)


df_hh_appended$today <- openxlsx::convertToDate(df_hh_appended$today)
df_hh_appended$EOS2_DATE_RECENT_VISIT <- openxlsx::convertToDate(df_hh_appended$EOS2_DATE_RECENT_VISIT)

view(df_hh_appended)


# Remove specified columns from df_hh_appended
df_hh_appended <- df_hh_appended %>%
  select(-c(
    start,
    end,
    deviceid,
    audit,
    INT00_NOTE_CON,
    INT01_SS_CON_NOTE,
    '_submission_time',
    '_validation_status',
    '_notes',
    '_status',
    '_submitted_by',
    '__version__',
    '_tags',
    INT02.1_SS_AGE_NOTE,
    note_hh_members,
    SHL08_NOTE_WNTR,
    GBV01_NOTE,
    '_uuid',
    SE.2.0_NUM_HH_EXP,	
    SE.2.0_NUM_HH_EXP_check
  ))


write.xlsx(df_hh_appended, "hungary_household_data.xlsx")



################################################################################
### INDIVIDUAL
################################################################################

df_ind_appended <- df_ind_hungary

df_ind_appended$unique_hh_index <- paste(df_ind_appended$country, df_ind_appended$"_parent_index", sep = "_")



# Remove specified columns 
df_ind_appended <- df_ind_appended %>%
  select(-c(
    '_parent_table_name',
    '_submission__uuid',
    '_submission__submission_time',	
    '_submission__validation_status',	
    '_submission__notes',	
    '_submission__status',	
    '_submission__submitted_by',
    '_submission___version__',	
    '_submission__tags'
  ))


write.xlsx(df_ind_appended, "hungary_individual_data.xlsx")



# ------------------------------------------------------------------------------
# CALCULATE INDICATORS
# ------------------------------------------------------------------------------

# Filter rows where INT01_SS_CON is "yes"
df_hh_appended <- df_hh_appended %>%
  filter(INT01_SS_CON == "yes")


# ------------------------------------------------------------------------------
# DISABILITY
# ------------------------------------------------------------------------------

# WG.1.1_SS_DIFF_SEE
# WG.1.2_SS_DIFF_HEAR
# WG.1.3_SS_DIFF_WALK
# WG.1.4_SS_DIFF_REM
# WG.1.5_SS_DIFF_DRESS
# WG.1.6_SS_DIFF_COMM

df_ind_appended <- df_ind_appended %>%
  mutate( # disability identifier variables according to Washington Group standards
    disaux1_34 = WG.1.1_SS_DIFF_SEE %in% c("lot_difficulty", "cannot_all"), # indicator variables for all 6 domains with value TRUE if A LOT OF DIFFICULTY or CANNOT DO AT ALL
    disaux2_34 = WG.1.2_SS_DIFF_HEAR %in% c("lot_difficulty", "cannot_all"),
    disaux3_34 = WG.1.3_SS_DIFF_WALK %in% c("lot_difficulty", "cannot_all"),
    disaux4_34 = WG.1.4_SS_DIFF_REM %in% c("lot_difficulty", "cannot_all"),
    disaux5_34 = WG.1.5_SS_DIFF_DRESS %in% c("lot_difficulty", "cannot_all"),
    disaux6_34 = WG.1.6_SS_DIFF_COMM %in% c("lot_difficulty", "cannot_all")
  ) %>%
  mutate(
    disSum34 = rowSums(select(., disaux1_34, disaux2_34, disaux3_34, disaux4_34, disaux5_34, disaux6_34)) # count number of TRUE indicator variables over 6 domains
  ) %>%
  mutate(
    DISABILITY3 = case_when( # the level of inclusion is at least one domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL.
      disSum34 >= 1 ~ 1,
      disSum34 == 0 & (!(WG.1.1_SS_DIFF_SEE %in% c("do_not_know", "prefer_not_to_answer") & WG.1.2_SS_DIFF_HEAR %in% c("do_not_know", "prefer_not_to_answer") & WG.1.3_SS_DIFF_WALK %in% c("do_not_know", "prefer_not_to_answer") & WG.1.4_SS_DIFF_REM %in% c("do_not_know", "prefer_not_to_answer") & WG.1.5_SS_DIFF_DRESS %in% c("do_not_know", "prefer_not_to_answer") & WG.1.6_SS_DIFF_COMM %in% c("do_not_know", "prefer_not_to_answer"))) ~ 0,
      WG.1.1_SS_DIFF_SEE %in% c("do_not_know", "prefer_not_to_answer") & WG.1.2_SS_DIFF_HEAR %in% c("do_not_know", "prefer_not_to_answer") & WG.1.3_SS_DIFF_WALK %in% c("do_not_know", "prefer_not_to_answer") & WG.1.4_SS_DIFF_REM %in% c("do_not_know", "prefer_not_to_answer") & WG.1.5_SS_DIFF_DRESS %in% c("do_not_know", "prefer_not_to_answer") & WG.1.6_SS_DIFF_COMM %in% c("do_not_know", "prefer_not_to_answer") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY3 = labelled(DISABILITY3,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 3"))

# Calculate having at least one disability identifier among 4 categories

df_ind_appended <- df_ind_appended %>%
  mutate(calc_disability = case_when(DISABILITY3 == 1 ~ 1, 
                                TRUE ~ 0)
  ) %>%
  mutate(calc_disability = labelled(calc_disability,
                               labels = c(
                                 "Without disability" = 0,
                                 "With disability" = 1)
  ))

table(df_ind_appended$calc_disability)

round(prop.table(table(df_ind_appended$calc_disability)), 2)

# df_ind_appended %>% group_by(disability) %>% filter(!is.na(disability)) %>% 
#  summarise(n = sum(Weight)) %>%
#  mutate(pct = n / sum(n),
#         pctlabel = paste0(round(pct*100), "%"))

df_ind_appended <- df_ind_appended %>%
  select(
    -disaux1_34, -disaux2_34, -disaux3_34, -disaux4_34, -disaux5_34, -disaux6_34, -disSum34, -DISABILITY3
  )

# -----------------------------------------------------------------------------
# LIVELIHOOD COPING STRATEGIES INDEX
# -----------------------------------------------------------------------------

# variable labels 
# L1_SS_BASIC
# L2_SS_ASSETS
# L3_SS_FOOD_CREDIT
# L4_SS_SELL_ASSETS
# L5_SS_REDUCED_HLTH_EXP
# L6_SS_REDUCED_EDU_EXP
# L7_SS_OUT_SCH
# L8_SS_SELL_HSE_LND
# L9_SS_MIGR
# L10_SS_CHL_LAB
# L11_SS_DEGR_INCM


# yes	Yes
# no_not_needed	No, didn't need to apply this coping strategy

# no_already_credit	No, because we already purchased food on credit or borrowed food and cannot continue to do it.
# no_reduced_hlt_exp	No, because we already reduced essential health expenditures within the last 12 months and cannot continue to do it
# no_reduced_edu_exp	No, because we already reduced essential education expenditures within the last 12 months and cannot continue to do it
# no_out_of_school	No, because we already withdrew school-aged children from school within the last 12 months
# no_migrated	No, because we already migrated/became displaced within the last 12 months
# no_school_aged	No, because we already involved school aged children in this activity in the last 12 months
# no_high_risk	No, because we already have engaged in high-risk or dangerous work/job within the last 12 months and cannot continue to do it
# no_assets_sold	No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it

# not_applicable	Not applicable (don’t have access to this strategy)
# do_not_know	Don't know

# Specify the columns and old response options
columns_to_rename <- c(
  "L1_SS_BASIC",
  "L2_SS_ASSETS",
  "L3_SS_FOOD_CREDIT",
  "L4_SS_SELL_ASSETS",
  "L5_SS_REDUCED_HLTH_EXP",
  "L6_SS_REDUCED_EDU_EXP",
  "L7_SS_OUT_SCH",
  "L8_SS_SELL_HSE_LND",
  "L9_SS_MIGR",
  "L10_SS_CHL_LAB",
  "L11_SS_DEGR_INCM"
)

old_responses <- c(
  "no_already_credit",
  "no_reduced_hlt_exp",
  "no_reduced_edu_exp",
  "no_out_of_school",
  "no_migrated",
  "no_school_aged",
  "no_high_risk",
  "no_assets_sold"
)

# Rename the responses in the specified columns
df_hh_appended <- df_hh_appended %>%
  mutate(across(all_of(columns_to_rename), ~ ifelse(. %in% old_responses, "no_already_done", .)))

# Check the results
head(df_hh_appended)


# ------------------------------------------------------------------------------
# STRESS
# ------------------------------------------------------------------------------

var_label(df_hh_appended$L9_SS_MIGR) <- "Entire household migrated/displaced due to a lack of resources to cover basic needs"
var_label(df_hh_appended$L1_SS_BASIC) <- "Spent savings to meet essential needs"
var_label(df_hh_appended$L2_SS_ASSETS) <- "Sell household assets/goods (radio, furniture, television, jewellery etc.) to meet basic needs"
var_label(df_hh_appended$L3_SS_FOOD_CREDIT) <- "Purchase food on credit or borrowed food due to a lack of resources to cover basic needs"

# CRISIS
var_label(df_hh_appended$L4_SS_SELL_ASSETS) <- "Sell productive assets or means of transport (sewing machine, bicycle, car, etc.) due to a lack of resources to cover basic needs"
var_label(df_hh_appended$L7_SS_OUT_SCH) <- "Withdrew school-aged children from school because of a lack of food or money to buy food"
var_label(df_hh_appended$L5_SS_REDUCED_HLTH_EXP) <- "Reduce essential health expenditures (including drugs) due to a lack of resources to cover basic needs"
var_label(df_hh_appended$L6_SS_REDUCED_EDU_EXP) <- "Reduce essential education expenditures due to a lack of resources to cover basic needs"

# EMERGENCY
var_label(df_hh_appended$L8_SS_SELL_HSE_LND) <- "Sell house or land (including inside Ukraine) to cover basic needs"
var_label(df_hh_appended$L11_SS_DEGR_INCM) <- "Use degrading sources of income, illegal work, or high-risk jobs to cover basic needs"
var_label(df_hh_appended$L10_SS_CHL_LAB) <- "Involve school-aged children in income generation to meet basic needs"

view(df_hh_appended)

df_hh_appended <- df_hh_appended %>%
  mutate(across(c(L9_SS_MIGR, L1_SS_BASIC, L2_SS_ASSETS, L3_SS_FOOD_CREDIT, L4_SS_SELL_ASSETS, L7_SS_OUT_SCH, L5_SS_REDUCED_HLTH_EXP, L6_SS_REDUCED_EDU_EXP, L8_SS_SELL_HSE_LND, L11_SS_DEGR_INCM, L10_SS_CHL_LAB), ~
                  case_when(
                    . == "yes" ~ 30,
                    . == "no_not_needed" ~ 10,
                    . == "no_already_done" ~ 20,
                    . == "not_applicable" ~ 999, # to make it compatible with previous year, I will not change the code
                    . == "do_not_know" ~ 999, # to make it compatible with previous year, I will not change the code
                    TRUE ~ as.numeric(.)
                  )
  ))  


#write.xlsx(df_hh_appended, "check_lcsi_data.xlsx")


# Display columns
selected_columns <- df_hh_appended %>%
  select(L9_SS_MIGR, L1_SS_BASIC, L2_SS_ASSETS, L3_SS_FOOD_CREDIT)

# Print the selected columns
print(selected_columns)

# Create a variable to specify if the household used any of the strategies by severity

# Stress
df_hh_appended <- df_hh_appended %>% mutate(calc_stress_coping_EN = case_when(
  L9_SS_MIGR == 20 | L9_SS_MIGR == 30 ~ 1,
  L1_SS_BASIC == 20 | L1_SS_BASIC == 30 ~ 1,
  L2_SS_ASSETS == 20 | L2_SS_ASSETS == 30 ~ 1,
  L3_SS_FOOD_CREDIT == 20 | L3_SS_FOOD_CREDIT == 30 ~ 1,
  TRUE ~ 0))
var_label(df_hh_appended$calc_stress_coping_EN) <- "Did the HH engage in stress coping strategies"

# Crisis
df_hh_appended <- df_hh_appended %>% mutate(calc_crisis_coping_EN = case_when(
  L4_SS_SELL_ASSETS == 20 | L4_SS_SELL_ASSETS == 30 ~ 1,
  L7_SS_OUT_SCH == 20 | L7_SS_OUT_SCH == 30 ~ 1,
  L5_SS_REDUCED_HLTH_EXP == 20 | L5_SS_REDUCED_HLTH_EXP == 30 ~ 1,
  L6_SS_REDUCED_EDU_EXP == 20 | L6_SS_REDUCED_EDU_EXP == 30 ~ 1,
  TRUE ~ 0))
var_label(df_hh_appended$calc_crisis_coping_EN) <- "Did the HH engage in crisis coping strategies"

# Emergency
df_hh_appended <- df_hh_appended %>% mutate(calc_emergency_coping_EN = case_when(
  L8_SS_SELL_HSE_LND == 20 | L8_SS_SELL_HSE_LND == 30 ~ 1,
  L11_SS_DEGR_INCM == 20 | L11_SS_DEGR_INCM == 30 ~ 1,
  L10_SS_CHL_LAB == 20 | L10_SS_CHL_LAB == 30 ~ 1,
  TRUE ~ 0))
var_label(df_hh_appended$calc_emergency_coping_EN) <- "Did the HH engage in emergency coping strategies"


# Calculate Max_coping_behaviour
df_hh_appended <- df_hh_appended %>% mutate(calc_Max_coping_behaviourEN = case_when(
  calc_emergency_coping_EN == 1 ~ 4,
  calc_crisis_coping_EN == 1 ~ 3,
  calc_stress_coping_EN == 1 ~ 2,
  TRUE ~ 1))
var_label(df_hh_appended$calc_Max_coping_behaviourEN) <- "Summary of asset depletion"
val_lab(df_hh_appended$calc_Max_coping_behaviourEN) = num_lab("
             1 HH not adopting coping strategies
             2 Stress coping strategies
             3 Crisis coping strategies
             4 Emergencies coping strategies
")

round(prop.table(table(df_hh_appended$calc_Max_coping_behaviourEN)), 2)

# df_hh_appended %>% group_by(Max_coping_behaviourEN) %>% filter(!is.na(Max_coping_behaviourEN)) %>% 
#  summarise(n = sum(Weight)) %>%
#  mutate(pct = n / sum(n),
#         pctlabel = paste0(round(pct*100), "%"))


# -----------------------------------------------------------------------------
# REDUCED COPING STRATEGIES INDEX
# -----------------------------------------------------------------------------

# FS.3.1_NUM_COPE - less preferred food
# FS.3.2_NUM_BURROW - borrow
# FS.3.3_NUM_LIM - limit portion size
# FS.3.4_NUM_LACK - restrict consumption by adults in order for small children to eat
# FS.3.5_NUM_REDUCED - reduce number of meals eaten

# assign variable and value labels
var_label(df_hh_appended$FS.3.1_NUM_COPE) <-  "Rely on less preferred and less expensive food in the past 7 days"
var_label(df_hh_appended$FS.3.2_NUM_BURROW) <- "Borrow food or rely on help from a relative or friend in the past 7 days"
var_label(df_hh_appended$FS.3.5_NUM_REDUCED) <-  "Reduce number of meals eaten in a day in the past 7 days"
var_label(df_hh_appended$FS.3.3_NUM_LIM) <- "Limit portion size of meals at meal times in the past 7 days"
var_label(df_hh_appended$FS.3.4_NUM_LACK) <-  "Restrict consumption by adults in order for small children to eat in the past 7 days"

# Replace multiple specific numeric values (e.g., 999 and 888) with NA in multiple columns
df_hh_appended <- df_hh_appended %>%
  mutate(across(c(FS.3.1_NUM_COPE, FS.3.2_NUM_BURROW, FS.3.5_NUM_REDUCED,FS.3.3_NUM_LIM, FS.3.4_NUM_LACK), ~ replace(., . %in% c(9999, 8888), NA)))


# Display columns 
selected_columns <- df_hh_appended %>%
  select(FS.3.1_NUM_COPE, FS.3.2_NUM_BURROW, FS.3.5_NUM_REDUCED, FS.3.3_NUM_LIM,FS.3.4_NUM_LACK)

# Print the selected columns
print(selected_columns)


# calculate reduced Coping Strategy Index (rCSI)
df_hh_appended <- df_hh_appended %>% mutate(calc_rCSI = FS.3.1_NUM_COPE + (2 * FS.3.2_NUM_BURROW) + FS.3.5_NUM_REDUCED + FS.3.3_NUM_LIM + (3 * FS.3.4_NUM_LACK))
var_label(df_hh_appended$calc_rCSI) <- "Reduced coping strategies index (rCSI)"


# Unweighted
rCSI_table_mean <- df_hh_appended %>% 
  drop_na(calc_rCSI) %>% 
  summarise(meanrCSI = mean(calc_rCSI))


# Weighted
# rCSI_table_mean <- df_hh_appended %>% 
# drop_na(rCSI) %>% 
# summarise(meanrCSI =  weighted.mean(rCSI,Weight))


# -----------------------------------------------------------------------------
# EXPENDITURE
# -----------------------------------------------------------------------------

# SE.2.1_NUM_FOOD
# SE.2.2_NUM_ACCOM
# SE.2.6_NUM_HH_BILL
# SE.2.3_NUM_HLTH
# SE.2.4_NUM_HYG
# SE.2.5_NUM_COMM
# SE.2.7_NUM_OTH

# SE.2.8_NUM_HLTH_6_MTH - 6 MONTHS
# SE.2.9_NUM_DEBT - 6 MONYHS 
# SE.2.10_NUM_EDU - 12 MONTHS


# Replace 9999 and 99999 with NA in multiple columns
df_hh_appended <- df_hh_appended %>%
  mutate(across(c(SE.2.1_NUM_FOOD, SE.2.2_NUM_ACCOM,SE.2.3_NUM_HLTH, SE.2.4_NUM_HYG, 
                  SE.2.5_NUM_COMM, SE.2.6_NUM_HH_BILL, SE.2.7_NUM_OTH, SE.2.8_NUM_HLTH_6_MTH, 
                  SE.2.9_NUM_DEBT, SE.2.10_NUM_EDU), ~ replace(., . %in% c(9999, 99999, 999999, 9999999), NA)))


# Check the type of the L9_SS_MIGR column
column_type <- typeof(df_hh_appended$SE.2.8_NUM_HLTH_6_MTH)

# Print the result
print(column_type)



# HEALTHCARE EXPENDITURE 6 MONTHS - CONVERT TO 1 MONTH

df_hh_appended$SE.2.8_NUM_HLTH_6_MTH <- as.numeric(as.character(df_hh_appended$SE.2.8_NUM_HLTH_6_MTH))
df_hh_appended$SE.2.9_NUM_DEBT <- as.numeric(as.character(df_hh_appended$SE.2.8_NUM_HLTH_6_MTH))
df_hh_appended$SE.2.10_NUM_EDU <- as.numeric(as.character(df_hh_appended$SE.2.8_NUM_HLTH_6_MTH))


# df_hh_appended$SE.2.8_NUM_HLTH_6_MTH  <- as.numeric(df_hh_appended$SE.2.0_NUM_HH_EXP)
df_hh_appended$SE.2.8_NUM_HLTH_6_MTH_30_days  <- df_hh_appended$SE.2.8_NUM_HLTH_6_MTH  / 6

# DEBT EXPENDITURE 6 MONTHS - CONVERT TO 1 MONTH
df_hh_appended$SE.2.9_NUM_DEBT_30_days  <- df_hh_appended$SE.2.9_NUM_DEBT  / 6

# EDUCATION EXPENDITURE 12 MONTHS - CONVERT TO 1 MONTH
df_hh_appended$SE.2.10_NUM_EDU_30_days  <- df_hh_appended$SE.2.10_NUM_EDU  / 12


# Select the columns you want to sum
columns_to_sum <- c(
  "SE.2.1_NUM_FOOD", "SE.2.2_NUM_ACCOM", "SE.2.3_NUM_HLTH",
  "SE.2.4_NUM_HYG", "SE.2.5_NUM_COMM", "SE.2.6_NUM_HH_BILL",
  "SE.2.7_NUM_OTH", "SE.2.8_NUM_HLTH_6_MTH_30_days", "SE.2.9_NUM_DEBT_30_days",
  "SE.2.10_NUM_EDU_30_days"
)

# Create a logical vector indicating if NAs are present in the specified columns
na_in_se_columns <- rowSums(is.na(df_hh_appended[, c("SE.2.1_NUM_FOOD", "SE.2.2_NUM_ACCOM", "SE.2.3_NUM_HLTH")])) > 0


# Convert the selected columns to numeric
df_hh_appended[columns_to_sum] <- lapply(df_hh_appended[columns_to_sum], function(x) as.numeric(as.character(x)))


# Use rowSums to sum the selected columns while ignoring missing values
df_hh_appended$calc_hh_total_expenditure <- round(rowSums(df_hh_appended[columns_to_sum], na.rm = TRUE), 2)

# Assign NA to total_expenditure when NAs are present in the specified columns
df_hh_appended$calc_hh_total_expenditure[na_in_se_columns] <- NA



# 1. SHARE OF EXPENDITURE ON FOOD
df_hh_appended$calc_share_food_expenditure <- round(df_hh_appended$SE.2.1_NUM_FOOD / df_hh_appended$calc_hh_total_expenditure,2)

df_hh_appended %>% summarise(average = mean(calc_share_food_expenditure, na.rm = T))

# 2. SHARE OF EXPENDITURE ON ACCOMMODATION
df_hh_appended$calc_share_accomm_expenditure <- round(df_hh_appended$SE.2.2_NUM_ACCOM / df_hh_appended$calc_hh_total_expenditure,2)

# 3. SHARE OF EXPENDITURE ON HEALTH
df_hh_appended$calc_health_expenditure = df_hh_appended$SE.2.3_NUM_HLTH + df_hh_appended$SE.2.8_NUM_HLTH_6_MTH_30_days
df_hh_appended$calc_share_health_expenditure <- round(df_hh_appended$calc_health_expenditure / df_hh_appended$calc_hh_total_expenditure,2)

# 4. SHARE OF EXPENDITURE ON HYGIENE
df_hh_appended$calc_share_hygiene_expenditure <- round(df_hh_appended$SE.2.4_NUM_HYG / df_hh_appended$calc_hh_total_expenditure,2)

# 5. SHARE OF EXPENDITURE ON COMMUNICATION
df_hh_appended$calc_share_communication_expenditure <- round(df_hh_appended$SE.2.5_NUM_COMM / df_hh_appended$calc_hh_total_expenditure,2)

# 6. SHARE OF EXPENDITURE ON HH BILLS
df_hh_appended$calc_share_hh_bills_expenditure <- round(df_hh_appended$SE.2.6_NUM_HH_BILL / df_hh_appended$calc_hh_total_expenditure,2)

# 7. SHARE OF EXPENDITURE ON EDUCATION
df_hh_appended$calc_share_education_expenditure <- round(df_hh_appended$SE.2.10_NUM_EDU_30_days / df_hh_appended$calc_hh_total_expenditure,2)

# 8. SHARE OF EXPENDITURE ON DEBT
df_hh_appended$calc_share_debt_expenditure <- round(df_hh_appended$SE.2.9_NUM_DEBT_30_days / df_hh_appended$calc_hh_total_expenditure,2)

# 9. SHARE OF EXPENDITURE ON OTHER
df_hh_appended$calc_share_other_expenditure <- round(df_hh_appended$SE.2.7_NUM_OTH / df_hh_appended$calc_hh_total_expenditure,2)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Calculate quantiles with na.rm = TRUE
quantiles <- quantile(df_hh_appended$calc_hh_total_expenditure, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)

# Assign categories based on quantiles
df_hh_appended$calc_expenditure_quintiles <- cut(df_hh_appended$calc_hh_total_expenditure, 
                                                 breaks = quantiles, 
                                                 labels = c("Very Low", "Low", "Medium", "High", "Very High"), 
                                                 include.lowest = TRUE)

# Print the result
View(df_hh_appended)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#              RMS - RESULTS MONITORING SURVEY INDICATORS - COMPASS
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# # 2.3 Core impact indicator
# Proportion of PoC with access to health services
# ------------------------------------------------------------------------------

# RMS	S7	S7: Access to health services
# RMS	HACC01	1. During the past 30 days, has ${name_individual} consulted a health practitioner, dentist, traditional healer, or pharmacist, or visited a health center?
# RMS	HACC02	2. For what reason(s) did ${name_individual} seek consultation?
# RMS	HACC02_other	If other please specify
# RMS	HACC03	3. During the past 30 days, has ${name_individual} needed health services that’s/he could not have access to?
# RMS	HACC04	4. Why has ${name_individual} been unable to access a medical treatment in the past 30 days?
# RMS	HACC04_other	If other please specify

# MSNA	Access	Access
# MSNA	H1_SS_HLTH_PBLM	In the last month (or since arrival in case less than 30 days since arrival), did this person in your household have a health problem and need to access health care?
# MSNA	H2_SS_HLTH_CHRONIC_ILL	Does the person have a chronic illness?
# MSNA	H3_SS_HLTH_OBTAIN_CARE	Was the person able to obtain the needed health care?
# MSNA	H4_SM_HLTH_ACC_BARRIER	What was the main reason this person was unable to access health care?
# MSNA	H4_TXT_HLTH_ACC_BARRIER_OTHER	Other barriers, please specify

# exclude
# H4_SM_HLTH_ACC_BARRIER_wanted_to_wait_and_see_if_the_problem_go_better
# H4_SM_HLTH_ACC_BARRIER_do_not_trust_local_provider
# H4_SM_HLTH_ACC_BARRIER_fear_or_distrust_of_HW_EXAM_treatment
# H4_SM_HLTH_ACC_BARRIER_other 


# Numerator: Population who have received the asked for health services in the previous 30 days
# Denominator: Total population who have asked for health services in the previous 30 days

# class(df_ind$H4_SM_HLTH_ACC_BARRIER_wanted_to_wait_and_see_if_the_problem_go_better)


df_ind_appended <- df_ind_appended %>% # Those who were able to access healthcare
  mutate(calc_health_access = case_when(
    H3_SS_HLTH_OBTAIN_CARE == "yes"  ~ 1,
    H3_SS_HLTH_OBTAIN_CARE == "do_not_know" ~ NA_real_,
    H3_SS_HLTH_OBTAIN_CARE == "prefer_not_to_answer" ~ NA_real_,
    TRUE ~ 0)
  ) 

df_ind_appended <- df_ind_appended %>% # Those who needed and asked to access healthcare - remove those who didn't try to access healthcare for some reasons
  mutate(calc_health_need = case_when(
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "wanted_to_wait_and_see_if_the_problem_go_better" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "do_not_trust_local_provider" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "fear_or_distrust_of_HW_EXAM_treatment" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" & H3_SS_HLTH_OBTAIN_CARE == "no" & H4_SM_HLTH_ACC_BARRIER == "other" ~ 0, 
    H1_SS_HLTH_PBLM == "yes" ~ 1,
    H1_SS_HLTH_PBLM == "do_not_know" ~ NA_real_,
    H1_SS_HLTH_PBLM == "prefer_not_to_answer" ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(rms_impact2_3_health = calc_health_access / calc_health_need) %>%
  mutate(rms_impact2_3_health = labelled(rms_impact2_3_health,
                                     labels = c (
                                       "Yes"= 1,
                                       "No"= 0
                                     ),
                                     label="Proportion of PoC with access to health services"))


table(df_ind_appended$rms_impact2_3_health)


round(prop.table(table(df_ind_appended$rms_impact2_3_health)), 3)


# write_xlsx(df_ind_appended, "RMS/final_individual_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 3.2a Core impact indicator	
# Proportion of Persons of Concern enrolled in primary education
# ------------------------------------------------------------------------------

# Numerator: Number of students enrolled in primary education (regardless of age)
# Denominator: Total number of individuals of primary school age

# E0_SS_ENROLLED_HC	Is this child/young person attending a school/kindergarten/nursery that is part of the national education systems in [host country] in 2023/2024?
# E1_SS_ATT_EDU_LEVEL	During the 2023-2024 academic year, what level of education is/was this child/young person attending?
# early_child_care
# pre_primary
# primary
# secondary
# secondary_tvet
# tertiary
# do_not_know
# prefer_not_to_answer

# E2_SM_RES_NO_EDU	What are the reasons your child/young person does not attend school/kindergarten/nursery that is part of the national education system in [host country]?
# E0_SS_STILL_ENROLLED	Is/was this child/young person formally enrolled in a school in Ukraine in school year 2023-2024, even when being abroad?
# E3_SS_ENROLL	Will you enroll this child/young person in a school/kindergarten/nursery that is part of the national education systems in [host country] for next year, 2024/2025?
# E6_SS_DIST_LER	Is/was this child/young person learning remotely or on-line in the school year 2023-2024?
# E9_SS_DIST_LER_TYP	What type of remote or on-line learning was the child/young person enrolled in or conducting during the school year 2023-2024?
# E10_SS_LER_UND_SUP	Is/was this child/young person studying under supervision of a teacher or other qualified educator from Ukraine?
# E11_SS_PART_EXAM	Did this child/young person participate in exams, tests or evaluations while learning remotely/online?
# E8_SS_TIME_DIST_LER	How much time does the child/young person spend per day learning remotely/on-line in 2023-2024?
# E7_SS_CONT_DIST_LER	Will this child/young person continue learning remotely/on-line in the school year 2024/2025?


df_ind_appended <- df_ind_appended %>%
  mutate(edu_primary = case_when(
    (E0_SS_ENROLLED_HC == "yes") & (E1_SS_ATT_EDU_LEVEL == "primary") ~ 1,  # attending school in person
    (E0_SS_ENROLLED_HC == "no") ~ 0,  # not enrolled / attending school in host country
    (E0_SS_ENROLLED_HC == "prefer_not_to_answer" | E0_SS_ENROLLED_HC == "do_not_know") ~ NA_real_,
    TRUE ~ NA_real_  # default case, added to handle other cases not covered above
  )) %>%
  mutate(age_primary = case_when(
    DR.11_NUM_AGE >= 6 & DR.11_NUM_AGE <= 14 ~ 1, # general age group for primary education
    TRUE ~ NA_real_)
  ) %>%
  group_by(country) %>%
  mutate(rms_impact3_2b_primary_edu_enrol_rate = sum(edu_primary, na.rm = TRUE) / sum(age_primary, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rms_impact3_2b_primary_edu_enrol_rate = round(rms_impact3_2b_primary_edu_enrol_rate, 2)) %>%
  mutate(rms_impact3_2b_primary_edu_enrol_rate = labelled(
    rms_impact3_2b_primary_edu_enrol_rate,
    labels = c("Yes" = 1, "No" = 0),
    label = "Proportion of persons of concern enrolled in primary education"
  ))

# Optional: Display or check a table of results
table(df_ind_appended$country, df_ind_appended$rms_impact3_2b_primary_edu_enrol_rate)


#write_xlsx(df_ind_appended, "final_individual_indicators_check.xlsx", col_names = TRUE)


# ------------------------------------------------------------------------------
# 3.2b Core impact indicator
# Proportion of Persons of Concern enrolled in secondary education
# ------------------------------------------------------------------------------

# Numerator: Number of students enrolled in secondary education (regardless of age)
# Denominator: Total number of individuals of secondary school age

df_ind_appended <- df_ind_appended %>%
mutate(calc_edu_secondary = case_when(
  (E0_SS_ENROLLED_HC == "yes") & (E1_SS_ATT_EDU_LEVEL == "secondary") ~ 1,  # attending in person
  E0_SS_ENROLLED_HC == "no" ~ 0,  # not attending education at all
  (E0_SS_ENROLLED_HC == "prefer_not_to_answer" | E0_SS_ENROLLED_HC == "do_not_know") ~ NA_real_,
  TRUE ~ NA_real_  # default case, added to handle other cases not covered above
)) %>%
  # Rename age_primary to age_secondary for consistency
  mutate(calc_age_secondary = case_when(
    DR.11_NUM_AGE >= 15 & DR.11_NUM_AGE <= 18 ~ 1, # general age group for secondary (11 - 18)
    TRUE ~ NA_real_)
  ) %>%
  group_by(country) %>%  # Group data by country
  # Perform the calculation within each group
  mutate(
    calc_enrolled_secondary = sum(calc_edu_secondary, na.rm = TRUE),
    calc_eligible_secondary = sum(calc_age_secondary, na.rm = TRUE),
    rms_impact3_2b_secondary_edu_enrol_rate = ifelse(calc_eligible_secondary > 0, calc_enrolled_secondary / calc_eligible_secondary, NA_real_)
  ) %>%
  ungroup()


# To display the table of outcomes by country
table(df_ind_appended$country, df_ind_appended$rms_impact3_2b_secondary_edu_enrol_rate)


#write_xlsx(df_ind_appended, "final_individual_indicators_check_secondary.xlsx", col_names=TRUE)



# ------------------------------------------------------------------------------
# 3.3 Core impact indicator
# Proportion of population that feel safe walking alone in their neighborhood
# ------------------------------------------------------------------------------

# PRT06_SS_SAFETY_LVL	- How safe do you feel walking alone in your area/neighborhood after dark?

df_hh_appended <- df_hh_appended %>%
  mutate(rms_impact3_3_safety_walking = case_when(
    PRT06_SS_SAFETY_LVL =="very_safe" | PRT06_SS_SAFETY_LVL =="fairly_safe" ~ 1,
    PRT06_SS_SAFETY_LVL == "bit_unsafe" | PRT06_SS_SAFETY_LVL == "very_unsafe" ~ 0 , 
    TRUE ~ NA_real_)
  ) %>% 
  mutate(rms_impact3_3_safety_walking = labelled(rms_impact3_3_safety_walking,
                                             labels = c(
                                               "Yes"= 1,
                                               "No"= 0
                                             ),
                                             label="Proportion of population that feel safe walking alone in their neighbourhood"))

table(df_hh_appended$rms_impact3_3_safety_walking)

round(prop.table(table(df_hh_appended$rms_impact3_3_safety_walking)), 2)

# write_xlsx(df_hh_appended, "RMS/final_household_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 1.2 Core outcome indicator
# Proportion of children under 5 years of age whose births have been registered with a civil authority
# ------------------------------------------------------------------------------

# PRT01_SS_ID	Does this individual have the following documents? (enumerators to ask for each)
# tin	Tax Identification Number (TIN / ITN)
# national_id	National ID / ID Card
# internal_passport	Passport of a Citizen of Ukraine / Internal Passport
# non_bio_passport	Valid non-biometric Passport
# bio_passport	Valid Biometric passport
# birth_cert	Birth certificate
# none	Has none of the documents
# prefer_not_to_answer	Prefer not answer
# do_not_know	Do not know

df_ind_appended <- df_ind_appended %>%
  mutate(calc_birth_certificate = case_when(
    ((PRT01_SS_ID_birth_cert == 1 | PRT01_SS_ID_national_id==1 | PRT01_SS_ID_internal_passport ==1 | PRT01_SS_ID_non_bio_passport ==1 | PRT01_SS_ID_bio_passport ==1) & DR.11_NUM_AGE < 5)  ~ 1, 
    (PRT01_SS_ID == "none" & DR.11_NUM_AGE < 5)  ~ 0, 
    TRUE ~ NA_real_)
  ) %>%
  mutate(calc_birth_certificate = labelled(calc_birth_certificate,
                                   labels=c(
                                      'Yes'=1,
                                      'No'=0
                                   ),
                                   label = "Children under 5 with a birth certificate"))


## Calculate children who has been registered with civil authorities

# CP1_SS_BTH_REG	Has this child's birth been registered with civil authorities? (In Ukraine, [host country] or other country)
# in_ukraine	Yes - in Ukraine
# in_this_host_country	Yes - in [this host country]
# in_another_country	Yes - in another country
# no	No
# do_not_know	Do not know
# prefer_not_to_answer	Prefer not answer


df_ind_appended <- df_ind_appended %>%
  mutate(calc_birth_registered = case_when(
    CP1_SS_BTH_REG == "no" ~ 0,
    CP1_SS_BTH_REG == "in_ukraine" | CP1_SS_BTH_REG == "in_this_host_country" | CP1_SS_BTH_REG == "in_another_country" ~ 1,
    CP1_SS_BTH_REG == "prefer_not_to_answer" | CP1_SS_BTH_REG == "do_not_know" ~ NA_real_
  )) %>%
  mutate(calc_birth_registered = labelled(
    calc_birth_registered,
    labels = c(Yes = 1, No = 0),
    label = "Children under 5 birth registered with civil authorities"
  ))

  
df_ind_appended <- df_ind_appended %>%
    mutate(rms_outcome1_2_children_registered = case_when(
      (calc_birth_registered == 1 | calc_birth_certificate  == 1) & DR.11_NUM_AGE < 5 ~ 1, 
      calc_birth_registered == 0 & calc_birth_certificate == 0 & DR.11_NUM_AGE < 5 ~ 0,
      TRUE ~ NA_real_  # Ensure to handle other cases to avoid NA introduction by default
    )) %>%
    mutate(rms_outcome1_2_children_registered = labelled(
      rms_outcome1_2_children_registered,
      labels = c(Yes = 1, No = 0),
      label = "Proportion of children under 5 years of age whose births have been registered with a civil authority"
    ))
  
# Displaying the table of outcomes
table(df_ind_appended$rms_outcome1_2_children_registered)
  
# Calculating and rounding the proportions
round(prop.table(table(df_ind_appended$rms_outcome1_2_children_registered)), 2)


# write_xlsx(df_ind_appended, "RMS/final_individual_indicators.xlsx", col_names=TRUE)



# ------------------------------------------------------------------------------
# 1.3 Core outcome indicator	
# Proportion of Persons of Concern with legally recognized identity documents or credentials
# ------------------------------------------------------------------------------

# PRT01_SS_ID	Does this individual have the following documents? (enumerators to ask for each)
# tin	Tax Identification Number (TIN / ITN)
# national_id	National ID / ID Card
# internal_passport	Passport of a Citizen of Ukraine / Internal Passport
# non_bio_passport	Valid non-biometric Passport
# bio_passport	Valid Biometric passport
# birth_cert	Birth certificate
# none	Has none of the documents
# prefer_not_to_answer	Prefer not answer
# do_not_know	Do not know

# PRT01_SS_ID_tin	
# PRT01_SS_ID_national_id	
# PRT01_SS_ID_internal_passport	
# PRT01_SS_ID_non_bio_passport	
# PRT01_SS_ID_bio_passport	
# PRT01_SS_ID_birth_cert	
# PRT01_SS_ID_none	
# PRT01_SS_ID_prefer_not_to_answer	
# PRT01_SS_ID_do_not_know

df_ind_appended <- df_ind_appended %>%
  mutate(rms_outcome1_3_legal_documents = case_when(
    (PRT01_SS_ID_tin == 1 | PRT01_SS_ID_national_id == 1 | PRT01_SS_ID_internal_passport == 1 | PRT01_SS_ID_non_bio_passport == 1 | PRT01_SS_ID_birth_cert == 1 | PRT01_SS_ID_tin == 1) ~ 1,
    PRT01_SS_ID == "none" ~ 0,
    (PRT01_SS_ID_prefer_not_to_answer == 1 | PRT01_SS_ID_do_not_know) == 1 ~ NA_real_)
  ) %>%
  mutate(rms_outcome1_3_legal_documents = labelled(rms_outcome1_3_legal_documents,
                                               labels = c(
                                                 'Yes'= 1,
                                                 'No'= 0
                                               ),
                                               label = "Proportion of Persons of Concern with legally recognized identity documents or credentials"))

table(df_ind_appended$rms_outcome1_3_legal_documents)

round(prop.table(table(df_ind_appended$rms_outcome1_3_legal_documents)), 2)

# write_xlsx(df_ind_appended, "RMS/final_individual_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 4.1 Core outcome indicator	
# Proportion of Persons of Concern who know where to access available GBV services
# ------------------------------------------------------------------------------

# GBV01_NOTE	If someone in your community is subject to gender-based violence and asks for your help, would you be able to tell this person about the following services in this area?
# GBV01a_SS_HLTH	Health services
# GBV01b_SS_PSY	Psycho-social services
# GBV01c_SS_SAFE	Safety and security services? (police, safe shelters)
# GBV01d_SS_HELPLINE	Specific helpline to call and request a service? - DO NOT INCLUDE
# GBV01e_SS_LEGAL	Legal assistance

# yes	Yes
# no	No
# do_not_know	Do not know
# prefer_not_to_answer	Prefer not to answer


# In this case, "Do not know" and all the responses to all GBV related questions is "no" - then GBV indicator No 


df_hh_appended <- df_hh_appended %>%
  group_by(country) %>%  # Group data by the 'country' column
  mutate(rms_outcome4_1_GBV = case_when(
    GBV01a_SS_HLTH == "yes" | GBV01b_SS_PSY == "yes" | GBV01c_SS_SAFE == "yes" | GBV01e_SS_LEGAL == "yes" ~ 1,
    GBV01a_SS_HLTH == "prefer_not_to_answer" & GBV01b_SS_PSY == "prefer_not_to_answer" & GBV01c_SS_SAFE == "prefer_not_to_answer" & GBV01e_SS_LEGAL == "prefer_not_to_answer" ~ NA_real_,    
    TRUE ~ 0
  )) %>%
  mutate(rms_outcome4_1_GBV = labelled(
    rms_outcome4_1_GBV,
    labels = c('Yes' = 1, "No" = 0),
    label = "Proportion of PoC who know where to access available GBV services"
  )) %>%
  ungroup()  # Optionally ungroup data if further operations do not require grouping


# Review the results
country_gbv_outcomes <- df_hh_appended %>%
  group_by(country) %>%
  summarise(
    Total_Count = n(),
    Yes_Count = sum(rms_outcome4_1_GBV == 1, na.rm = TRUE),
    Percent_Yes = Yes_Count / Total_Count * 100,
    .groups = 'drop'  # This automatically ungroups the data after summarising
  )

print(country_gbv_outcomes)

#write_xlsx(df_hh_appended, "rms_household_indicators_check.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 13.1 Core outcome indicator
# Proportion of Persons of Concern with an account at a bank or other financial institution or with a mobile-money-service provider
# ------------------------------------------------------------------------------

## L12_SS_HV_BNK_ACC	- Do you currently have a bank account or account at a formal financial institution in the Czech Republic, either by yourself or with someone else?
# yes	Yes
# no	No
# do_not_know	Do not know
# prefer_not_to_answer	Prefer not to answer

df_hh_appended <- df_hh_appended %>%
  mutate(
    rms_outcome13_1_bank_account = case_when(
      L12_SS_HV_BNK_ACC == "yes" ~ 1,
      L12_SS_HV_BNK_ACC == "no" ~ 0,
      TRUE ~ NA_real_)
  ) %>%
  mutate(rms_outcome13_1_bank_account = labelled(rms_outcome13_1_bank_account,
                                             labels = c(
                                               "Yes" = 1,
                                               "No" = 0
                                             ),
                                             label = "PoC with an account at a bank or other financial institution or with a mobile-money service provider"))

table(df_hh_appended$rms_outcome13_1_bank_account)

round(prop.table(table(df_hh_appended$rms_outcome13_1_bank_account)), 2)

#write_xlsx(df_hh_appended, "RMS/final_household_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 13.2 Core outcome indicator	
# Proportion of Persons of Concern who self-report positive changes in their income compared to previous year
# ------------------------------------------------------------------------------

# L13_SS_AFF_GOODS	- Compared to your first months in the Czech Republic, do you think you can now afford more goods and services, the same, or fewer goods and services?
# Only calculate as positive if they responded 'more' 

df_hh_appended <- df_hh_appended %>%
  mutate(rms_outcome13_2_income = case_when(
    L13_SS_AFF_GOODS == "more" ~ 1,
    L13_SS_AFF_GOODS == "same" | L13_SS_AFF_GOODS == "fewer" ~ 0,
    TRUE ~ NA_real_)
  ) %>%
  mutate(rms_outcome13_2_income = labelled(rms_outcome13_2_income,
                                       labels = c(
                                         "Yes" = 1,
                                         "No" = 0
                                       ),
                                       label = "Proportion of PoC who self-report positive changes in their income compared to previous year"))


table(df_hh_appended$rms_outcome13_2_income)

round(prop.table(table(df_hh_appended$rms_outcome13_2_income)), 2)


write_xlsx(df_hh_appended, "final_household_indicators.xlsx", col_names=TRUE)

# ------------------------------------------------------------------------------
# 13.3 Core outcome indicator	
# Proportion of Persons of Concern (working age) who are unemployed
# ------------------------------------------------------------------------------

# Working age is 

## Numerator: Those of working age who were not in employment, looked for employment in the past 30 days and were available to take up employment
## Denominator: Those of working age in labor force

# SE2_SS_WORK	 - During the past 7 days, did this person work for someone else for pay, for one or more hours?
# SE3_SS_BUSINESS	 - During the past 7 days, did this person run or do any kind of business, farming, or other activity to generate income?
# SE4_SS_FAM_BUSINESS	-  During the past 7 days, did this person help in a family business or farm?
# SE5_SS_HELP_FAM_BUSINESS	- Even though this person did not work, during the past 7 days, did he/she have a business or a helper job in a family business/farm from which he/she was temporarily absent?
# SE6_SS_TRY_FIND_JOB	- During the last 30 days, did this person do anything to find a paid job or try to start a business?
# SE7_SS_START_WORK_IN_2_WKS- Could this person start working within the next two weeks if he/she was offered a job??

# Unemployed persons are defined as all those of working age (i.e., in the workforce - to be defined by each country context in line with legal definition) who were not in employment, carried out activities to seek employment during the past 7 days and were currently available to take up employment (SDG Indicator 8.5.2).

df_ind_appended <- df_ind_appended %>%
  mutate(calc_employed = case_when(
    SE2_SS_WORK == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE3_SS_BUSINESS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE4_SS_FAM_BUSINESS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE5_SS_HELP_FAM_BUSINESS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    SE2_SS_WORK == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE3_SS_BUSINESS == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE4_SS_FAM_BUSINESS == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE5_SS_HELP_FAM_BUSINESS == "no" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    SE2_SS_WORK == "prefer_not_to_answer" | SE3_SS_BUSINESS == "prefer_not_to_answer" | SE4_SS_FAM_BUSINESS == "prefer_not_to_answer" | SE5_SS_HELP_FAM_BUSINESS == "prefer_not_to_answer" ~ NA_real_,
    SE2_SS_WORK == "do_not_know" | SE3_SS_BUSINESS == "do_not_know" | SE4_SS_FAM_BUSINESS == "do_not_know" | SE5_SS_HELP_FAM_BUSINESS == "do_not_know" ~ NA_real_,
    TRUE ~ NA_real_
  )) %>%
  mutate(calc_unemployed = case_when(
    calc_employed == 0 & SE6_SS_TRY_FIND_JOB == "yes" & SE7_SS_START_WORK_IN_2_WKS == "yes" & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    calc_employed == 1 & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 0,
    is.na(calc_employed) | is.na(SE6_SS_TRY_FIND_JOB) | is.na(SE7_SS_START_WORK_IN_2_WKS) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(calc_labour_force = case_when(
    (calc_employed == 1 | calc_unemployed == 1) & (DR.11_NUM_AGE > 15 & DR.11_NUM_AGE < 65) ~ 1,
    is.na(calc_employed) | is.na(calc_unemployed) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  group_by(country) %>%
  mutate(
    calc_unemployed_sum = sum(calc_unemployed, na.rm = TRUE),
    calc_labour_force_total = sum(calc_labour_force, na.rm = TRUE),
    rms_outcome13_3_unemployment = ifelse(calc_labour_force_total > 0, calc_unemployed_sum / calc_labour_force_total, NA_real_),
    rms_outcome13_3_unemployment = round(rms_outcome13_3_unemployment, 2)
  ) %>%
  ungroup()


# Optional: Display or check a table of results
table(df_ind_appended$country, df_ind_appended$rms_outcome13_3_unemployment)


write_xlsx(df_ind_appended, "final_individual_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 16.2 Core outcome indicator	
# Proportion of Persons of Concern covered by social protection floors/systems
# ------------------------------------------------------------------------------

### CHECK THE LIST!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# SE2.11b_SM_BEN_HST	Which social protection benefits do you receive from the Czech government?
# cash_benefits
# disability_grant
# unemployment_grant
# child_family_grant
# other_source - where to include "Other sources" ?
# dont_know
# prefer_not_to_answer

# If PoC has covered by at least one of the social protection floors/systems

# accommodation_allowance	Accommodation allowance
# child_family_grant	Child or family benefit
# disability_grant	Disability benefit
# unemployment_grant	Unemployment benefit
# emp_injury_benefit	Employment injury benefit
# oth_vul_benefit	Other vulnerability benefit

# pension	Pension benefit
# business_grant	Business grant
# training_grant	Training grant
# oth_gov_benefit	Other government benefit (specify)
# other_source	Other source (specify)
# do_not_know	Don't know
# prefer_not_to_answer	Prefer not to say

df_hh_appended <- df_hh_appended %>%
  mutate(rms_outcome16_2_social_protection = case_when(
    SE2.11b_SM_BEN_HST %in% c("accommodation_allowance", "disability_grant", "unemployment_grant", "child_family_grant", "other_source","emp_injury_benefit","oth_vul_benefit") ~ 1,
    SE2.11b_SM_BEN_HST %in% c("prefer_not_to_answer", "do_not_know") ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(rms_outcome16_2_social_protection = labelled(rms_outcome16_2_social_protection,
                                                  labels = c('Yes' = 1, 
                                                             'No' = 0),
                                                  label = "Proportion of Persons of Concern covered by social protection floors/systems"))


table(df_hh_appended$rms_outcome16_2_social_protection)

round(prop.table(table(df_hh_appended$rms_outcome16_2_social_protection)), 2)

#write_xlsx(df_hh, "RMS/final_household_indicators.xlsx", col_names=TRUE)


# ------------------------------------------------------------------------------
# 9.1 Core outcome indicator - PROXY!!!! NOT THE ACTUAL INDICATOR
# Proportion of Persons of Concern living in habitable and affordable housing
# ------------------------------------------------------------------------------

# We only have the questions for calculating the crowding
# SHL02_NUM_ROOMS - number of rooms
# DR8_NUM_HH_SIZE - HH size
# SHL07_SM_LIV_COND - What issue, if any, are you facing in terms of living conditions in your accommodation?

# no_issues	No issues
# unable_to_cook_store_food	Unable to cook and/or store food properly (cooking facilities are unsafe, insufficient cooking items)
# lack_of_showers_toilets	Lack of separate showers and/or toilets
# lack_of_hot_water	Lack of sufficient hot water
# do_not_feel_protected	Do not feel protected (Unable to lock home securely, insufficient light inside or outside, overall sentiment)
# insufficient_privacy	Insufficient privacy (no partitions, doors)
# unable_to_keep_warm_cool	Unable to keep warm or cool (no or dysfunctional temperature regulating devices, lack of insulation, insufficient winter clothes)
# unclean_space	Space is not sufficiently clean
# inaccessible_by_transportation	Space is not easily accessible using local transportation
# disposal_of_waste_system	Space doesn't have an organized disposal of waste system
# inaccessible_to_disabled	Place is not accessible to persons with disabilities
# insufficient_sleeping_materials	Insufficient sleeping materials (mattress, blankets etc)
# do_not_know	Don't know
# prefer_not_to_answer	Prefer not to say

df_hh_appended <- df_hh_appended %>%
  mutate(
    calc_crowding = round(DR8_NUM_HH_SIZE / SHL02_NUM_ROOMS, 2),  # Round to 2 decimal places
    sufficient_dwel_1 = case_when(
      calc_crowding <= 3 ~ 1,
      TRUE ~ 0
    )
  )

round(prop.table(table(df_hh_appended$sufficient_dwel_1)), 2)

df_hh_appended <- df_hh_appended %>%
  mutate(sufficient_dwel_2 = case_when(
    SHL07_SM_LIV_COND_no_issues == 1 ~ 1,
    
    SHL07_SM_LIV_COND_unclean_space == 1 &
      SHL07_SM_LIV_COND_unable_to_cook_store_food == 0 &
      SHL07_SM_LIV_COND_lack_of_showers_toilets == 0 &
      SHL07_SM_LIV_COND_lack_of_hot_water == 0 &
      SHL07_SM_LIV_COND_do_not_feel_protected == 0 &
      SHL07_SM_LIV_COND_insufficient_privacy == 0 &
      SHL07_SM_LIV_COND_unable_to_keep_warm_cool == 0 &
      SHL07_SM_LIV_COND_inaccessible_by_transportation == 0 &
      SHL07_SM_LIV_COND_disposal_of_waste_system == 0 &
      SHL07_SM_LIV_COND_inaccessible_to_disabled == 0 &
      SHL07_SM_LIV_COND_insufficient_sleeping_materials == 0 ~ 1,
    
    SHL07_SM_LIV_COND_inaccessible_by_transportation == 1 & 
      SHL07_SM_LIV_COND_unable_to_cook_store_food == 0 &
      SHL07_SM_LIV_COND_lack_of_showers_toilets == 0 &
      SHL07_SM_LIV_COND_lack_of_hot_water == 0 &
      SHL07_SM_LIV_COND_do_not_feel_protected == 0 &
      SHL07_SM_LIV_COND_insufficient_privacy == 0 &
      SHL07_SM_LIV_COND_unable_to_keep_warm_cool == 0 &
      SHL07_SM_LIV_COND_unclean_space == 0 &
      SHL07_SM_LIV_COND_disposal_of_waste_system == 0 &
      SHL07_SM_LIV_COND_inaccessible_to_disabled == 0 &
      SHL07_SM_LIV_COND_insufficient_sleeping_materials == 0 ~ 1,
    
    SHL07_SM_LIV_COND_unclean_space == 1 & 
      SHL07_SM_LIV_COND_unable_to_cook_store_food == 0 &
      SHL07_SM_LIV_COND_lack_of_showers_toilets == 0 &
      SHL07_SM_LIV_COND_lack_of_hot_water == 0 &
      SHL07_SM_LIV_COND_do_not_feel_protected == 0 &
      SHL07_SM_LIV_COND_insufficient_privacy == 0 &
      SHL07_SM_LIV_COND_unable_to_keep_warm_cool == 0 &
      SHL07_SM_LIV_COND_inaccessible_by_transportation == 0 &
      SHL07_SM_LIV_COND_disposal_of_waste_system == 0 &
      SHL07_SM_LIV_COND_inaccessible_to_disabled == 0 &
      SHL07_SM_LIV_COND_insufficient_sleeping_materials == 0 &
      # Additional conditions to make sure no other issues are selected
      SHL07_SM_LIV_COND_no_issues == 0 &
      SHL07_SM_LIV_COND_inaccessible_by_transportation == 0 ~ 1,
    
    SHL07_SM_LIV_COND_do_not_know == 1 | SHL07_SM_LIV_COND_prefer_not_to_answer == 1 ~ NA_real_,
    
    TRUE ~ 0
  ))

df_hh_appended <- df_hh_appended %>%
  mutate(rms_outcome9_1_housing = case_when(
    sufficient_dwel_1 == 0 | sufficient_dwel_2 == 0  ~ 0, 
    sufficient_dwel_1 == 1 & sufficient_dwel_2 == 1  ~ 1, 
    TRUE ~ NA_real_ )
  ) %>%
  mutate(rms_outcome9_1_housing = labelled(rms_outcome9_1_housing,
                                       labels = c(
                                         "Yes" = 1,
                                         "No" = 0
                                       ),
                                       label = "Proportion of PoCs living in habitable and affordable housing"))


table(df_hh_appended$rms_outcome9_1_housing)

round(prop.table(table(df_hh_appended$rms_outcome9_1_housing)), 2)

write_xlsx(df_hh_appended, "final_household_indicators.xlsx", col_names = TRUE)


# ------------------------------------------------------------------------------
# 10.1 Core Outcome Indicator	
# Proportion of children aged 9 months to five years who have received measles vaccination
# ------------------------------------------------------------------------------

# H5.1_SS_HLTH_VACCINE_MEASLES	Has this child/children aged 9 months - 5 years ever received a measles-containing vaccine?
# H5.2_SS_HLTH_VACCINE_MEASLES_2DOSE	Did [child name] receive a second dose?
# H6_SS_HLTH_VACCINE_POLIO	How many polio vaccine doses has this child received in total?
# yesnoext	yes
# yesnoext	no
# yesnoext	DoNotKnow
# yesnoext	PreferNotAnswer

# Turn into numeric
# df_ind$H5.1_SS_HLTH_VACCINE_MEASLES <- labelled_chr2dbl(df_ind$H5.1_SS_HLTH_VACCINE_MEASLES)


df_ind_appended <- df_ind_appended %>%
  mutate(rms_outcome10_1_measles = case_when(
    H5.1_SS_HLTH_VACCINE_MEASLES == "yes" ~ 1, 
    H5.1_SS_HLTH_VACCINE_MEASLES == "no" ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(rms_outcome10_1_measles = labelled(rms_outcome10_1_measles,
                                        labels = c(
                                          "Yes" = 1,
                                          "No" = 0
                                        ),
                                        label = "Proportion of children aged 9 months to five years who have received measles vaccination*"))



table(df_ind_appended$rms_outcome10_1_measles)

round(prop.table(table(df_ind_appended$rms_outcome10_1_measles)), 2)



write_xlsx(df_ind_appended, "final_individual_indicators.xlsx", col_names = TRUE)



# ------------------------------------------------------------------------------
# % HHs with of youth (age 15-24 years) not in education, employment or training
# ------------------------------------------------------------------------------
# 16 to 24 inclusive - that's the data we have for employment


# Need to combine indicators on attendance, employment / unemployent and main activity

# SE8_SS_ACTIVITY	Which of the following best describes what (this person) is mainly doing at present?
# status_unempl	Unemployed/job-seeker
# studying	Studying
# professional_training	Professional training
# engaged_in_HH_resp	Engaged in household or family responsibilities including taking care of children and elderly
# retired	Retired or Pensioner
# long_term_ill_injury	With a long-term illness, injury or disability
# unpaid_volunteering	Doing unpaid volunteering, community or charity work

# Employed: employed != 1
# Not attending school: E1_SS_ATT_EDU != yes
# Not attending training: SE8_SS_ACTIVITY !=professional_training | SE8_SS_ACTIVITY !=professional_training = studying
# Education only in host country?? because now we are removing those who might be attending only online

df_ind_appended <- df_ind_appended %>%  
  mutate(calc_inactive_youth = case_when(     
    (calc_employed != 1 & E0_SS_ENROLLED_HC != "yes" & !(SE8_SS_ACTIVITY %in% c("professional_training", "studying")) & (DR.11_NUM_AGE >= 15 & DR.11_NUM_AGE <= 24)) ~ 1,    
    (calc_employed == 1 | E0_SS_ENROLLED_HC == "yes" | SE8_SS_ACTIVITY %in% c("professional_training", "studying")) & (DR.11_NUM_AGE >= 15 & DR.11_NUM_AGE <= 24) ~ 0,      
    TRUE ~ NA_real_   ))

#df_selected <- df_ind_appended %>%
#  select(DR.11_NUM_AGE, calc_employed, E0_SS_ENROLLED_HC, SE8_SS_ACTIVITY, calc_inactive_youth)

#view(df_selected)

round(prop.table(table(df_ind_appended$calc_inactive_youth)), 2)

write_xlsx(df_ind_appended, "final_individual_indicators.xlsx", col_names = TRUE)


# ------------------------------------------------------------------------------
# Additional demographic indicators 
# ------------------------------------------------------------------------------


df_ind_appended <- df_ind_appended %>%
  rename("parent_index" = "_parent_index")


# First, summarize the data by '_parent_index'
df_ind_appended <- df_ind_appended %>%
  dplyr::group_by(unique_hh_index) %>%
  dplyr::summarize(
    calc_sum_children = sum(DR.11_NUM_AGE < 18, na.rm = TRUE),
    calc_sum_adults = sum((DR.11_NUM_AGE >= 18 & DR.11_NUM_AGE < 60), na.rm = TRUE),
    calc_sum_adults_over_18 = sum((DR.11_NUM_AGE >= 18), na.rm = TRUE),
    calc_sum_females = sum(DR.12_SS_GEN == "female", na.rm = TRUE),
    calc_sum_males =  sum(DR.12_SS_GEN == "male", na.rm = TRUE),
    calc_sum_females_over_18 = sum((DR.12_SS_GEN == "female" & DR.11_NUM_AGE >= 18), na.rm = TRUE),
    calc_sum_males_over_18 = sum((DR.12_SS_GEN == "male" & DR.11_NUM_AGE >= 18), na.rm = TRUE),
    calc_sum_older_persons = sum(DR.11_NUM_AGE >= 60, na.rm = TRUE),
    calc_sum_disability = sum(calc_disability == 1, na.rm = TRUE),
    calc_sum_chronic_illness = sum(H2_SS_HLTH_CHRONIC_ILL == "yes", na.rm = TRUE),
    calc_sum_employed = sum(calc_employed == 1, na.rm = TRUE),
  ) %>%
  ungroup()



# Now select the required columns to form df_ind_join
df_ind_join <- df_ind_appended %>%
  select(unique_hh_index, calc_sum_children, calc_sum_adults, calc_sum_females, calc_sum_males, calc_sum_older_persons, calc_sum_disability, calc_sum_employed, calc_sum_chronic_illness, calc_sum_females_over_18, calc_sum_males_over_18)


# Joining the dataframes
df_hh_appended <- df_hh_appended %>%
  left_join(df_ind_join, by = "unique_hh_index")

view(df_hh_appended)



df_hh_appended <- df_hh_appended %>%
  mutate(calc_disability_tag  = case_when(
    calc_sum_disability > 0 ~ "HH with disability", 
    TRUE ~ "HH without disability"
  ))


df_hh_appended <- df_hh_appended %>%
  mutate(calc_child_tag   = case_when(
    calc_sum_children > 0 ~ "HH with children", 
    TRUE ~ "HH without children"
  ))


df_hh_appended <- df_hh_appended %>%
  mutate(calc_employed_tag  = case_when(
    calc_sum_employed > 0 ~ "HH with employed members", 
    TRUE ~ "HH without employed members"
  ))



df_hh_appended <- df_hh_appended %>%
  mutate(calc_older_persons_tag  = case_when(
    calc_sum_older_persons > 0 ~ "HH with older persons", 
    TRUE ~ "HH without older persons"
  ))


df_hh_appended <- df_hh_appended %>%
  mutate(calc_chronic_illness_tag = case_when(
    calc_sum_chronic_illness > 0 ~ "HH with chronic illness", 
    TRUE ~ "HH without chronic illness"
  ))


# ------------------------------------------------------------------------------
# HOUSEHOLD TYPOLOGIES
# ------------------------------------------------------------------------------

# Create the new column 'household_typology_1' using case_when
df_hh_appended <- df_hh_appended %>%
  mutate(
    calc_household_typology_1 = case_when(
      calc_sum_adults > 0 & calc_sum_children == 0 & calc_sum_older_persons == 0 ~ "One or more adults (18-59) without dependents",
      calc_sum_older_persons > 0 & calc_sum_children == 0 & calc_sum_adults == 0 ~ "One or more older persons (60+)",
      calc_sum_older_persons > 0 & calc_sum_children > 0 & calc_sum_adults == 0 ~ "One or more older persons (60+) with children",
      calc_sum_adults == 1 & (calc_sum_children > 0 | calc_sum_older_persons > 0) ~ "Only one adult (18-59) with dependents",
      calc_sum_adults >= 2 & (calc_sum_children > 0 | calc_sum_older_persons > 0) ~ "Two or more adults (18-59) with dependents",
      TRUE ~ "Other"  # Default case if none of the above conditions are met
    )
  ) 


# Create the new column 'household_typology_2'
df_hh_appended <- df_hh_appended %>%
  mutate(
    calc_household_typology_2 = case_when(
      calc_sum_females_over_18  > 0 & calc_sum_children == 0 & calc_sum_males_over_18 == 0  ~ "Female adult(s) (18+) without children",
      calc_sum_females_over_18  > 0 & calc_sum_children > 0 & calc_sum_males_over_18 == 0 ~ "Female adult(s) (18+) with children",
      calc_sum_males_over_18 > 0 & calc_sum_children == 0 & calc_sum_females_over_18 == 0 ~ "Male adult(s) (18+) without children",
      calc_sum_males_over_18 > 0  & calc_sum_children > 0 & calc_sum_females_over_18 == 0 ~ "Male adult(s) (18+) with children",
      (calc_sum_females_over_18 > 0 | calc_sum_males_over_18 > 0) & calc_sum_children == 0 ~ "Two or more adults (+18) without children",
      (calc_sum_females_over_18 > 0 | calc_sum_males_over_18 > 0) & calc_sum_children > 0 ~ "Two or more adults (+18) with children",
      TRUE ~ "Other"  # Default case if none of the above conditions are met
    )
  ) 


write_xlsx(df_hh_appended, "final_household_indicators.xlsx", col_names = TRUE)


### INCOME - EXCHANGE 

view(df_hh_appended)

# CONVERT SE2.11a_NUM_FAM_SUPP_AMT TO LOCAL CURRENCY AND MERGE TO THE MAIN DATASET USING THE DATE OF INTERVIEW

# Calculate the median date of data collection period
median_date <- median(df_hh_appended$today)

# Print the median date
print(median_date)

# Find the earliest and latest dates
earliest_date <- min(df_hh_appended$today)
latest_date <- max(df_hh_appended$today)

# Calculate the midpoint of the time period
midpoint_date <- earliest_date + (latest_date - earliest_date) / 2

# Print the midpoint date
print(midpoint_date)


df_hh_appended$exchange_rate <- 0.11


write.xlsx(df_hh_appended, "final_household_indicators.xlsx", rowNames = TRUE)

### INCOME

# SE2.11a_NUM_FAM_SUPP_AMT # divide by 3

# SE2.11a_NUM_FAM_SUPP_AMT
# SE2.11a_NUM_INC_AMT_RGL
# SE2.11a_NUM_INC_AMT_RGL_parttime
# SE2.11a_NUM_INC_AMT_RMT_UKR
# SE2.11a_NUM_INC_AMT_RMT_OTH
# SE2.11a_NUM_INC_AMT_SLF
# SE2.11a_NUM_INC_AMT_OTH
# SE2.11b_NUM_BEN_AMT_Tot
# SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE
# SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER
# SE2.11d_NUM_BEN_OTH_CSH
# SE2.11d_NUM_BEN_OTH_INV
# SE2.11d_NUM_BEN_OTH_LON
# SE2.11d_NUM_BEN_OTH_OTH

# Family support
df_hh_appended$SE2.11a_NUM_FAM_SUPP_AMT <- as.numeric(df_hh_appended$SE2.11a_NUM_FAM_SUPP_AMT)

# Employment
df_hh_appended$SE2.11a_NUM_INC_AMT_RGL <- as.numeric(df_hh_appended$SE2.11a_NUM_INC_AMT_RGL)
df_hh_appended$SE2.11a_NUM_INC_AMT_RGL_parttime <- as.numeric(df_hh_appended$SE2.11a_NUM_INC_AMT_RGL_parttime)
df_hh_appended$SE2.11a_NUM_INC_AMT_RMT_UKR <- as.numeric(df_hh_appended$SE2.11a_NUM_INC_AMT_RMT_UKR)
df_hh_appended$SE2.11a_NUM_INC_AMT_RMT_OTH <- as.numeric(df_hh_appended$SE2.11a_NUM_INC_AMT_RMT_OTH)
df_hh_appended$SE2.11a_NUM_INC_AMT_SLF <- as.numeric(df_hh_appended$SE2.11a_NUM_INC_AMT_SLF)
df_hh_appended$SE2.11a_NUM_INC_AMT_OTH <- as.numeric(df_hh_appended$SE2.11a_NUM_INC_AMT_OTH)


# Social protection
# Host country
df_hh_appended$SE2.11b_NUM_BEN_AMT_Tot <- as.numeric(df_hh_appended$SE2.11b_NUM_BEN_AMT_Tot)

# Ukraine
df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE <- as.numeric(df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE)
df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER <- as.numeric(df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER)

# Humanitarian
df_hh_appended$SE2.11d_NUM_BEN_OTH_CSH <- as.numeric(df_hh_appended$SE2.11d_NUM_BEN_OTH_CSH)
df_hh_appended$SE2.11d_NUM_BEN_OTH_INV <- as.numeric(df_hh_appended$SE2.11d_NUM_BEN_OTH_INV)
df_hh_appended$SE2.11d_NUM_BEN_OTH_LON <- as.numeric(df_hh_appended$SE2.11d_NUM_BEN_OTH_LON)
df_hh_appended$SE2.11d_NUM_BEN_OTH_OTH <- as.numeric(df_hh_appended$SE2.11d_NUM_BEN_OTH_OTH)


# BEFORE CLEANING THE DATA: Generate the new variable incomplete_income_profile (they said they have this income type but did not indicate any amount)
# Employment income 

nonresp_values <- c(
  1, 9999, 99999, 999999, 9999999, 99999999, 999999999, 100000000)

df_hh_appended <- df_hh_appended %>%
  mutate(calc_income_profile = case_when(
    SE2.11a_NUM_INC_AMT_RGL %in% nonresp_values | SE2.11a_NUM_INC_AMT_RGL_parttime %in% nonresp_values |
      SE2.11a_NUM_INC_AMT_SLF %in% nonresp_values | SE2.11a_NUM_INC_AMT_RMT_UKR %in% nonresp_values |
      SE2.11a_NUM_INC_AMT_RMT_OTH %in% nonresp_values ~ "incomplete_employment_income",
    SE2.11_SS_INC_SOR_prefer_not_to_answer == 1 | SE2.11_SS_INC_SOR_do_not_know == 1 ~ "incomplete_employment_income", 
    SE2.11b_NUM_BEN_AMT_Tot %in% nonresp_values | SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE %in% nonresp_values | 
      SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER %in% nonresp_values | SE2.11a_NUM_FAM_SUPP_AMT %in% nonresp_values |
      SE2.11a_NUM_INC_AMT_OTH %in% nonresp_values | SE2.11d_NUM_BEN_OTH_CSH %in% nonresp_values |
      SE2.11d_NUM_BEN_OTH_INV %in% nonresp_values | SE2.11d_NUM_BEN_OTH_OTH ~ "incomplete_other_income",
    SE2.11a_SS_FAM_SUPP_FIN == "do_not_know" | SE2.11a_SS_FAM_SUPP_FIN == "prefer_not_to_answer" |
      SE2.11d_SM_BEN_OTH == "do_not_know" | SE2.11d_SM_BEN_OTH == "prefer_not_to_answer" ~ "incomplete_other_inc",
    TRUE ~ "complete_income_profile"
  ))

df_hh_appended <- df_hh_appended %>%
  mutate(calc_no_income = case_when(
    SE2.11_SS_INC_SOR_no_income ==1 ~ "no_income",
    TRUE ~ "income_reported"
  ))


variables <- c("SE2.11a_NUM_FAM_SUPP_AMT", # family support
               "SE2.11a_NUM_INC_AMT_RGL",  # employment 
               "SE2.11a_NUM_INC_AMT_RGL_parttime", # part-time employment
               "SE2.11a_NUM_INC_AMT_RMT_UKR",  # employment remote Ukraine
               "SE2.11a_NUM_INC_AMT_RMT_OTH", # employment remote other
               "SE2.11a_NUM_INC_AMT_SLF", # employment self-employed
               
               "SE2.11a_NUM_INC_AMT_OTH", # other income 0 
               
               "SE2.11b_NUM_BEN_AMT_Tot", # total benefits host country 
               
               "SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE", # Ukraine benefits
               "SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER", # Ukraine benefits
               
               "SE2.11d_NUM_BEN_OTH_CSH", # humanitarian
               "SE2.11d_NUM_BEN_OTH_INV", # investments 
               "SE2.11d_NUM_BEN_OTH_OTH") # other sources



# Remove specified values from specified columns
df_hh_appended[variables] <- lapply(df_hh_appended[variables], function(x) 
  ifelse(x %in% c(1, 9999, 99999, 999999, 9999999, 99999999, 999999999, 100000000), NA, x))


# Family support to convert to 30 days
df_hh_appended$SE2.11a_NUM_FAM_SUPP_AMT_monthly <- df_hh_appended$SE2.11a_NUM_FAM_SUPP_AMT / 3      # convert to 30 days

# convert to local currency
df_hh_appended$SE2.11a_NUM_FAM_SUPP_AMT_converted <- ifelse(!is.na(df_hh_appended$SE2.11a_NUM_FAM_SUPP_AMT_monthly) & !is.na(df_hh_appended$exchange_rate),
                                                            round(df_hh_appended$SE2.11a_NUM_FAM_SUPP_AMT_monthly / df_hh_appended$exchange_rate, 2), 
                                                            NA)

# convert to local currency
df_hh_appended$SE2.11a_NUM_INC_AMT_RMT_UKR_converted <- ifelse(!is.na(df_hh_appended$SE2.11a_NUM_INC_AMT_RMT_UKR) & !is.na(df_hh_appended$exchange_rate),
                                                               round(df_hh_appended$SE2.11a_NUM_INC_AMT_RMT_UKR / df_hh_appended$exchange_rate, 2), 
                                                               NA)

# convert to local currency
df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE_converted <- ifelse(!is.na(df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE) & !is.na(df_hh_appended$exchange_rate),
                                                                      round(df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE / df_hh_appended$exchange_rate, 2), 
                                                                      NA)
# convert to local currency
df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER_converted <- ifelse(!is.na(df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER) & !is.na(df_hh_appended$exchange_rate),
                                                                     round(df_hh_appended$SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER / df_hh_appended$exchange_rate, 2), 
                                                                     NA)

total_rows<- nrow(df_hh_appended)
print(total_rows)


variables_1 <- c("SE2.11a_NUM_FAM_SUPP_AMT_converted", # family support
                 "SE2.11a_NUM_INC_AMT_RGL",  # employment 
                 "SE2.11a_NUM_INC_AMT_RGL_parttime", # part-time employment
                 "SE2.11a_NUM_INC_AMT_RMT_UKR_converted",  # employment remote Ukraine
                 "SE2.11a_NUM_INC_AMT_RMT_OTH", # employment remote other
                 "SE2.11a_NUM_INC_AMT_SLF", # employment self-employed
                 "SE2.11a_NUM_INC_AMT_OTH", # other income
                 "SE2.11b_NUM_BEN_AMT_Tot", # total benefits host country 
                 "SE2.11c_NUM_BEN_AMT_PEN_UKR_OLDAGE_converted", # Ukraine benefits
                 "SE2.11c_NUM_BEN_AMT_PEN_UKR_OTHER_converted", # Ukraine benefits
                 "SE2.11d_NUM_BEN_OTH_CSH", # humanitarian
                 "SE2.11d_NUM_BEN_OTH_INV", # investments 
                 "SE2.11d_NUM_BEN_OTH_OTH") # other sources


# Sum the income columns
df_hh_appended$calc_total_hh_income <- rowSums(df_hh_appended[, variables_1], na.rm = TRUE)


# Set total_income to NA where all relevant columns are NA
#all_na_rows <- rowSums(is.na(df_hh_appended[, variables_1])) == length(variables_1)
#df_hh_appended$calc_total_hh_income[all_na_rows] <- NA


df_hh_appended$calc_total_hh_income <- round(df_hh_appended$calc_total_hh_income, 2)


# Calculate quantiles with na.rm = TRUE
#quantiles <- quantile(df_hh_appended$calc_total_hh_income, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)


# Assign categories based on quantiles
# df_hh_appended$calc_income_quintiles <- cut(df_hh_appended$calc_total_hh_income, breaks = quantiles, labels = c("Very Low", "Low", "Medium", "High", "Very High"), include.lowest = TRUE)


df_hh_appended <- subset(df_hh_appended, select = -c(sufficient_dwel_1, sufficient_dwel_2))


write.xlsx(df_hh_appended, "final_household_indicators.xlsx", rowNames = TRUE)




