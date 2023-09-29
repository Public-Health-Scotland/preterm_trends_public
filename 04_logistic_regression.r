##Logistic regression####
#Decisions: not multinomial, do by each term grouip? 
#- i think . that is in comments on the protocol but just double check

###weightings for logistic regression
## the response variable is the proportion of births in the birth category of interest (always 0/1)
##the weight is the total in that category (ie the denominator)
## example: if jus tlooking at preterm birth year trends, the response variable is:
## n_births_preterm_in year/n_births_total_in_year
##the weight is the n_births_total_in_year

##if modelling eg agexyear the proportions, denominator and total are with respect to each yearXage group
## response = n_births_preterm_in year:age group /n_births_total_in_year_age_group

#remove missing data,  not informative and muddies the picture

# source filepaths etc####
source(paste0(here::here(), "/00_setup.R"))

#load data.####
single_gest_weeks <- readRDS(paste0(data_path, "2_working_data/gest_wks_single.rds")) %>% filter(year!="2020")
single_gest_age <- readRDS(paste0(data_path, "2_working_data/gest_by_age_single.rds"))%>% filter(year!="2020")
single_gest_simd <-  readRDS(paste0(data_path,"2_working_data/gest_by_simd_single.rds"))%>% filter(year!="2020")
single_gest_onset <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_single.rds"))%>% filter(year!="2020")

multi_gest_weeks <- readRDS(paste0(data_path, "2_working_data/gest_wks_multi.rds"))%>% filter(year!="2020")
multi_gest_age <- readRDS(paste0(data_path, "2_working_data/gest_by_age_multi.rds"))%>% filter(year!="2020")
multi_gest_simd <-  readRDS(paste0(data_path,"2_working_data/gest_by_simd_multi.rds"))%>% filter(year!="2020")
multi_gest_onset <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_multi.rds"))%>% filter(year!="2020")

############################
##Singletons####
########################
##Trend over time####
#model term, preterm, post term separately.
#preterm
single_gest <- single_gest_weeks %>%
  filter(birth_category !="Unknown") %>%
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  mutate(n_births_total = sum(n_births)) %>%
  ungroup()  %>%
  mutate(prop_births = n_births/n_births_total)

df <- single_gest %>%
  filter(birth_category=="Preterm")

mod_pre <- glm(prop_births ~ as.numeric(year) , 
               family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_pre)
confint(mod_pre, exp=T)
gtsummary::tbl_regression(mod_pre, exponentiate = TRUE)
#saveRDS(gtsummary::tbl_regression(mod_pre, exponentiate = TRUE), "tables/regression_tables/preterm_trend_single.rds")
coeff_yr <- exp(summary(mod_pre)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod_pre)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values , "tables/regression_tables/yrvalues_preterm_trend_single.rds")

df <- single_gest %>%
  filter(birth_category=="Post-term")

mod_post <- glm(prop_births ~ as.numeric(year) , 
               family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_post)
#saveRDS(gtsummary::tbl_regression(mod_post, exponentiate = TRUE), "tables/regression_tables/postterm_trend_single.rds")
coeff_yr <- exp(summary(mod_post)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod_post)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values , "tables/regression_tables/yrvalues_postterm_trend_single.rds")


df <- single_gest %>%
  filter(birth_category=="Term")

mod_term <- glm(prop_births ~ as.numeric(year) , 
              family = binomial("logit"), weights = n_births_total, data=df)
summary(mod_term)

mod <- mod_term
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values , "tables/regression_tables/yrvalues_term_trend_single.rds")
gtsummary::tbl_regression(mod_term, exponentiate = TRUE)
saveRDS(gtsummary::tbl_regression(mod_term, exponentiate = TRUE),
        "tables/regression_tables/term_trend_single.rds")



##Trend over time, by socio economic factors####
#Age####

single_gest_age_agg <- single_gest_age %>%
  filter(birth_category !="Unknown" & maternal_age_at_delivery_years != "Unknown") %>%
  group_by(year,maternal_age_at_delivery_years,  birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year, maternal_age_at_delivery_years) %>%# weighting total in each year x age group for model with year and age
  mutate(n_births_total = sum(n_births)) %>%
  ungroup()  %>%
  mutate(prop_births = n_births/n_births_total)
head(single_gest_age_agg)

df <- single_gest_age_agg %>% filter(birth_category=="Preterm")
mod_pre1 <- glm(prop_births ~ as.numeric(year)+maternal_age_at_delivery_years , 
               family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_pre1)
gtsummary::tbl_regression(mod_pre1, exponentiate = TRUE)

mod_pre2 <- glm(prop_births ~ as.numeric(year)*maternal_age_at_delivery_years , 
                family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_pre2)
gtsummary::tbl_regression(mod_pre2, exponentiate = TRUE)#
##with < 20 as the baseline, all the others increase less apart from >40
#perhaps should set the comparison to a mid age group?
# so older and younger groups see more of an increase over time

#post term age####
df<- single_gest_age_agg %>% filter(birth_category=="Post-term")
mod_post1 <- glm(prop_births ~ as.numeric(year)+maternal_age_at_delivery_years , 
                family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_post1)
gtsummary::tbl_regression(mod_post1, exponentiate = TRUE)

#term age####
df<- single_gest_age_agg %>% filter(birth_category=="Term")
mod_term1 <- glm(prop_births ~ as.numeric(year)+maternal_age_at_delivery_years , 
                 family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_term1)
gtsummary::tbl_regression(mod_term1, exponentiate = TRUE)


##simd####
single_gest_simd_agg <- single_gest_simd %>%
  filter(birth_category !="Unknown" & maternal_simd_quintile_from_postcode_of_residence != "Unknown") %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence,  birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>%
  mutate(n_births_total = sum(n_births)) %>%
  ungroup()  %>%
  mutate(prop_births = n_births/n_births_total)
head(single_gest_simd_agg)


df <- single_gest_simd_agg %>% filter(birth_category=="Preterm")
mod_pre1 <- glm(prop_births ~ as.numeric(year) + maternal_simd_quintile_from_postcode_of_residence, 
                family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_pre1)
gtsummary::tbl_regression(mod_pre1, exponentiate = TRUE)
mod_pre2 <- glm(prop_births ~ as.numeric(year)*(maternal_simd_quintile_from_postcode_of_residence), 
                family = binomial("logit"), weights = n_births_total, data=df)
gtsummary::tbl_regression(mod_pre2, exponentiate = TRUE)

##no sig interaction
mod_pre2 <- glm(prop_births ~ as.numeric(year)*as.numeric(maternal_simd_quintile_from_postcode_of_residence), 
                family = binomial("logit"), weights = n_births_total, data=df)
gtsummary::tbl_regression(mod_pre2, exponentiate = TRUE)

summary(mod_pre2)

#post term
df <- single_gest_simd_agg %>% filter(birth_category=="Post-term")
mod_post1 <- glm(prop_births ~ as.numeric(year) + maternal_simd_quintile_from_postcode_of_residence, 
                family = binomial("logit"), weights = n_births_total, data=df)


gtsummary::tbl_regression(mod_post1, exponentiate = TRUE)


###############################
#multiples#####
#######################
multi_gest <- multi_gest_weeks %>%
  filter(birth_category !="Unknown") %>%
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%
  mutate(n_births_total = sum(n_births)) %>%
  ungroup()  %>%
  mutate(prop_births = n_births/n_births_total)

df <- multi_gest %>%
  filter(birth_category=="Preterm")

mod_pre <- glm(prop_births ~ as.numeric(year) , 
               family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_pre)
gtsummary::tbl_regression(mod_pre, exponentiate = TRUE)
saveRDS(gtsummary::tbl_regression(mod_pre, exponentiate = TRUE),
        "tables/regression_tables/preterm_trend_multi.rds")

##term
df <- multi_gest %>%
  filter(birth_category=="Term")

mod_term <- glm(prop_births ~ as.numeric(year) , 
                family = binomial("logit"), weights = n_births_total, data=df)
summary(mod_term)
gtsummary::tbl_regression(mod_term, exponentiate = TRUE)
saveRDS(gtsummary::tbl_regression(mod_term, exponentiate = TRUE),
        "tables/regression_tables/term_trend_multi.rds")


##Trend over time, by socio economic factors####
#Age####
multi_gest_age_agg <- multi_gest_age %>%
  filter(birth_category !="Unknown" & maternal_age_at_delivery_years != "Unknown") %>%
  group_by(year,maternal_age_at_delivery_years,  birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year, maternal_age_at_delivery_years) %>%
  mutate(n_births_total = sum(n_births)) %>%
  ungroup()  %>%
  mutate(prop_births = n_births/n_births_total)
head(multi_gest_age_agg)

df <- multi_gest_age_agg %>% filter(birth_category=="Preterm")
mod_pre1 <- glm(prop_births ~ as.numeric(year)+maternal_age_at_delivery_years , 
                family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_pre1)
gtsummary::tbl_regression(mod_pre1, exponentiate = TRUE)
saveRDS(gtsummary::tbl_regression(mod_pre1, exponentiate = TRUE),
        "tables/regression_tables/term_age_trend_multi.rds")


df <- multi_gest_age_agg %>% filter(birth_category=="Term")
mod_term <- glm(prop_births ~ as.numeric(year)+maternal_age_at_delivery_years , 
                family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_term)
gtsummary::tbl_regression(mod_term , exponentiate = TRUE)
saveRDS(gtsummary::tbl_regression(mod_term, exponentiate = TRUE),
        "tables/regression_tables/term_age_trend_multi.rds")

##SIMD####
multi_gest_simd_agg <- multi_gest_simd %>%
  filter(birth_category !="Unknown" & maternal_simd_quintile_from_postcode_of_residence != "Unknown") %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence,  birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>%
  mutate(n_births_total = sum(n_births)) %>%
  ungroup()  %>%
  mutate(prop_births = n_births/n_births_total)
head(single_gest_simd_agg)


df <- multi_gest_simd_agg %>% filter(birth_category=="Preterm")
mod_pre1 <- glm(prop_births ~ as.numeric(year) + maternal_simd_quintile_from_postcode_of_residence, 
                family = binomial("logit"), weights = n_births_total, data=df)

summary(mod_pre1)
gtsummary::tbl_regression(mod_pre1, exponentiate = TRUE)
saveRDS(gtsummary::tbl_regression(mod_term, exponentiate = TRUE),
        "tables/regression_tables/preterm_simd_trend_multi.rds")

##no significant interaction with simd as numeric.
mod_pre1 <- glm(prop_births ~ as.numeric(year)*as.numeric(maternal_simd_quintile_from_postcode_of_residence), 
                family = binomial("logit"), weights = n_births_total, data=df)


df <- single_gest_simd_agg %>% filter(birth_category=="Term")
mod_term <- glm(prop_births ~ as.numeric(year) + maternal_simd_quintile_from_postcode_of_residence, 
                 family = binomial("logit"), weights = n_births_total, data=df)
gtsummary::tbl_regression(mod_term, exponentiate = TRUE)


###################################
###Preterm categories####
###################################
##singles####
##preterm trends only
single_preterm_grp <- single_gest_weeks %>% 
  group_by(year,  preterm_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% 
  mutate(tot_births = sum(N_births)) %>%
  ungroup() %>% arrange(year, preterm_category) %>%
  mutate(prop_births = N_births/tot_births)

df <-single_preterm_grp %>% filter(preterm_category=="Moderate-Late Preterm")

m_modpre <- glm(prop_births ~ as.numeric(year), 
                  family = binomial("logit"), weights = tot_births, data=df)
gtsummary::tbl_regression(m_modpre , exponentiate = TRUE) # increasep<0.001
mod <- m_modpre
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_modPT_trend_single.rds")

df <-single_preterm_grp %>% filter(preterm_category=="Very Preterm")

m_vpre <- glm(prop_births ~ as.numeric(year), 
              family = binomial("logit"), weights = tot_births, data=df)
gtsummary::tbl_regression(m_vpre , exponentiate = TRUE) # decrease p<0.001
mod <- m_vpre 
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_vPT_trend_single.rds")

df <-single_preterm_grp %>% filter(preterm_category=="Extremely Preterm")

m_xpre <- glm(prop_births ~ as.numeric(year), 
              family = binomial("logit"), weights = tot_births, data=df)
gtsummary::tbl_regression(m_xpre , exponentiate = TRUE)###decrease p=0.003
mod <- m_xpre 
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_exPT_trend_single.rds")

###preterm trends by onset
##
single_onset_grp <- single_gest_onset %>% 
    filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective" &
            onset_of_delivery != "Unknown" &
             preterm_category != "Unknown" & preterm_category != "Non Preterm") %>% 
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>%
  group_by(year, onset_group, preterm_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, onset_group) %>% 
  mutate(tot_preterm_births = sum(N_births)) %>%
  ungroup() %>% arrange(year, preterm_category) %>%
  mutate(prop_births = N_births/tot_preterm_births) # prop preterm in each onset x preterm_cat grp


df <-single_onset_grp %>% filter(preterm_category=="Moderate-Late Preterm")

mod_modpre <- glm(prop_births ~ as.numeric(year)+onset_group , 
                family = binomial("logit"), weights = tot_preterm_births, data=df)
gtsummary::tbl_regression(mod_modpre, exponentiate = TRUE)  #incr w yr
exp(summary(mod_modpre)$coefficients)
#interaction?
mod_modpre <- glm(prop_births ~ as.numeric(year)*onset_group , 
                  family = binomial("logit"), weights = tot_preterm_births, data=df)
summary(mod_modpre) #significant interaction
gtsummary::tbl_regression(mod_modpre, exponentiate = TRUE)  ##gt summary is screwing up here? ...
exp(summary(mod_modpre)$coefficients)# oh, maybe the coeffcient is just huge!


##V preterm
df <-single_onset_grp %>% filter(preterm_category=="Very Preterm")

mod_vpre <- glm(prop_births ~ as.numeric(year)+onset_group , 
                family = binomial("logit"), weights = tot_preterm_births, data=df)
gtsummary::tbl_regression(mod_vpre, exponentiate = TRUE)
#interaction
mod_vpre <- glm(prop_births ~ as.numeric(year)+as.numeric(year)*onset_group , 
                  family = binomial("logit"), weights = tot_preterm_births, data=df)
gtsummary::tbl_regression(mod_vpre, exponentiate = TRUE)# significant interaction yearx onset


##Extremely preterm
df <-single_onset_grp %>% filter(preterm_category=="Extremely Preterm")

mod_xpre <- glm(prop_births ~as.numeric(year)+onset_group , 
                family = binomial("logit"), weights = tot_preterm_births, data=df)
gtsummary::tbl_regression(mod_xpre, exponentiate = TRUE)# significant interaction yearx onset

##All preterm by onset

df <-single_onset_grp %>% group_by(year, onset_group) %>%
  summarise(N_births  = sum(N_births)) %>%
  group_by(year) %>% 
  mutate(tot_preterm_births = sum(N_births)) %>%
  ungroup() %>% mutate(preterm_category="All preterm") %>% 
                       arrange(year, preterm_category) %>%
  mutate(prop_births = N_births/tot_preterm_births) #

mod_pre_all <- glm(prop_births ~ as.numeric(year)+onset_group , 
                  family = binomial("logit"), weights = tot_preterm_births, data=df)
gtsummary::tbl_regression(mod_pre_all, exponentiate = TRUE)  #incr w yr
exp(summary(mod_pre_all)$coefficients)


#####################################
###preterms only - multiples##########
#####################################





###############################
##preterm by onset - models for tables####
####Singletons####
###preterm trends by onset
##
single_onset_grp <- single_gest_onset %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective" &
           onset_of_delivery != "Unknown" &
           preterm_category != "Unknown"# & preterm_category != "Non Preterm"
         ) %>% 
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>%
  group_by(year, onset_group, preterm_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% # denominator is total births in the year
 # mutate(tot_preterm_births = sum(N_births)) %>%
  mutate(tot_births = sum(N_births)) %>%
  ungroup() %>% arrange(year, preterm_category) %>%
  mutate(prop_births = N_births/tot_births) # prop preterm in each onset x preterm_cat grp

##moderate pt, all onset
df <-single_onset_grp %>% filter(preterm_category=="Moderate-Late Preterm")
mod_modpre_all <- glm(prop_births ~ as.numeric(year) , 
                     family = binomial("logit"), weights = tot_births, data=df)
summary(mod_modpre_all )
gtsummary::tbl_regression(mod_modpre_all, exponentiate = TRUE)  #incr w yr
exp(summary(mod_modpre_all)$coefficients)

mod <- mod_modpre_all
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_modlatePT_all_trend_single.rds")



##moderate pt, provider onset
df <-single_onset_grp %>% filter(preterm_category=="Moderate-Late Preterm" &
                                   onset_group=="Provider initiated")

mod_modpre_PI <- glm(prop_births ~ as.numeric(year) , 
                  family = binomial("logit"), weights = tot_births, data=df)
summary(mod_modpre_PI )
gtsummary::tbl_regression(mod_modpre_PI, exponentiate = TRUE)  #incr w yr
exp(summary(mod_modpre_PI)$coefficients)

mod <- mod_modpre_PI
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_modlatePT_PI_trend_single.rds")



#moderate preterm spontaneous
df <-single_onset_grp %>% filter(preterm_category=="Moderate-Late Preterm" & onset_group=="Spontaneous")

mod_modpre_S <- glm(prop_births ~ as.numeric(year), 
                  family = binomial("logit"), weights = tot_births, data=df)
summary(mod_modpre_S ) #
gtsummary::tbl_regression(mod_modpre_S, exponentiate = TRUE)  ##
mod <- mod_modpre_S
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_modlatePT_SP_trend_single.rds")

##very pt, all onset
df <-single_onset_grp %>% filter(preterm_category=="Very Preterm" )

mod_vpre <- glm(prop_births ~ as.numeric(year) , 
                   family = binomial("logit"), weights = tot_births, data=df)
summary(mod_vpre)
gtsummary::tbl_regression(mod_vpre, exponentiate = TRUE)  #incr w yr
exp(summary(mod_vpre )$coefficients)
mod <- mod_vpre
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_vPT_all_trend_single.rds")



##very pt, provider onset
df <-single_onset_grp %>% filter(preterm_category=="Very Preterm" & onset_group=="Provider initiated")

mod_vpre_PI <- glm(prop_births ~ as.numeric(year) , 
                    family = binomial("logit"), weights = tot_births, data=df)
summary(mod_vpre_PI)
gtsummary::tbl_regression(mod_vpre_PI, exponentiate = TRUE)  #incr w yr
exp(summary(mod_vpre_PI )$coefficients)
mod <- mod_vpre_PI
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_vPT_PI_trend_single.rds")


##very pt preterm spontaneous
df <-single_onset_grp %>% filter(preterm_category=="Very Preterm" & onset_group=="Spontaneous")

mod_vpre_S <- glm(prop_births ~ as.numeric(year), 
                   family = binomial("logit"), weights = tot_births, data=df)
summary(mod_vpre_S) #
gtsummary::tbl_regression(mod_vpre_S, exponentiate = TRUE)  ##
mod <- mod_vpre_S
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_vPT_Sp_trend_single.rds")



##extreme pt, provider onset
df <-single_onset_grp %>% filter(preterm_category=="Extremely Preterm")

mod_expre <- glm(prop_births ~ as.numeric(year) , 
                    family = binomial("logit"), weights = tot_births, data=df)
summary(mod_expre)
gtsummary::tbl_regression(mod_expre, exponentiate = TRUE)  #incr w yr
exp(summary(mod_expre)$coefficients)
mod <- mod_expre
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_exPT_all_trend_single.rds")



##extreme pt, provider onset
df <-single_onset_grp %>% filter(preterm_category=="Extremely Preterm" & onset_group=="Provider initiated")

mod_expre_PI <- glm(prop_births ~ as.numeric(year) , 
                   family = binomial("logit"), weights = tot_births, data=df)
summary(mod_expre_PI)
gtsummary::tbl_regression(mod_expre_PI, exponentiate = TRUE)  #incr w yr
exp(summary(mod_expre_PI)$coefficients)


mod <- mod_expre_PI
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_exPT_PI_trend_single.rds")


##extreme pt preterm spontaneous
df <-single_onset_grp %>% filter(preterm_category=="Extremely Preterm" & onset_group=="Spontaneous")

mod_expre_S <- glm(prop_births ~ as.numeric(year), 
                  family = binomial("logit"), weights = tot_births, data=df)
summary(mod_vpre_S) #
gtsummary::tbl_regression(mod_expre_S, exponentiate = TRUE)  ##
mod <- mod_expre_S
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_exPT_Sp_trend_single.rds")




###all preterm by onsets
single_gest_onset <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_single.rds"))%>% filter(year!="2020")
onset_df <- single_gest_onset %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective" &
           onset_of_delivery != "Unknown" &
           preterm_category != "Unknown" #& preterm_category != "Non Preterm"
         ) %>% 
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>%
  group_by(year, onset_group, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% # denominator is total preterms in the year
  mutate(tot_births = sum(N_births)) %>%
  ungroup() %>% arrange(year, birth_category) %>%
  mutate(prop_births = N_births/tot_births) # prop preterm in each onset x preterm_cat grp


##preterm all, 
df <- onset_df %>% filter(birth_category=="Preterm" & onset_group=="Provider initiated")

mod_allpre_PI <- glm(prop_births ~ as.numeric(year) , 
                     family = binomial("logit"), weights = tot_births, data=df)
summary(mod_allpre_PI )
gtsummary::tbl_regression(mod_allpre_PI, exponentiate = TRUE,estimate_fun = purrr::partial(style_ratio, digits = 3),
                          pvalue_fun = purrr::partial(style_sigfig, digits = 3))   #incr w yr
exp(summary(mod_allpre_PI)$coefficients)

mod <- mod_allpre_PI
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_allPT_PI_trend_single.rds")

#preterm all 
df <- onset_df %>% filter(birth_category=="Preterm" & onset_group=="Spontaneous")

mod_allpre_S <- glm(prop_births ~ as.numeric(year) , 
                     family = binomial("logit"), weights = tot_births, data=df)
summary(mod_allpre_S )


mod <- mod_allpre_S
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_allPT_Sp_trend_single.rds")
####################



###############################
##preterm by onset - models for tables####
###Multiples####
###preterm trends by onset

multi_onset_grp <- multi_gest_onset %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective" &
           onset_of_delivery != "Unknown" &
           preterm_category != "Unknown"# & preterm_category != "Non Preterm"
  ) %>% 
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>%
  group_by(year, onset_group, preterm_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% # denominator is total births in the year
  # mutate(tot_preterm_births = sum(N_births)) %>%
  mutate(tot_births = sum(N_births)) %>%
  ungroup() %>% arrange(year, preterm_category) %>%
  mutate(prop_births = N_births/tot_births) # prop preterm in each onset x preterm_cat grp

##moderate pt, all onset
df <- multi_onset_grp %>% filter(preterm_category=="Moderate-Late Preterm")
mod_modpre_all <- glm(prop_births ~ as.numeric(year) , 
                      family = binomial("logit"), weights = tot_births, data=df)
summary(mod_modpre_all )
gtsummary::tbl_regression(mod_modpre_all, exponentiate = TRUE)  #incr w yr
exp(summary(mod_modpre_all)$coefficients)

mod <- mod_modpre_all
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_modlatePT_all_trend_multi.rds")



##moderate pt, provider onset
df <-multi_onset_grp %>% filter(preterm_category=="Moderate-Late Preterm" &
                                   onset_group=="Provider initiated")

mod_modpre_PI <- glm(prop_births ~ as.numeric(year) , 
                     family = binomial("logit"), weights = tot_births, data=df)
summary(mod_modpre_PI )
gtsummary::tbl_regression(mod_modpre_PI, exponentiate = TRUE)  #incr w yr
exp(summary(mod_modpre_PI)$coefficients)

mod <- mod_modpre_PI
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_modlatePT_PI_trend_multi.rds")



#moderate preterm spontaneous
df <-multi_onset_grp %>% filter(preterm_category=="Moderate-Late Preterm" & onset_group=="Spontaneous")

mod_modpre_S <- glm(prop_births ~ as.numeric(year), 
                    family = binomial("logit"), weights = tot_births, data=df)
summary(mod_modpre_S ) #
gtsummary::tbl_regression(mod_modpre_S, exponentiate = TRUE)  ##
mod <- mod_modpre_S
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_modlatePT_SP_trend_multi.rds")

##very pt, all onset
df <- multi_onset_grp %>% filter(preterm_category=="Very Preterm" )

mod_vpre <- glm(prop_births ~ as.numeric(year) , 
                family = binomial("logit"), weights = tot_births, data=df)
summary(mod_vpre)
gtsummary::tbl_regression(mod_vpre, exponentiate = TRUE)  #incr w yr
exp(summary(mod_vpre )$coefficients)
mod <- mod_vpre
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_vPT_all_trend_multi.rds")



##very pt, provider onset
df <-multi_onset_grp %>% filter(preterm_category=="Very Preterm" & onset_group=="Provider initiated")

mod_vpre_PI <- glm(prop_births ~ as.numeric(year) , 
                   family = binomial("logit"), weights = tot_births, data=df)
summary(mod_vpre_PI)
gtsummary::tbl_regression(mod_vpre_PI, exponentiate = TRUE)  #incr w yr
exp(summary(mod_vpre_PI )$coefficients)
mod <- mod_vpre_PI
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_vPT_PI_trend_multi.rds")


##very pt preterm spontaneous
df <-multi_onset_grp %>% filter(preterm_category=="Very Preterm" & onset_group=="Spontaneous")

mod_vpre_S <- glm(prop_births ~ as.numeric(year), 
                  family = binomial("logit"), weights = tot_births, data=df)
summary(mod_vpre_S) #
gtsummary::tbl_regression(mod_vpre_S, exponentiate = TRUE)  ##
mod <- mod_vpre_S
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_vPT_Sp_trend_multi.rds")



##extreme pt, provider onset
df <-multi_onset_grp %>% filter(preterm_category=="Extremely Preterm")

mod_expre <- glm(prop_births ~ as.numeric(year) , 
                 family = binomial("logit"), weights = tot_births, data=df)
summary(mod_expre)
gtsummary::tbl_regression(mod_expre, exponentiate = TRUE)  #incr w yr
exp(summary(mod_expre)$coefficients)
mod <- mod_expre
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_exPT_all_trend_multi.rds")



##extreme pt, provider onset
df <-multi_onset_grp %>% filter(preterm_category=="Extremely Preterm" & onset_group=="Provider initiated")

mod_expre_PI <- glm(prop_births ~ as.numeric(year) , 
                    family = binomial("logit"), weights = tot_births, data=df)
summary(mod_expre_PI)
gtsummary::tbl_regression(mod_expre_PI, exponentiate = TRUE)  #incr w yr
exp(summary(mod_expre_PI)$coefficients)


mod <- mod_expre_PI
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_exPT_PI_trend_multi.rds")


##extreme pt preterm spontaneous
df <-multi_onset_grp %>% filter(preterm_category=="Extremely Preterm" & onset_group=="Spontaneous")

mod_expre_S <- glm(prop_births ~ as.numeric(year), 
                   family = binomial("logit"), weights = tot_births, data=df)
summary(mod_vpre_S) #
gtsummary::tbl_regression(mod_expre_S, exponentiate = TRUE)  ##
mod <- mod_expre_S
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_exPT_Sp_trend_multi.rds")

###all preterm by onsets
multi_gest_onset <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_multi.rds"))%>% filter(year!="2020")
onset_df <- multi_gest_onset %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective" &
           onset_of_delivery != "Unknown" &
           preterm_category != "Unknown" #& preterm_category != "Non Preterm"
  ) %>% 
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>%
  group_by(year, onset_group, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% # denominator is total preterms in the year
  mutate(tot_births = sum(N_births)) %>%
  ungroup() %>% arrange(year, birth_category) %>%
  mutate(prop_births = N_births/tot_births) # prop preterm in each onset x preterm_cat grp


##preterm all, 
df <- onset_df %>% filter(birth_category=="Preterm" & onset_group=="Provider initiated")

mod_allpre_PI <- glm(prop_births ~ as.numeric(year) , 
                     family = binomial("logit"), weights = tot_births, data=df)
summary(mod_allpre_PI )
gtsummary::tbl_regression(mod_allpre_PI, exponentiate = TRUE,estimate_fun = purrr::partial(style_ratio, digits = 3),
                          pvalue_fun = purrr::partial(style_sigfig, digits = 3))   #incr w yr
exp(summary(mod_allpre_PI)$coefficients)

mod <- mod_allpre_PI
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_allPT_PI_trend_multi.rds")

#preterm all 
df <- onset_df %>% filter(birth_category=="Preterm" & onset_group=="Spontaneous")

mod_allpre_S <- glm(prop_births ~ as.numeric(year) , 
                    family = binomial("logit"), weights = tot_births, data=df)
summary(mod_allpre_S )


mod <- mod_allpre_S
coeff_yr <- exp(summary(mod)$coeff[2])
confint_yr <- as.data.frame(exp(confint(mod)))[2,]
mod_values <- cbind(coeff_yr, confint_yr)
saveRDS(mod_values, "tables/regression_tables/yrvalues_allPT_Sp_trend_multi.rds")
####################

