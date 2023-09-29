##########################################
#05. Tables for paper####
############################


###setup####


#load data.####
single_gest_weeks <- readRDS(paste0(data_path, "2_working_data/gest_wks_single.rds"))%>% filter(year!="2020")
single_gest_age <- readRDS(paste0(data_path, "2_working_data/gest_by_age_single.rds"))%>% filter(year!="2020")
single_gest_simd <-  readRDS(paste0(data_path,"2_working_data/gest_by_simd_single.rds"))%>% filter(year!="2020")
single_gest_onset <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_single.rds"))%>% filter(year!="2020")

multi_gest_weeks <- readRDS(paste0(data_path, "2_working_data/gest_wks_multi.rds"))%>% filter(year!="2020")
multi_gest_age <- readRDS(paste0(data_path, "2_working_data/gest_by_age_multi.rds"))%>% filter(year!="2020")
multi_gest_simd <-  readRDS(paste0(data_path,"2_working_data/gest_by_simd_multi.rds"))%>% filter(year!="2020")
multi_gest_onset <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_multi.rds"))%>% filter(year!="2020")

#Table 1 numbers####
##numbers for main table in text and for supplementary table 1
##numbers and rates by term group. Preterms by onset and by subgroup and nonset
##need to also add model p values for each as well at some point.
##singletons####
single_gest_year_totals_incl_unknown <- single_gest_weeks %>%
  #filter(birth_category != "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  summarise(total_births_yr_incl = sum(n_births)) %>%
  ungroup() 

single_gest_year_totals_excl_unknown <- single_gest_weeks %>%
  filter(birth_category != "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  summarise(total_births_yr_excl_unkn = sum(n_births)) %>%
  ungroup() 

single_ptb_onset_totals_excl_unkn <- single_gest_onset %>%
  filter(birth_category != "Unknown" & onset_of_delivery != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  summarise(total_births_yr_excl_unkn_onset = sum(n_births)) %>%
  ungroup() 

single_gest_year_totals <-
  left_join(single_gest_year_totals_incl_unknown, single_gest_year_totals_excl_unknown) 

single_gest_year_totals <-
  left_join(single_gest_year_totals,single_ptb_onset_totals_excl_unkn )


single_gest_groups <- single_gest_weeks %>%
  filter(birth_category != "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  ungroup()  %>%
  left_join(single_gest_year_totals) %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn *100) %>%
  mutate(onset_group ="all", preterm_category = " ") 


#paste unknown on the end but dont calculate totals, rates etc
single_gest_unkn <- single_gest_weeks %>%
  filter(birth_category == "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  left_join(single_gest_year_totals) %>%
  mutate(rate_per_100 = NA) %>%
  mutate(onset_group ="all", preterm_category = " ") 

single_gest_groups <- rbind(single_gest_groups, single_gest_unkn)


single_ptb_subgrp_onset <- single_gest_onset %>%  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
  left_join(single_gest_year_totals) %>%
  left_join(single_ptb_onset_totals_excl_unkn) %>%
  filter(birth_category=="Preterm") %>% 
  group_by(year,total_births_yr_excl_unkn,total_births_yr_incl, total_births_yr_excl_unkn_onset , birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn_onset  *100)

single_ptb_onset <- single_ptb_subgrp_onset %>% mutate(preterm_category="All") %>%
  group_by(year,total_births_yr_excl_unkn_onset ,total_births_yr_excl_unkn, total_births_yr_incl, birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn_onset  *100)

#preterm subgroup but ALL onset (include unknowns in denominator)
single_ptb_subgroup <- single_ptb_subgrp_onset %>% mutate( onset_group ="All onset") %>%
  group_by(year,total_births_yr_excl_unkn_onset ,total_births_yr_excl_unkn, total_births_yr_incl, birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn  *100)


tab1 <- rbind(single_gest_groups, single_ptb_onset, single_ptb_subgrp_onset, single_ptb_subgroup )


tab1_num <- tab1 %>%
  mutate(category_of_singleton_births = paste0(birth_category, "-",preterm_category, "-", onset_group )) %>%
  select(-c(rate_per_100, total_births_yr_incl, total_births_yr_excl_unkn, total_births_yr_excl_unkn_onset)) %>%
  pivot_wider(names_from = year, values_from = c(n_births)) %>% 
  mutate(value_type = "Number") %>%
  mutate(totals = rowSums(select(., `2005`:`2019`))) %>%
         mutate(difference =`2019`-`2005`)

tab1_rate <-tab1 %>%
  mutate(category_of_singleton_births = paste0(birth_category, "-",preterm_category, "-", onset_group )) %>%
  select(-c(n_births, total_births_yr_incl, total_births_yr_excl_unkn_onset, total_births_yr_excl_unkn))  %>%
  pivot_wider(names_from = year, values_from = c(rate_per_100)) %>% 
  mutate(value_type = "Rate") %>% mutate(totals = NA, difference= NA)

total_births_yr_incl <- single_gest_groups %>% select(year, total_births_yr_incl) %>% unique() %>%
  filter(!is.na(total_births_yr_incl)) %>%
  pivot_wider(names_from  = year, values_from = total_births_yr_incl) %>% 
  mutate(value_type = "Number",category_of_singleton_births = "Total Births" ) %>%
  mutate(totals = rowSums(select(., `2005`:`2019`))) %>%
  mutate(difference =`2019`-`2005`) %>%
  mutate(birth_category="All", preterm_category="None", onset_group="All")

total_births_yr_ex <- single_gest_groups %>% select(year, total_births_yr_excl_unkn) %>% 
  unique() %>%
  filter(!is.na(total_births_yr_excl_unkn)) %>%
  pivot_wider(names_from  = year, values_from = total_births_yr_excl_unkn) %>% 
  mutate(value_type = "Number",category_of_singleton_births = "Total Births excluding unknown gestation" ) %>%
  mutate(totals = rowSums(select(., `2005`:`2019`))) %>%
  mutate(difference =`2019`-`2005`) %>%
  mutate(birth_category="All", preterm_category="None", onset_group="All")

total_births_yr_ex_onset <- single_gest_groups %>% select(year, total_births_yr_excl_unkn_onset) %>% 
  unique() %>%
  filter(!is.na(total_births_yr_excl_unkn_onset)) %>%
  pivot_wider(names_from  = year, values_from = total_births_yr_excl_unkn_onset) %>% 
  mutate(value_type = "Number",category_of_singleton_births = "Total Births excluding unknown gestation and unknown onset" ) %>%
  mutate(totals = rowSums(select(., `2005`:`2019`))) %>%
  mutate(difference =`2019`-`2005`) %>%
  mutate(birth_category="All", preterm_category="None", onset_group="All")


table1 <- rbind(total_births_yr_ex, total_births_yr_ex_onset ,total_births_yr_incl, tab1_num, tab1_rate)

table1 <- table1 %>% select(category_of_singleton_births, value_type, everything()) %>%
  arrange(category_of_singleton_births)


row_total_rates_denom <- table1$totals[table1$category_of_singleton_births=="Total Births excluding unknown gestation"]
row_total_rates_denom_onset <- table1$totals[table1$category_of_singleton_births=="Total Births excluding unknown gestation and unknown onset" ]

rates_df <- table1 %>% select(category_of_singleton_births, totals) %>% filter(!is.na(totals)) %>% 
  filter(!category_of_singleton_births %in% c("Unknown- -all","Total Births excluding unknown gestation","Total Births")) %>% 
  mutate(rate_overall = totals/row_total_rates_denom *100)

tab1_rates_df <- table1 %>% filter(value_type=="Rate") %>% 
  left_join(rates_df %>% select(category_of_singleton_births, rate_overall)) %>%
  select(-totals) %>% rename( totals=rate_overall)

table1 <- rbind(table1 %>% filter(value_type!="Rate"), tab1_rates_df)%>%
  mutate(difference =`2019`-`2005`) 

##sort labels
table1 <- table1 %>%
  mutate(labels = case_when(category_of_singleton_births =="Preterm- -all" ~ "Preterm Births (<37 weeks gestation)", 
                            category_of_singleton_births =="Unknown- -all" ~ "Unknown gestation",
                            category_of_singleton_births =="Post-term- -all" ~ "Post-term Births (>41 weeks gestation)",
                            category_of_singleton_births =="Term- -all" ~ "Term Births (37-41 weeks gestation)", 
                            category_of_singleton_births =="Preterm-All-Provider initiated" ~ "Provider initiated Preterm Births (<37 weeks gestation)",
                            category_of_singleton_births =="Preterm-All-Spontaneous" ~ "Spontaneous Preterm Births (<37 weeks gestation)",  
                            category_of_singleton_births =="Preterm-All-Unknown" ~ "Unknown onset Preterm Births (<37 weeks gestation)", 
                            category_of_singleton_births =="Preterm-Extremely Preterm-All onset" ~ "Extremely Preterm Births (<28 weeks gestation)",
                            category_of_singleton_births =="Preterm-Extremely Preterm-Provider initiated" ~ "Provider initiated Extremely Preterm Births (<28 weeks gestation)",
                            category_of_singleton_births =="Preterm-Extremely Preterm-Spontaneous" ~ "Spontaneous Extremely Preterm Births (<28 weeks gestation)",
                            category_of_singleton_births =="Preterm-Moderate-Late Preterm-Spontaneous" ~ "Spontaneous Moderate-Late Preterm Births (32-36 weeks gestation)",
                            category_of_singleton_births =="Preterm-Moderate-Late Preterm-Provider initiated" ~ "Provider initiated Moderate-Late Preterm Births (32-36 weeks gestation)",
                            category_of_singleton_births =="Preterm-Moderate-Late Preterm-All onset" ~ "Moderate-Late Preterm Births (32-36 weeks gestation)",
                            category_of_singleton_births =="Preterm-Very Preterm-All onset" ~ "Very Preterm Births (28-31 weeks gestation)",
                            category_of_singleton_births =="Preterm-Very Preterm-Provider initiated" ~ "Provider initiated Very Preterm Births (28-31 weeks gestation)",
                            category_of_singleton_births =="Preterm-Very Preterm-Spontaneous" ~ "Spontaneous Very Preterm Births (28-31 weeks gestation)",
                            TRUE ~ category_of_singleton_births)
  ) %>% select(labels, everything())


saveRDS(table1, "tables/table1_single.rds")

##split table into preterm onset/ overall rates

table1a <- table1 %>% filter(str_detect(category_of_singleton_births,"all") | str_detect(category_of_singleton_births,"All")| str_detect(category_of_singleton_births,"Total")  )
table1b <- table1 %>% filter(!str_detect(category_of_singleton_births,"all") & !str_detect(category_of_singleton_births,"All") & !str_detect(category_of_singleton_births,"Total")  )

supp_tab1a <- table1a %>% 
  filter(str_ends(category_of_singleton_births, pattern= "all")| str_detect(category_of_singleton_births,"Total"))
supp_tab1a <- supp_tab1a %>% mutate(rel_difference = difference/`2005`)

supp_tab1a <- supp_tab1a%>%
  select( labels, category_of_singleton_births, value_type, totals, everything() ) %>%
  select(-c(birth_category, preterm_category, onset_group))

write.csv(supp_tab1a, paste0(output_path, "Supp1a.csv"))

#Multiples supp table 1b  ####
##numbers and rates by term group. Preterms by onset and by subgroup and onset
##need to also add model p values for each as well at some point.
multi_gest_year_totals_incl_unknown <- multi_gest_weeks %>%
  #filter(birth_category != "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  summarise(total_births_yr_incl = sum(n_births)) %>%
  ungroup() 

multi_gest_year_totals_excl_unknown <- multi_gest_weeks %>%
  filter(birth_category != "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  summarise(total_births_yr_excl_unkn = sum(n_births)) %>%
  ungroup() 

multi_gest_year_totals <- left_join(multi_gest_year_totals_incl_unknown, multi_gest_year_totals_excl_unknown)


multi_gest_groups <- multi_gest_weeks %>%
  filter(birth_category != "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  ungroup()  %>%
  left_join(multi_gest_year_totals) %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn *100) %>%
  mutate(onset_group ="all", preterm_category = " ") 


#paste unknown on the end but dont calculate totals, rates etc
multi_gest_unkn <- multi_gest_weeks %>%
  filter(birth_category == "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  left_join(multi_gest_year_totals) %>%
  mutate(rate_per_100 = NA) %>%
  mutate(onset_group ="all", preterm_category = " ") 

multi_gest_groups <- rbind(multi_gest_groups, multi_gest_unkn)


multi_gest_year_totals <- left_join(multi_gest_year_totals_incl_unknown, multi_gest_year_totals_excl_unknown)


multi_ptb_subgrp_onset <- multi_gest_onset %>%
  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
  filter(onset_group!="Unknown") %>%
  left_join(multi_gest_year_totals) %>%
  filter(birth_category=="Preterm") %>% 
  group_by(year,total_births_yr_excl_unkn,total_births_yr_incl, birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn *100)

multi_ptb_onset <- multi_ptb_subgrp_onset %>% mutate(preterm_category="All") %>%
  group_by(year,total_births_yr_excl_unkn, total_births_yr_incl, birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn *100)



multi_ptb_subgroup <- multi_gest_onset %>% mutate( onset_group ="All onset") %>%
  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
  left_join(multi_gest_year_totals) %>%
  filter(birth_category=="Preterm") %>% 
  group_by(year,total_births_yr_excl_unkn,total_births_yr_incl, birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn *100)


tab1 <- rbind(multi_gest_groups, multi_ptb_onset, multi_ptb_subgrp_onset, multi_ptb_subgroup )%>% 
  filter(birth_category !="Unknown") %>% 
  mutate(category_of_multiple_births = paste0(birth_category, "-",preterm_category, "-", onset_group )) %>% unique()


tab1_num <- tab1 %>% filter(birth_category !="Unknown") %>%
  mutate(category_of_multiple_births = paste0(birth_category, "-",preterm_category, "-", onset_group )) %>%
  select(-c(rate_per_100, total_births_yr_incl, total_births_yr_excl_unkn)) %>%
  pivot_wider(names_from = year, values_from = c(n_births)) %>% 
  mutate(value_type = "Number") %>%
  mutate(totals = rowSums(select(., `2005`:`2019`))) %>%
  mutate(difference =`2019`-`2005`)


tab1_rate <-tab1 %>%
  mutate(category_of_multiple_births = paste0(birth_category, "-",preterm_category, "-", onset_group )) %>%
  select(-c(n_births, total_births_yr_incl, total_births_yr_excl_unkn))  %>%
  pivot_wider(names_from = year, values_from = c(rate_per_100)) %>% 
  mutate(value_type = "Rate") %>% mutate(totals = NA, difference= NA)

total_births_yr_incl <- multi_gest_groups %>% select(year, total_births_yr_incl) %>% unique() %>%
  filter(!is.na(total_births_yr_incl)) %>%
  pivot_wider(names_from  = year, values_from = total_births_yr_incl) %>% 
  mutate(value_type = "Number",category_of_multiple_births = "Total Births" ) %>%
  mutate(totals = rowSums(select(., `2005`:`2019`))) %>%
  mutate(difference =`2019`-`2005`) %>%
  mutate(birth_category="All", preterm_category="None", onset_group="All")

total_births_yr_ex <- multi_gest_groups %>% select(year, total_births_yr_excl_unkn) %>% unique() %>%
  filter(!is.na(total_births_yr_excl_unkn)) %>%
  pivot_wider(names_from  = year, values_from = total_births_yr_excl_unkn) %>% 
  mutate(value_type = "Number",category_of_multiple_births = "Total Births excluding unknown gestation" ) %>%
  mutate(totals = rowSums(select(., `2005`:`2019`))) %>%
  mutate(difference =`2019`-`2005`) %>%
  mutate(birth_category="All", preterm_category="None", onset_group="All")

table1 <- rbind(total_births_yr_ex, total_births_yr_incl, tab1_num, tab1_rate)


table1 <- table1 %>% select(category_of_multiple_births, value_type, everything()) %>%
  arrange(category_of_multiple_births)


row_total_rates_denom <- table1$totals[table1$category_of_multiple_births=="Total Births excluding unknown gestation"]

rates_df <- table1 %>% select(category_of_multiple_births, totals) %>% filter(!is.na(totals)) %>% 
  filter(!category_of_multiple_births %in% c("Unknown- -all","Total Births excluding unknown gestation","Total Births")) %>% 
  mutate(rate_overall = totals/row_total_rates_denom *100)

tab1_rates_df <- table1 %>% filter(value_type=="Rate") %>% 
  left_join(rates_df %>% select(category_of_multiple_births, rate_overall)) %>%
  select(-totals) %>% rename( totals=rate_overall)

table1 <- rbind(table1 %>% filter(value_type!="Rate"), tab1_rates_df)%>%
  mutate(difference =`2019`-`2005`) 


##sort labels
table1 <- table1 %>%
  mutate(labels = case_when(category_of_multiple_births =="Preterm- -all" ~ "Preterm Births (<37 weeks gestation)", 
                            category_of_multiple_births =="Unknown- -all" ~ "Unknown gestation",
                            category_of_multiple_births =="Post-term- -all" ~ "Post-term Births (>41 weeks gestation)",
                            category_of_multiple_births =="Term- -all" ~ "Term Births (37-41 weeks gestation)", 
                            category_of_multiple_births =="Preterm-All-Provider initiated" ~ "Provider initiated Preterm Births (<37 weeks gestation)",
                            category_of_multiple_births =="Preterm-All-Spontaneous" ~ "Spontaneous Preterm Births (<37 weeks gestation)",  
                            category_of_multiple_births =="Preterm-All-Unknown" ~ "Unknown onset Preterm Births (<37 weeks gestation)", 
                            category_of_multiple_births =="Preterm-Extremely Preterm-All onset" ~ "Extremely Preterm Births (<28 weeks gestation)",
                            category_of_multiple_births =="Preterm-Extremely Preterm-Provider initiated" ~ "Provider initiated Extremely Preterm Births (<28 weeks gestation)",
                            category_of_multiple_births =="Preterm-Extremely Preterm-Spontaneous" ~ "Spontaneous Extremely Preterm Births (<28 weeks gestation)",
                            category_of_multiple_births =="Preterm-Moderate-Late Preterm-Spontaneous" ~ "Spontaneous Moderate-Late Preterm Births (32-36 weeks gestation)",
                            category_of_multiple_births =="Preterm-Moderate-Late Preterm-Provider initiated" ~ "Provider initiated Moderate-Late Preterm Births (32-36 weeks gestation)",
                            category_of_multiple_births =="Preterm-Moderate-Late Preterm-All onset" ~ "Moderate-Late Preterm Births (32-36 weeks gestation)",
                            category_of_multiple_births =="Preterm-Very Preterm-All onset" ~ "Very Preterm Births (28-31 weeks gestation)",
                            category_of_multiple_births =="Preterm-Very Preterm-Provider initiated" ~ "Provider initiated Very Preterm Births (28-31 weeks gestation)",
                            category_of_multiple_births =="Preterm-Very Preterm-Spontaneous" ~ "Spontaneous Very Preterm Births (28-31 weeks gestation)",
                            TRUE ~ category_of_multiple_births)
  ) %>% select(labels, everything())


write_csv(table1, "tables/table1_multiples.csv")
saveRDS(table1, "tables/table1_multiples.rds")
table2 <- table1 %>% filter(str_detect(category_of_multiple_births,"all") | 
                              str_detect(category_of_multiple_births,"All")| 
                              str_detect(category_of_multiple_births,"Total")  )
#table2b <- table1 %>% filter(!str_detect(category_of_singleton_births,"all") & !str_detect(category_of_singleton_births,"All") & !str_detect(category_of_singleton_births,"Total")  )

supp_tab2b <- table2 %>% 
  filter(str_ends(category_of_multiple_births, pattern= "all")| str_detect(category_of_multiple_births,"Total"))
supp_tab2b <- supp_tab2b %>% mutate(rel_difference = difference/`2005`)

supp_tab2b <- supp_tab2b %>%
  select( labels, category_of_multiple_births, value_type, totals, everything() ) %>%
  select(-c(birth_category, preterm_category, onset_group))

write.csv(supp_tab2b, paste0(output_path, "Supp1b.csv"))


#Main table singletons#####
##confidence intervals for the % of births by GA category (% of births in year)
single_gest_groups  <- single_gest_groups %>% filter(!is.na(birth_category))%>%
  group_by(year) %>% mutate(#rate_per_100 = MultinomCI( c(n_births),  conf.level=0.95 )[,"est"], 
    rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
    rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100) %>% 
  ungroup() 
###Confidence intervals for the difference between 2005 and 2019 - 
diff_data_s<- single_gest_groups %>% 
  filter(!is.na(birth_category) & (year==2005 | year==2019)) %>%
  mutate(n_not_bc = total_births_yr_excl_unkn-n_births) %>% as.data.frame() %>% 
  select(year, birth_category, n_births, n_not_bc,total_births_yr_excl_unkn) %>%
  mutate(type = "singleton")

diff_data_m<- multi_gest_groups %>% 
  filter(!is.na(birth_category) & (year==2005 | year==2019)) %>%
  mutate(n_not_bc = total_births_yr_excl_unkn-n_births) %>% as.data.frame() %>% 
  select(year, birth_category, n_births, n_not_bc,total_births_yr_excl_unkn) %>%
  mutate(type = "multiple")

diff_data <- rbind(diff_data_s, diff_data_m)

diff_data <- diff_data %>% group_by(year, type) %>% 
  mutate(rate_per_100 = MultinomCI( c(n_births),  conf.level=0.95 )[,"est"]*100, 
         rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
         rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)


diffs <- diff_data %>% ungroup() %>% filter(birth_category != "Unknown") %>% 
 arrange(desc(year)) %>%
  group_by(type,birth_category ) %>% 
  #  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(n_births, total_births_yr_excl_unkn, conf.level=0.95)))) %>%
  tidyr::unnest(tst) %>% rename(abs_change_lci = conf.low, abs_change_uci = conf.high) %>%
  ungroup() %>% mutate(abs_change_rate = estimate1-estimate2) %>%
  select(birth_category, type, abs_change_rate,abs_change_lci, abs_change_uci) %>%
  unique()


diff_data <- diff_data  %>% ungroup() %>%
  pivot_wider(id_cols= c(birth_category, type),names_from = year, values_from =c(n_births, n_not_bc, total_births_yr_excl_unkn, rate_per_100,  rate_per_100_lci,  rate_per_100_uci ))%>%
  left_join(diffs)# %>%
#  rename(abs_change_rate = estimate ,abs_change_lci =  lower , abs_change_uci =  upper )

diff_data <- diff_data %>%  mutate(z_crit = 1.96, 
                                   p1 = rate_per_100_2005/100, p2 = rate_per_100_2019/100,
                                   cv1 = sqrt(p1*(1-p1)),
                                   cv2 = sqrt(p2*(1-p2)),
                                   relative_diff_perc = (p2-p1)/p1 *100) %>% 
  as.data.frame() %>% 
  mutate(relative_difference_lci  = reldiffci(x=relative_diff_perc, z= z_crit, cv1=cv1, cv2=cv2)$lci, 
         relative_difference_uci  = reldiffci(x=relative_diff_perc, z= z_crit, cv1=cv1, cv2=cv2)$uci ) %>%
  select(-c(z_crit, p1, p2, cv1, cv2))

diff_tab <- diff_data %>% select(type, birth_category,
                                 rate_per_100_2005, rate_per_100_lci_2005, rate_per_100_uci_2005, 
                                 rate_per_100_2019, rate_per_100_lci_2019, rate_per_100_uci_2019,
                                 abs_change_rate, abs_change_lci, abs_change_uci, 
                                 relative_diff_perc, relative_difference_lci, relative_difference_uci) %>%
  mutate(rate_per_100_livebirths_2005 =  paste0(format(round(rate_per_100_2005,2), nsmall=2)," (",
                                                format(round(rate_per_100_lci_2005,2), nsmall=2)," - ",
                                                format(round(rate_per_100_uci_2005,2), nsmall=2),")"), 
         rate_per_100_livebirths_2019 =  paste0(format(round(rate_per_100_2019,2), nsmall=2)," (",
                                                format(round(rate_per_100_lci_2019,2), nsmall=2)," - ",
                                                format(round(rate_per_100_uci_2019,2), nsmall=2),")"), 
         Absolute_difference = paste0(format(round(abs_change_rate*100,2), nsmall=2), " (",
                                      format(round(abs_change_lci*100,2), nsmall=2), " - ",
                                      format(round(abs_change_uci*100,2), nsmall=2), ")"), 
         Relative_difference = paste0(format(round(relative_diff_perc,2), nsmall=2), "% (",
                                      format(round(relative_difference_lci, 2), nsmall=2)," - ",
                                      format(round(relative_difference_uci, 2), nsmall=2),")"))

write_csv(diff_tab, paste0(output_path, "ratechange1_table.csv"))


diff_tab <- diff_data %>% select(type, birth_category,
                                 rate_per_100_2005, rate_per_100_lci_2005, rate_per_100_uci_2005, 
                                 rate_per_100_2019, rate_per_100_lci_2019, rate_per_100_uci_2019,
                                 abs_change_rate, abs_change_lci, abs_change_uci, 
                                 relative_diff_perc, relative_difference_lci, relative_difference_uci) %>%
  mutate(rate_per_100_livebirths_2005 =  paste0(format(round(rate_per_100_2005,1), nsmall=1)," (",
                                                format(round(rate_per_100_lci_2005,1), nsmall=1)," - ",
                                                format(round(rate_per_100_uci_2005,1), nsmall=1),")"), 
         rate_per_100_livebirths_2019 =  paste0(format(round(rate_per_100_2019,1), nsmall=1)," (",
                                                format(round(rate_per_100_lci_2019,1), nsmall=1)," - ",
                                                format(round(rate_per_100_uci_2019,1), nsmall=1),")"), 
         Absolute_difference = paste0(format(round(abs_change_rate*100,1), nsmall=1), " (",
                                      format(round(abs_change_lci*100,1), nsmall=1), " - ",
                                      format(round(abs_change_uci*100,1), nsmall=1), ")"), 
         Relative_difference = paste0(format(round(relative_diff_perc,1), nsmall=1), "% (",
                                      format(round(relative_difference_lci, 1), nsmall=1)," - ",
                                      format(round(relative_difference_uci, 1), nsmall=1),")"))

write_csv(diff_tab, paste0(output_path, "ratechange1_table_1dp.csv"))
ratechange1 <- diff_tab

###table of preterm onsets with CIs ####
#singletons
#include all onset and unknown onset for gestation subgroups
single_ptb_subgrp_allonsets <- single_gest_onset %>%  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
#  filter(onset_group != "Unknown") %>%
  group_by(year) %>% 
  mutate(total_births_yr_excl_unkn = sum(N)) %>%
  ungroup() %>%
  group_by(year,total_births_yr_excl_unkn ,birth_category, preterm_category) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn  *100) %>%
  group_by(year) %>%
  mutate( rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
          rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)# %>%


single_ptb_subgrp_onset <- single_gest_onset %>%  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
  filter(onset_group != "Unknown") %>%
  group_by(year) %>% 
  mutate(total_births_yr_excl_unkn = sum(N)) %>%
  ungroup() %>%
  group_by(year,total_births_yr_excl_unkn ,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn  *100) %>%
  group_by(year) %>%
  mutate( rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
          rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)# %>%
# filter(birth_category=="Preterm") 

single_ptb_onset <- single_ptb_subgrp_onset %>% mutate(preterm_category="All") %>%
  filter(onset_group != "Unknown") %>% 
  group_by(year,total_births_yr_excl_unkn,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn *100) %>%
  group_by(year) %>%
  mutate( rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
          rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)
single_ptb_subgrp_allonsets <- single_gest_onset %>%  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
  #  filter(onset_group != "Unknown") %>%
  group_by(year) %>% 
  mutate(total_births_yr_excl_unkn = sum(N)) %>%
  ungroup() %>%
  group_by(year,total_births_yr_excl_unkn ,birth_category, preterm_category) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn  *100) %>%
  group_by(year) %>%
  mutate( rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
          rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)# %>%



single_ptb_subgrp_allonsets <- single_gest_onset %>%  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
  #  filter(onset_group != "Unknown") %>%
  group_by(year) %>% 
  mutate(total_births_yr_excl_unkn = sum(N)) %>%
  ungroup() %>%
  group_by(year,total_births_yr_excl_unkn ,birth_category, preterm_category) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate( onset_group ="All onset") %>%
  mutate(rate_per_100 = n_births/total_births_yr_excl_unkn  *100) %>%
  group_by(year) %>%
  mutate( rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
          rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)# %>%



diff_data <- rbind(single_ptb_subgrp_onset %>% filter(birth_category== "Preterm" & (year==2005 | year==2019)),  
                   single_ptb_onset %>% filter(birth_category== "Preterm" & (year==2005 | year==2019)),
                   single_ptb_subgrp_allonsets  %>% filter(birth_category== "Preterm" & (year==2005 | year==2019))) %>%
  mutate(n_not_bc  = total_births_yr_excl_unkn -n_births)

diffs <-  diff_data %>% ungroup() %>% filter(birth_category != "Unknown") %>% 
  arrange(desc(year),preterm_category ,onset_group) %>%
  group_by(preterm_category ,onset_group) %>% 
  #  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(n_births, total_births_yr_excl_unkn, conf.level=0.95)))) %>%
  tidyr::unnest(tst) %>% rename(abs_change_lci = conf.low, abs_change_uci = conf.high) %>%
  ungroup() %>% mutate(abs_change_rate = estimate1-estimate2) %>%
  select(preterm_category ,onset_group, abs_change_rate,abs_change_lci, abs_change_uci) %>%
  unique()

diff_data <- diff_data %>% ungroup() %>%
  pivot_wider(id_cols= c(birth_category, preterm_category,   onset_group),
              names_from = year, values_from =c(n_births, n_not_bc, total_births_yr_excl_unkn, rate_per_100,  rate_per_100_lci,  rate_per_100_uci ))%>%
  left_join(diffs) #%>%
#rename(abs_change_rate = estimate ,abs_change_lci =  lower , abs_change_uci =  upper ) %>%
# select(-method, -compnames)

diff_data <- diff_data %>%  mutate(z_crit = 1.96, 
                                   p1 = rate_per_100_2005/100, p2 = rate_per_100_2019/100,
                                   cv1 = sqrt(p1*(1-p1)),
                                   cv2 = sqrt(p2*(1-p2)),
                                   relative_diff_perc = (p2-p1)/p1 *100) %>% 
  as.data.frame() %>% 
  mutate(relative_difference_lci  = reldiffci(x=relative_diff_perc, z= z_crit, cv1=cv1, cv2=cv2)$lci, 
         relative_difference_uci  = reldiffci(x=relative_diff_perc, z= z_crit, cv1=cv1, cv2=cv2)$uci ) %>%
  select(-c(z_crit, p1, p2, cv1, cv2))


diff_tab <- diff_data %>% select(birth_category, preterm_category   ,   onset_group,
                                 rate_per_100_2005, rate_per_100_lci_2005, rate_per_100_uci_2005, 
                                 rate_per_100_2019, rate_per_100_lci_2019, rate_per_100_uci_2019,
                                 abs_change_rate, abs_change_lci, abs_change_uci, 
                                 relative_diff_perc, relative_difference_lci, relative_difference_uci) %>%
  mutate(rate_per_100_livebirths_2005 =  paste0(format(round(rate_per_100_2005,2), nsmall=2)," (",
                                                format(round(rate_per_100_lci_2005,2), nsmall=2)," - ",
                                                format(round(rate_per_100_uci_2005,2), nsmall=2),")"), 
         rate_per_100_livebirths_2019 =  paste0(format(round(rate_per_100_2019,2), nsmall=2)," (",
                                                format(round(rate_per_100_lci_2019,2), nsmall=2)," - ",
                                                format(round(rate_per_100_uci_2019,2), nsmall=2),")"), 
         Absolute_difference = paste0(format(round(abs_change_rate*100,2), nsmall=2), " (",
                                      format(round(abs_change_lci*100,2), nsmall=2), " - ",
                                      format(round(abs_change_uci*100,2), nsmall=2), ")"), 
         Relative_difference = paste0(format(round(relative_diff_perc,2), nsmall=2), "% (",
                                      format(round(relative_difference_lci, 2), nsmall=2)," - ",
                                      format(round(relative_difference_uci, 2), nsmall=2),")"))
diff_tab <- diff_tab %>% select(birth_category, preterm_category   ,   onset_group,rate_per_100_livebirths_2005,
                                rate_per_100_livebirths_2019,Absolute_difference ,  Relative_difference )
write.csv(diff_tab, paste0(output_path, "ratechange2_onset_table_singletons.csv"))

diff_tab <- diff_data %>% select(birth_category, preterm_category   ,   onset_group,
                                 rate_per_100_2005, rate_per_100_lci_2005, rate_per_100_uci_2005, 
                                 rate_per_100_2019, rate_per_100_lci_2019, rate_per_100_uci_2019,
                                 abs_change_rate, abs_change_lci, abs_change_uci, 
                                 relative_diff_perc, relative_difference_lci, relative_difference_uci) %>%
  mutate(rate_per_100_livebirths_2005 =  paste0(format(round(rate_per_100_2005,1), nsmall=1)," (",
                                                format(round(rate_per_100_lci_2005,1), nsmall=1)," - ",
                                                format(round(rate_per_100_uci_2005,1), nsmall=1),")"), 
         rate_per_100_livebirths_2019 =  paste0(format(round(rate_per_100_2019,1), nsmall=1)," (",
                                                format(round(rate_per_100_lci_2019,1), nsmall=1)," - ",
                                                format(round(rate_per_100_uci_2019,1), nsmall=1),")"), 
         Absolute_difference = paste0(format(round(abs_change_rate*100,1), nsmall=1), " (",
                                      format(round(abs_change_lci*100,1), nsmall=1), " - ",
                                      format(round(abs_change_uci*100,1), nsmall=1), ")"), 
         Relative_difference = paste0(format(round(relative_diff_perc,1), nsmall=1), "% (",
                                      format(round(relative_difference_lci, 1), nsmall=1)," - ",
                                      format(round(relative_difference_uci, 1), nsmall=1),")"))
diff_tab <- diff_tab %>% select(birth_category, preterm_category   ,   onset_group,rate_per_100_livebirths_2005,
                                rate_per_100_livebirths_2019,Absolute_difference ,  Relative_difference )
write.csv(diff_tab, paste0(output_path, "ratechange2_onset_table_singletons_1dp.csv"))


rate_change2 <- diff_tab

###Singleton main rates table####
##import the results of logistic regressions
rate1<- read.csv(paste0(output_path, "ratechange1_table_1dp.csv"))
rate2<- read.csv(paste0(output_path, "ratechange2_onset_table_singletons_1dp.csv"))

rate1_single <- rate1 %>% filter(type=="singleton") %>%
  select(type, birth_category, rate_per_100_livebirths_2005, rate_per_100_livebirths_2019, Absolute_difference, Relative_difference)
rate1_single$preterm_category = c("None", "All", "None")
rate1_single$onset_group = c("All", "All", "All")

rate2_single <- rate2 %>% mutate(type="singleton") %>%
  select(type, birth_category,preterm_category , onset_group, rate_per_100_livebirths_2005, rate_per_100_livebirths_2019, Absolute_difference, Relative_difference)

rates <- rbind(rate1_single, rate2_single)

post_all_single <- readRDS("tables/regression_tables/yrvalues_postterm_trend_single.rds") %>%
  mutate(birth_category = "Post-term", preterm_category = "None", onset_group = "All")
term_all_single <- readRDS(  "tables/regression_tables/yrvalues_term_trend_single.rds")%>%
  mutate(birth_category = "Term", preterm_category = "None", onset_group = "All")
pt_all_single <- readRDS("tables/regression_tables/yrvalues_preterm_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "All", onset_group = "All")
pt_all_single_PI <- readRDS("tables/regression_tables/yrvalues_allPT_PI_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "All", onset_group = "Provider initiated")
pt_all_single_S <- readRDS("tables/regression_tables/yrvalues_allPT_Sp_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "All", onset_group = "Spontaneous")

mod_group_results <- rbind(post_all_single, term_all_single, pt_all_single, pt_all_single_PI, pt_all_single_S)

pt_ml_single_S <- readRDS("tables/regression_tables/yrvalues_modlatePT_SP_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Moderate-Late Preterm", onset_group = "Spontaneous")
pt_ml_single_PI <- readRDS("tables/regression_tables/yrvalues_modlatePT_PI_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Moderate-Late Preterm", onset_group = "Provider initiated")
pt_ml_single_all <- readRDS("tables/regression_tables/yrvalues_modlatePT_all_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Moderate-Late Preterm", onset_group = "All onset")

pt_v_single_S <- readRDS("tables/regression_tables/yrvalues_vPT_Sp_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Very Preterm", onset_group = "Spontaneous")
pt_v_single_PI <- readRDS("tables/regression_tables/yrvalues_vPT_PI_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Very Preterm", onset_group = "Provider initiated")
pt_v_single_all <- readRDS("tables/regression_tables/yrvalues_vPT_all_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Very Preterm", onset_group = "All onset")

pt_ex_single_S <- readRDS("tables/regression_tables/yrvalues_exPT_Sp_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Extremely Preterm", onset_group = "Spontaneous")
pt_ex_single_PI <- readRDS("tables/regression_tables/yrvalues_exPT_PI_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Extremely Preterm", onset_group = "Provider initiated")
pt_ex_single_all <- readRDS("tables/regression_tables/yrvalues_exPT_all_trend_single.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Extremely Preterm", onset_group = "All onset")

mod_sub_group_results <- rbind(pt_ml_single_S,pt_ml_single_PI, pt_ml_single_all,
                               pt_v_single_S, pt_v_single_PI,pt_v_single_all,
                               pt_ex_single_S, pt_ex_single_PI, pt_ex_single_all)

all_model_values <- rbind(mod_group_results, mod_sub_group_results)

all_model_values <- all_model_values %>% 
  mutate(unadj_or  = paste0(format(round(coeff_yr,2),nsmall = 2), " (",
                            format(round(`2.5 %`,2),nsmall = 2), " - ",
                            format(round(`97.5 %`, 2),nsmall = 2), ")")) %>% 
  select(-c(coeff_yr, `2.5 %`,`97.5 %`))

rate_table1 <- left_join(rates, all_model_values) %>% 
  select(type, birth_category, preterm_category, onset_group, everything()) 

rate_table1$birth_category <- factor(rate_table1$birth_category, levels = c("Preterm", "Term", "Post-term"))
rate_table1$onset_group <- factor(rate_table1$onset_group, levels = c("All", "All onset", "Spontaneous", "Provider initiated"))
rate_table1$preterm_category <- factor(rate_table1$preterm_category, 
                                       levels = c("All", "Moderate-Late Preterm", "Very Preterm", "Extremely Preterm", "None"))

rate_table1 <-rate_table1 %>% 
  arrange(birth_category, preterm_category, onset_group)

write.csv(rate_table1, paste0(output_path,"main_table_1_paper_1dp.csv"))


#multiples main table (supp table 10) numbers####
###multiples confidence intercvals and main table
#tabel for multples to go in supplemnetary
##multiples table of preterm onsets with CIs

multi_ptb_subgrp_onset <- multi_gest_onset %>%  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
  filter(onset_group != "Unknown") %>%
  group_by(year) %>% 
  mutate(total_births_yr = sum(N)) %>%
  ungroup() %>%
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100) %>%
  group_by(year) %>%
  mutate( 
    rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
    rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)# %>%
# filter(birth_category=="Preterm") 

multi_ptb_onset <- multi_ptb_subgrp_onset %>% mutate(preterm_category="All") %>%
  filter(onset_group != "Unknown") %>% 
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100) %>%
  group_by(year) %>%
  mutate(#
    rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
    rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)

multi_ptb_subgroup <- multi_gest_onset %>%  filter(birth_category != "Unknown") %>% 
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown"))%>% 
  mutate( onset_group ="All onset") %>%
  group_by(year) %>% 
  mutate(total_births_yr = sum(N)) %>%
  ungroup() %>%
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100) %>%
  group_by(year) %>%
  mutate( 
    rate_per_100_lci =MultinomCI( c(n_births),  conf.level=0.95 )[,"lwr.ci"]*100,
    rate_per_100_uci = MultinomCI( c(n_births),  conf.level=0.95 )[,"upr.ci"]*100)# %>%


diff_data <- rbind(multi_ptb_subgrp_onset %>% filter(birth_category== "Preterm" & (year==2005 | year==2019)),  
                   multi_ptb_onset %>% filter(birth_category== "Preterm" & (year==2005 | year==2019)),
                   multi_ptb_subgroup %>% filter(birth_category== "Preterm" & (year==2005 | year==2019))) %>%
  mutate(n_not_bc  = total_births_yr -n_births)


diffs <-  diff_data %>% ungroup() %>% arrange(desc(year)) %>%
  filter(birth_category != "Unknown") %>% 
  group_by(preterm_category ,onset_group) %>% 
  #  rowwise() %>%
  arrange(desc(year)) %>%
  mutate(tst = list(broom::tidy(prop.test(n_births, total_births_yr, conf.level=0.95)))) %>%
  tidyr::unnest(tst) %>% rename(abs_change_lci = conf.low, abs_change_uci = conf.high) %>%
  ungroup() %>% mutate(abs_change_rate = estimate1-estimate2) %>%
  select(preterm_category ,onset_group, abs_change_rate,abs_change_lci, abs_change_uci) %>%
  unique()



diff_data <- diff_data %>% ungroup() %>%
  pivot_wider(id_cols= c(birth_category, preterm_category,   onset_group),
              names_from = year, values_from =c(n_births, n_not_bc, total_births_yr, rate_per_100,  rate_per_100_lci,  rate_per_100_uci ))%>%
  left_join(diffs) 


diff_data <- diff_data %>%  mutate(z_crit = 1.96, 
                                   p1 = rate_per_100_2005/100, p2 = rate_per_100_2019/100,
                                   cv1 = sqrt(p1*(1-p1)),
                                   cv2 = sqrt(p2*(1-p2)),
                                   relative_diff_perc = (p2-p1)/p1 *100) %>% 
  as.data.frame() %>% 
  mutate(relative_difference_lci  = reldiffci(x=relative_diff_perc, z= z_crit, cv1=cv1, cv2=cv2)$lci, 
         relative_difference_uci  = reldiffci(x=relative_diff_perc, z= z_crit, cv1=cv1, cv2=cv2)$uci ) %>%
  select(-c(z_crit, p1, p2, cv1, cv2))



diff_tab <- diff_data %>% select(birth_category, preterm_category   ,   onset_group,
                                 rate_per_100_2005, rate_per_100_lci_2005, rate_per_100_uci_2005, 
                                 rate_per_100_2019, rate_per_100_lci_2019, rate_per_100_uci_2019,
                                 abs_change_rate, abs_change_lci, abs_change_uci, 
                                 relative_diff_perc, relative_difference_lci, relative_difference_uci) %>%
  mutate(rate_per_100_livebirths_2005 =  paste0(format(round(rate_per_100_2005,1),nsmall = 1),
                                                " (", format(round(rate_per_100_lci_2005,1),nsmall = 1)," - ", 
                                                format(round(rate_per_100_uci_2005,1),nsmall = 1),")"), 
         rate_per_100_livebirths_2019 =  paste0(format(round(rate_per_100_2019,1),nsmall=1)," (", format(round(rate_per_100_lci_2019,1),nsmall=1)," - ", format(round(rate_per_100_uci_2019,1),nsmall=1),")"), 
         Absolute_difference = paste0(format(round(abs_change_rate*100,1),nsmall=1), " (", format(round(abs_change_lci*100,1),nsmall=1), " - ", format(round(abs_change_uci*100,1),nsmall=1), ")"), 
         Relative_difference = paste0(format(round(relative_diff_perc,1),nsmall=1), "% (", format(round(relative_difference_lci, 1),nsmall=1)," - ", format(round(relative_difference_uci, 1),nsmall=1),")"))
diff_tab <- diff_tab %>% select(birth_category, preterm_category   ,   onset_group,rate_per_100_livebirths_2005,
                                rate_per_100_livebirths_2019,Absolute_difference ,  Relative_difference )

write_csv(diff_tab, paste0(output_path, "ratechange3_onset_table.csv"))



###load model results for multiples
rate1<- read.csv(paste0(output_path, "ratechange1_table_1dp.csv"))

rate1_multi <- rate1 %>% filter(type=="multiple" & birth_category !="Unknown") %>% 
  select(type, birth_category, rate_per_100_livebirths_2005, rate_per_100_livebirths_2019, Absolute_difference, Relative_difference)
rate1_multi$preterm_category = c("None", "All", "None")
rate1_multi$onset_group = c("All", "All", "All")

rate3 <- read.csv(paste0(output_path, "ratechange3_onset_table.csv"))
rate3 <- rate3 %>% mutate(type="multiple") %>%
  select(type, birth_category,preterm_category , onset_group, rate_per_100_livebirths_2005, rate_per_100_livebirths_2019, Absolute_difference, Relative_difference)

rates <- rbind(rate1_multi , rate3)


term_all_multi <- readRDS(  "tables/regression_tables/yrvalues_term_trend_multi.rds")%>%
  mutate(birth_category = "Term", preterm_category = "None", onset_group = "All")
pt_all_multi <- readRDS("tables/regression_tables/yrvalues_preterm_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "All", onset_group = "All")
pt_all_multi_PI <- readRDS("tables/regression_tables/yrvalues_allPT_PI_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "All", onset_group = "Provider initiated")
pt_all_multi_S <- readRDS("tables/regression_tables/yrvalues_allPT_Sp_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "All", onset_group = "Spontaneous")

mod_group_results <- rbind(term_all_multi, pt_all_multi, pt_all_multi_PI, pt_all_multi_S)

pt_ml_multi_S <- readRDS("tables/regression_tables/yrvalues_modlatePT_SP_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Moderate-Late Preterm", onset_group = "Spontaneous")
pt_ml_multi_PI <- readRDS("tables/regression_tables/yrvalues_modlatePT_PI_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Moderate-Late Preterm", onset_group = "Provider initiated")
pt_ml_multi_all <- readRDS("tables/regression_tables/yrvalues_modlatePT_all_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Moderate-Late Preterm", onset_group = "All onset")

pt_v_multi_S <- readRDS("tables/regression_tables/yrvalues_vPT_Sp_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Very Preterm", onset_group = "Spontaneous")
pt_v_multi_PI <- readRDS("tables/regression_tables/yrvalues_vPT_PI_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Very Preterm", onset_group = "Provider initiated")
pt_v_multi_all <- readRDS("tables/regression_tables/yrvalues_vPT_all_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Very Preterm", onset_group = "All onset")

pt_ex_multi_S <- readRDS("tables/regression_tables/yrvalues_exPT_Sp_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Extremely Preterm", onset_group = "Spontaneous")
pt_ex_multi_PI <- readRDS("tables/regression_tables/yrvalues_exPT_PI_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Extremely Preterm", onset_group = "Provider initiated")
pt_ex_multi_all <- readRDS("tables/regression_tables/yrvalues_exPT_all_trend_multi.rds") %>%
  mutate(birth_category = "Preterm", preterm_category = "Extremely Preterm", onset_group = "All onset")

mod_sub_group_results <- rbind(pt_ml_multi_S,pt_ml_multi_PI, pt_ml_multi_all,
                               pt_v_multi_S, pt_v_multi_PI,pt_v_multi_all,
                               pt_ex_multi_S, pt_ex_multi_PI, pt_ex_multi_all)

all_model_values <- rbind(mod_group_results, mod_sub_group_results)

all_model_values <- all_model_values %>% 
  mutate(unadj_or  = paste0(format(round(coeff_yr,2),nsmall = 2), " (",
                            format(round(`2.5 %`,2),nsmall = 2), " - ",
                            format(round(`97.5 %`, 2),nsmall = 2), ")")) %>% 
  select(-c(coeff_yr, `2.5 %`,`97.5 %`))

rate_table1 <- left_join(rates, all_model_values) %>% 
  select(birth_category, preterm_category, onset_group, everything()) 

rate_table1$birth_category <- factor(rate_table1$birth_category, levels = c("Preterm", "Term", "Post-term"))
rate_table1$onset_group <- factor(rate_table1$onset_group, levels = c("All", "All onset", "Spontaneous", "Provider initiated"))
rate_table1$preterm_category <- factor(rate_table1$preterm_category, 
                                       levels = c("All", "Moderate-Late Preterm", "Very Preterm", "Extremely Preterm", "None"))

rate_table1 <-rate_table1 %>% 
  arrange(birth_category, preterm_category, onset_group)

write.csv(rate_table1, paste0(output_path,"SUPP_table_10.csv"))


#weekly GA histograms  - Supplementary tables 2a 2b ####
##singletons GA####
single_gest_weeks_selected  <- single_gest_weeks %>% 
  filter(birth_category!="Unknown"& (year==2005 | year==2010 | year==2015|year==2019)) %>% 
  group_by(year) %>%
  mutate(total_births_yr_excl_unkn = sum(N)) %>%
  ungroup() %>%
  mutate(prop_births = N/total_births_yr_excl_unkn*100)

single_gest_weeks_selected_unkn  <- single_gest_weeks %>% 
  filter(birth_category=="Unknown"& (year==2005 | year==2010 | year==2015|year==2019))%>%
  mutate(value_type = "Number") %>%
  select(year, single_or_multi, N) %>% rename(unknown = N)


n_wide <- single_gest_weeks_selected %>% select(-c(preterm_category, birth_category))%>% 
  select(-prop_births, -total_births_yr_excl_unkn) %>%
  pivot_wider(names_from = gestation_at_delivery_completed_weeks, values_from = c(N)  ) %>%
  mutate(value_type= "Number")

p_wide <- single_gest_weeks_selected %>% select(-c(preterm_category, birth_category))%>% 
  select(-N, -total_births_yr_excl_unkn) %>%
  pivot_wider(names_from = gestation_at_delivery_completed_weeks, values_from = c(prop_births)  )%>%
  mutate(value_type= "rate")

supp_tab2a <- rbind(n_wide, p_wide)
supp_tab2a <- left_join(supp_tab2a, single_gest_weeks_selected_unkn) 
supp_tab2a <- supp_tab2a %>% arrange(year) %>% select(single_or_multi, year, value_type, everything())
write.csv(supp_tab2a, paste0(output_path, "Supp2a.csv"))

##multi GA####
multi_gest_weeks_selected  <- multi_gest_weeks %>% 
  filter(birth_category!="Unknown"& (year==2005 | year==2010 | year==2015|year==2019)) %>% 
  group_by(year) %>%
  mutate(total_births_yr_excl_unkn = sum(N)) %>%
  ungroup() %>%
  mutate(prop_births = N/total_births_yr_excl_unkn*100)

multi_gest_weeks_selected_unkn  <- multi_gest_weeks %>% 
  filter(birth_category=="Unknown"& (year==2005 | year==2010| year==2015|year==2019))%>%
  mutate(value_type = "Number") %>%
  select(year, single_or_multi, N) %>% rename(unknown = N)


n_wide <- multi_gest_weeks_selected %>% select(-c(preterm_category, birth_category))%>% 
  select(-prop_births, -total_births_yr_excl_unkn) %>%
  pivot_wider(names_from = gestation_at_delivery_completed_weeks, values_from = c(N)  ) %>%
  mutate(value_type= "Number")

p_wide <- multi_gest_weeks_selected %>% select(-c(preterm_category, birth_category))%>% 
  select(-N, -total_births_yr_excl_unkn) %>%
  pivot_wider(names_from = gestation_at_delivery_completed_weeks, values_from = c(prop_births)  )%>%
  mutate(value_type= "rate")

supp_tab2b <- rbind(n_wide, p_wide)
supp_tab2b <- left_join(supp_tab2b, multi_gest_weeks_selected_unkn) 
supp_tab2b <- supp_tab2b %>% arrange(year) %>% select(single_or_multi, year, value_type, everything())
write.csv(supp_tab2b, paste0(output_path, "Supp2b.csv"))

#Supplementary tables 3a, 3b####
table3a <- readRDS("tables/table1_single.rds")
##recalculate rate excluding unknown onset for this table
table3a <- table3a %>% filter(birth_category =="Preterm") %>% 
  select(labels, preterm_category, onset_group,  value_type, totals, everything())# %>%
#  select(-c(birth_category))

total_births_yr_ex_onset <- single_gest_groups %>% select(year, total_births_yr_excl_unkn_onset) %>% 
  unique() %>%
  filter(!is.na(total_births_yr_excl_unkn_onset)) %>%
  pivot_wider(names_from  = year, values_from = total_births_yr_excl_unkn_onset) %>% 
  mutate(value_type = "Number",category_of_singleton_births = "Total Births excluding unknown gestation and unknown onset" ) %>%
  mutate(totals = rowSums(select(., `2005`:`2019`))) %>%
  mutate(difference =`2019`-`2005`) %>%
  mutate(birth_category="All", preterm_category="None", onset_group="All")


table3a <- table3a %>% mutate(relative_difference  = difference/`2005`)
table3a <- table3a %>% filter(birth_category =="Preterm") %>% 
  select(labels, preterm_category, onset_group,  value_type, totals, everything()) %>%
  select(-c(birth_category))
write.csv(table3a, paste0(output_path, "supp3a.csv"))

table3b <- readRDS("tables/table1_multiples.rds")
table3b <- table3b %>% mutate(relative_difference  = difference/`2005`)
table3b <- table3b %>% filter(birth_category =="Preterm") %>% 
  select(labels, preterm_category, onset_group,  value_type, totals, everything()) %>%
  select(-c(birth_category))
write.csv(table3b, paste0(output_path, "supp3b.csv"))

#demographics####
###age Supp table 4####
####Singletons####
single_gest_age <- readRDS(paste0(data_path, "2_working_data/gest_by_age_single.rds"))
df <- single_gest_age %>% filter(year !=2020)
#SIMD - supp tables 7 and 8####
df_unkn_age <- single_gest_age %>% 
  filter(birth_category!="Unknown" & maternal_age_at_delivery_years=="Unknown") 

df_unkn_age_yr <- df_unkn_age %>% group_by(year) %>% summarise(n_births = sum(N))

df <- single_gest_age  %>% 
  filter(birth_category!="Unknown" & maternal_age_at_delivery_years!="Unknown") %>%
  filter(year !=2020)
##change the code for age by replacing with simd variables/values
##part 1 of table - % in SIMD group overall
df_overall <- df %>% group_by(year,maternal_age_at_delivery_years ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(total_births_year = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year)


df_overall_totals <-  df %>% group_by(maternal_age_at_delivery_years ) %>% 
  filter(birth_category!="Unknown" ) %>%
  summarise(n_births= sum(N)) %>% ungroup() %>% mutate(prop_births = n_births/sum(n_births))

df_overall_totals <- df_overall_totals  %>% pivot_longer(cols = c(n_births, prop_births)) %>%
  rename(totals  = value, value = name) %>%
  mutate(value = case_when(value=="n_births" ~ "number", value=="prop_births"~ "% all singleton births"))


df_n <- df_overall %>% select(-percent_births, -total_births_year) %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number")
df_p <- df_overall %>% select(-n_births, -total_births_year) %>%
  pivot_wider(names_from = year, values_from = percent_births) %>% 
  mutate(value = "% all singleton births")

age_tab1 <- rbind(df_n, df_p)
age_tab1 <-left_join(age_tab1, df_overall_totals)
age_tab1  <- age_tab1  %>% mutate(birth_category = "All singleton births") %>%
  select(birth_category,maternal_age_at_delivery_years, value, everything()) %>%
  arrange(maternal_age_at_delivery_years)

age_tab1 <- age_tab1 %>% mutate(abs_difference = `2019`-`2005`) %>%
  mutate(prop_difference = (`2019`-`2005`)/`2005` ) 

##part 2 of table: split by term and rate is rate within age category. 

df_term <-  df %>% group_by(year,maternal_age_at_delivery_years, birth_category ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year, maternal_age_at_delivery_years) %>% 
  mutate(total_births_year_age = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year_age)


tot_births_age_cat  <- df %>% group_by(maternal_age_at_delivery_years) %>%
  summarise(n_births_age_cat = sum(N)) 
df_term_totals <- df %>% group_by(maternal_age_at_delivery_years , birth_category) %>% 
  filter(birth_category!="Unknown" ) %>%
  summarise(n_births= sum(N)) %>% ungroup() %>% 
  left_join(tot_births_age_cat) %>%
  mutate(prop_births = n_births/n_births_age_cat)


df_term_totals <-df_term_totals %>% pivot_longer(cols = c(n_births, prop_births)) %>%
  rename(totals  = value, value = name) %>%
  mutate(value = case_when(value=="n_births" ~ "number", value=="prop_births"~ "% all singleton births"))


df_n <- df_term %>% select(-percent_births, -total_births_year_age) %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number")
df_p <- df_term %>% select(-n_births, -total_births_year_age) %>%
  pivot_wider(names_from = year, values_from = percent_births) %>% mutate(value = "% all singleton births")
df_n_unknown <- df_unkn_simd_yr %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number") %>%
  mutate(maternal_age_at_delivery_years= "Unknown", birth_category="All") 

tab2 <- rbind(df_n, df_p, df_n_unknown)

tab2 <- left_join(tab2, df_term_totals)


tab2 <- tab2 %>% select(birth_category,maternal_age_at_delivery_years, value, everything()) %>%
  arrange(factor(birth_category, levels = c("Preterm", "Term", "Post-term", "Unknown")),
          maternal_age_at_delivery_years)

tab2 <- tab2 %>% mutate(abs_difference = `2019`-`2005`) %>%
  mutate(prop_difference = (`2019`-`2005`)/`2005` ) 
#tab2 <- tab2 %>%
#  filter(maternal_simd_quintile_from_postcode_of_residence  !="Unknown", birth_category !="Unknown")

write_csv(age_tab1, paste0(output_path,"supp4a.csv"))
write_csv(tab2,paste0(output_path,"supp5a.csv"))
rm(tab2, age_tab1)


####multiples####

df <- multi_gest_age %>% filter(year !=2020)

#SIMD - supp tables 7 and 8####
df_unkn_age <- multi_gest_age %>% 
  filter(birth_category!="Unknown" & maternal_age_at_delivery_years=="Unknown") 

df_unkn_age_yr <- df_unkn_age %>% group_by(year) %>% summarise(n_births = sum(N))

df <- multi_gest_age  %>% 
  filter(birth_category!="Unknown" & maternal_age_at_delivery_years!="Unknown") %>%
  filter(year !=2020)
##change the code for age by replacing with simd variables/values
##part 1 of table - % in SIMD group overall
df_overall <- df %>% group_by(year,maternal_age_at_delivery_years ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(total_births_year = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year)


df_overall_totals <-  df %>% group_by(maternal_age_at_delivery_years ) %>% 
  filter(birth_category!="Unknown" ) %>%
  summarise(n_births= sum(N)) %>% ungroup() %>% mutate(prop_births = n_births/sum(n_births))

df_overall_totals <- df_overall_totals  %>% pivot_longer(cols = c(n_births, prop_births)) %>%
  rename(totals  = value, value = name) %>%
  mutate(value = case_when(value=="n_births" ~ "number", value=="prop_births"~ "% all multiple births"))


df_n <- df_overall %>% select(-percent_births, -total_births_year) %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number")
df_p <- df_overall %>% select(-n_births, -total_births_year) %>%
  pivot_wider(names_from = year, values_from = percent_births) %>% 
  mutate(value = "% all multiple births")

age_tab1 <- rbind(df_n, df_p)
age_tab1 <-left_join(age_tab1, df_overall_totals)
age_tab1  <- age_tab1  %>% mutate(birth_category = "All multiple births") %>%
  select(birth_category,maternal_age_at_delivery_years, value, everything()) %>%
  arrange(maternal_age_at_delivery_years)

age_tab1 <- age_tab1 %>% mutate(abs_difference = `2019`-`2005`) %>%
  mutate(prop_difference = (`2019`-`2005`)/`2005` ) 

##part 2 of table: split by term and rate is rate within age category. 

df_term <-  df %>% group_by(year,maternal_age_at_delivery_years, birth_category ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year, maternal_age_at_delivery_years) %>% 
  mutate(total_births_year_age = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year_age)


tot_births_age_cat  <- df %>% group_by(maternal_age_at_delivery_years) %>%
  summarise(n_births_age_cat = sum(N)) 
df_term_totals <- df %>% group_by(maternal_age_at_delivery_years , birth_category) %>% 
  filter(birth_category!="Unknown" ) %>%
  summarise(n_births= sum(N)) %>% ungroup() %>% 
  left_join(tot_births_age_cat) %>%
  mutate(prop_births = n_births/n_births_age_cat)


df_term_totals <-df_term_totals %>% pivot_longer(cols = c(n_births, prop_births)) %>%
  rename(totals  = value, value = name) %>%
  mutate(value = case_when(value=="n_births" ~ "number", value=="prop_births"~ "% all multiple births"))


df_n <- df_term %>% select(-percent_births, -total_births_year_age) %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number")
df_p <- df_term %>% select(-n_births, -total_births_year_age) %>%
  pivot_wider(names_from = year, values_from = percent_births) %>% mutate(value = "% all multiple births")
df_n_unknown <- df_unkn_simd_yr %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number") %>%
  mutate(maternal_age_at_delivery_years= "Unknown", birth_category="All") 

tab2 <- rbind(df_n, df_p, df_n_unknown)

tab2 <- left_join(tab2, df_term_totals)


tab2 <- tab2 %>% select(birth_category,maternal_age_at_delivery_years, value, everything()) %>%
  arrange(factor(birth_category, levels = c("Preterm", "Term", "Post-term", "Unknown")),
          maternal_age_at_delivery_years)

tab2 <- tab2 %>% mutate(abs_difference = `2019`-`2005`) %>%
  mutate(prop_difference = (`2019`-`2005`)/`2005` ) 

write_csv(age_tab1, paste0(output_path,"supp4b.csv"))
write_csv(tab2,paste0(output_path,"supp5b.csv"))

##Supp table 6 age ratios####
##done in excel

#SIMD - supp tables 7 and 8####
df_unkn_simd <- single_gest_simd %>% 
  filter(birth_category!="Unknown" & maternal_simd_quintile_from_postcode_of_residence=="Unknown") 

df_unkn_simd_yr <- df_unkn_simd %>% group_by(year) %>% summarise(n_births = sum(N))

df <- single_gest_simd  %>% 
  filter(birth_category!="Unknown" & maternal_simd_quintile_from_postcode_of_residence!="Unknown") %>%
  filter(year !=2020)
##change the code for age by replacing with simd variables/values
##part 1 of table - % in SIMD group overall
df_overall <- df %>% group_by(year,maternal_simd_quintile_from_postcode_of_residence ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(total_births_year = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year)


df_overall_totals <-  df %>% group_by(maternal_simd_quintile_from_postcode_of_residence ) %>% 
  filter(birth_category!="Unknown" ) %>%
  summarise(n_births= sum(N)) %>% ungroup() %>% mutate(prop_births = n_births/sum(n_births))

df_overall_totals <- df_overall_totals  %>% pivot_longer(cols = c(n_births, prop_births)) %>%
  rename(totals  = value, value = name) %>%
  mutate(value = case_when(value=="n_births" ~ "number", value=="prop_births"~ "% all singleton births"))


df_n <- df_overall %>% select(-percent_births, -total_births_year) %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number")
df_p <- df_overall %>% select(-n_births, -total_births_year) %>%
  pivot_wider(names_from = year, values_from = percent_births) %>% 
  mutate(value = "% all singleton births")

simd_tab1 <- rbind(df_n, df_p)
simd_tab1 <-left_join(simd_tab1, df_overall_totals)
simd_tab1  <- simd_tab1  %>% mutate(birth_category = "All singleton births") %>%
  select(birth_category,maternal_simd_quintile_from_postcode_of_residence, value, everything()) %>%
  arrange(maternal_simd_quintile_from_postcode_of_residence)

simd_tab1 <- simd_tab1 %>% mutate(abs_difference = `2019`-`2005`) %>%
  mutate(prop_difference = (`2019`-`2005`)/`2005` ) 

##part 2 of table: split by term and rate is rate within age category. 

df_term <-  df %>% group_by(year,maternal_simd_quintile_from_postcode_of_residence, birth_category ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(total_births_year_age = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year_age)


tot_births_simd_cat  <- df %>% group_by(maternal_simd_quintile_from_postcode_of_residence) %>%
  summarise(n_births_simd_cat = sum(N)) 
df_term_totals <- df %>% group_by(maternal_simd_quintile_from_postcode_of_residence , birth_category) %>% 
  filter(birth_category!="Unknown" ) %>%
  summarise(n_births= sum(N)) %>% ungroup() %>% 
  left_join(tot_births_simd_cat) %>%
  mutate(prop_births = n_births/n_births_simd_cat)


df_term_totals <-df_term_totals %>% pivot_longer(cols = c(n_births, prop_births)) %>%
  rename(totals  = value, value = name) %>%
  mutate(value = case_when(value=="n_births" ~ "number", value=="prop_births"~ "% all singleton births"))


df_n <- df_term %>% select(-percent_births, -total_births_year_age) %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number")
df_p <- df_term %>% select(-n_births, -total_births_year_age) %>%
  pivot_wider(names_from = year, values_from = percent_births) %>% mutate(value = "% all singleton births")
df_n_unknown <- df_unkn_simd_yr %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number") %>%
  mutate(maternal_simd_quintile_from_postcode_of_residence= "Unknown", birth_category="All") 

tab2 <- rbind(df_n, df_p, df_n_unknown)

tab2 <- left_join(tab2, df_term_totals)


tab2 <- tab2 %>% select(birth_category,maternal_simd_quintile_from_postcode_of_residence, value, everything()) %>%
  arrange(factor(birth_category, levels = c("Preterm", "Term", "Post-term", "Unknown")),
          maternal_simd_quintile_from_postcode_of_residence)

tab2 <- tab2 %>% mutate(abs_difference = `2019`-`2005`) %>%
  mutate(prop_difference = (`2019`-`2005`)/`2005` ) 
#tab2 <- tab2 %>%
#  filter(maternal_simd_quintile_from_postcode_of_residence  !="Unknown", birth_category !="Unknown")

write_csv(simd_tab1, paste0(output_path,"supp7a.csv"))
write_csv(tab2,paste0(output_path,"supp8a.csv"))


###SIMD multiples####
#tables to correspond t plots, actual plotting moved to script 6
df <- multi_gest_simd  %>% 
  filter(birth_category!="Unknown" & maternal_simd_quintile_from_postcode_of_residence!="Unknown")%>%
  filter(year !=2020)
##
df_unkn_simd <- multi_gest_simd %>% 
  filter(birth_category!="Unknown" & maternal_simd_quintile_from_postcode_of_residence=="Unknown") 

df_unkn_simd_yr <- df_unkn_simd %>% group_by(year) %>% summarise(n_births = sum(N))

##change the code for age by replacing with simd variables/values
##part 1 of table - % in SIMD group overall
df_overall <- df %>% group_by(year,maternal_simd_quintile_from_postcode_of_residence ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(total_births_year = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year)

df_overall_totals <-  df %>% group_by(maternal_simd_quintile_from_postcode_of_residence ) %>% 
  filter(birth_category!="Unknown" ) %>%
  summarise(n_births= sum(N)) %>% ungroup() %>% mutate(prop_births = n_births/sum(n_births))

df_overall_totals <- df_overall_totals  %>% pivot_longer(cols = c(n_births, prop_births)) %>%
  rename(totals  = value, value = name) %>%
  mutate(value = case_when(value=="n_births" ~ "number", value=="prop_births"~ "% all multiple births"))

df_n <- df_overall %>% select(-percent_births, -total_births_year) %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number")
df_p <- df_overall %>% select(-n_births, -total_births_year) %>%
  pivot_wider(names_from = year, values_from = percent_births) %>% mutate(value = "% all multiple births")

simd_tab1 <- rbind(df_n, df_p)
simd_tab1 <-left_join(simd_tab1, df_overall_totals)
simd_tab1  <- simd_tab1  %>% mutate(birth_category = "All multiple births") %>%
  select(birth_category,maternal_simd_quintile_from_postcode_of_residence, value, everything()) %>%
  arrange(maternal_simd_quintile_from_postcode_of_residence)

simd_tab1 <- simd_tab1 %>% mutate(abs_difference = `2019`-`2005`) %>%
  mutate(prop_difference = (`2019`-`2005`)/`2005` ) 

##part 2 of table: split by term and rate is rate within age category. 


df_term <-  df %>% group_by(year,maternal_simd_quintile_from_postcode_of_residence, birth_category ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(total_births_year_age = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year_age)


tot_births_simd_cat  <- df %>% group_by(maternal_simd_quintile_from_postcode_of_residence) %>%
  summarise(n_births_simd_cat = sum(N)) 
df_term_totals <- df %>% group_by(maternal_simd_quintile_from_postcode_of_residence , birth_category) %>% 
  filter(birth_category!="Unknown" ) %>%
  summarise(n_births= sum(N)) %>% ungroup() %>% 
  left_join(tot_births_simd_cat) %>%
  mutate(prop_births = n_births/n_births_simd_cat)


df_term_totals <-df_term_totals %>% pivot_longer(cols = c(n_births, prop_births)) %>%
  rename(totals  = value, value = name) %>%
  mutate(value = case_when(value=="n_births" ~ "number", value=="prop_births"~ "% all multiple births"))


df_n <- df_term %>% select(-percent_births, -total_births_year_age) %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number")
df_p <- df_term %>% select(-n_births, -total_births_year_age) %>%
  pivot_wider(names_from = year, values_from = percent_births) %>% mutate(value = "% all multiple births")
df_n_unknown <- df_unkn_simd_yr %>%
  pivot_wider(names_from = year, values_from = n_births) %>% mutate(value = "number") %>%
  mutate(maternal_simd_quintile_from_postcode_of_residence= "Unknown", birth_category="All") 

tab2 <- rbind(df_n, df_p, df_n_unknown)

tab2 <- left_join(tab2, df_term_totals)

tab2 <- tab2 %>% select(birth_category,maternal_simd_quintile_from_postcode_of_residence, value, everything()) %>%
  arrange(factor(birth_category, levels = c("Preterm", "Term", "Post-term", "Unknown")),
          maternal_simd_quintile_from_postcode_of_residence)

tab2 <- tab2 %>% mutate(abs_difference = `2019`-`2005`) %>%
  mutate(prop_difference = (`2019`-`2005`)/`2005` ) 
#tab2 <- tab2 %>%
#  filter(maternal_simd_quintile_from_postcode_of_residence  !="Unknown", birth_category !="Unknown")

write_csv(simd_tab1, paste0(output_path,"supp7b.csv"))
write_csv(tab2,paste0(output_path,"supp8b.csv"))



