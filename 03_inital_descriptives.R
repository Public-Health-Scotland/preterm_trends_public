## libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(janitor)

#intial descriptives ####
# checks on: missing/unknowns, % at 22-23 weeks.
# Descriptives graphs/tables of : % preterm, term, post term over time
#save Items for markdown which will include table of missings
source(paste0(here::here(), "/00_setup.R"))

#load singletons & multiples all datasets ####
multi_gest_weeks <- readRDS(paste0(data_path, "2_working_data/gest_wks_multi.rds"))

single_gest_weeks <- readRDS(paste0(data_path, "2_working_data/gest_wks_single.rds"))
single_gest_by_age <- readRDS(paste0(data_path, "2_working_data/gest_by_age_single.rds"))

gest_by_ethn_single <- readRDS(paste0(data_path,"2_working_data/gest_by_ethn_single.rds"))
gest_by_onset_single <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_single.rds"))
gest_by_ethn_multi <- readRDS(paste0(data_path,"2_working_data/gest_by_ethn_multi.rds"))
gest_by_onset_multi <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_multi.rds"))
gest_by_simd_multi <- readRDS(paste0(data_path,"2_working_data/gest_by_simd_multi.rds"))
gest_by_simd_single <- readRDS(paste0(data_path,"2_working_data/gest_by_simd_single.rds"))
gest_by_age_multi <- readRDS(paste0(data_path,"2_working_data/gest_by_age_multi.rds"))
gest_by_age_single <- readRDS(paste0(data_path,"2_working_data/gest_by_age_single.rds"))
#combine singles and multiples
gest_weeks <- rbind(single_gest_weeks, multi_gest_weeks)
gest_by_onset <- rbind(gest_by_onset_single, gest_by_onset_multi)
gest_by_age <- rbind(gest_by_age_multi, gest_by_age_single)
gest_by_simd <- rbind(gest_by_simd_multi, gest_by_simd_single)
gest_by_ethn <- rbind(gest_by_ethn_multi, gest_by_ethn_single)

#Check proportion 22-23 weeks recorded####
single_prop_2223 <- single_gest_weeks %>%
  filter(!gestation_at_delivery_completed_weeks=="Unknown") %>% ##only want to know proportion out of gest recorded
  mutate(wk22_23 = ifelse(gestation_at_delivery_completed_weeks =="22" |
                            gestation_at_delivery_completed_weeks =="23", 1, 0)) %>% 
  group_by(year) %>% summarise(total_with_gest = sum(N), total_22_23wks = sum(N[wk22_23==1])) %>%
  mutate(percent_22_23 = total_22_23wks/total_with_gest*100)

single_prop_2223 <- single_prop_2223 %>%
  pivot_longer(cols = c(total_with_gest, total_22_23wks, percent_22_23), names_to = "names", values_to = "n" ) %>%
  mutate(n = format(n, scientific = F)) %>%
  pivot_wider(names_from = year, values_from = n) %>% mutate(type= "singleton")

multi_prop_2223 <- multi_gest_weeks %>%
  filter(!gestation_at_delivery_completed_weeks=="Unknown") %>% ##only want to know proportion out of gest recorded
  mutate(wk22_23 = ifelse(gestation_at_delivery_completed_weeks =="22" |
                            gestation_at_delivery_completed_weeks =="23", 1, 0)) %>% 
  group_by(year) %>% summarise(total_with_gest = sum(N), total_22_23wks = sum(N[wk22_23==1])) %>%
  mutate(percent_22_23 = total_22_23wks/total_with_gest*100)

multi_prop_2223 <- multi_prop_2223 %>%
  pivot_longer(cols = c(total_with_gest, total_22_23wks, percent_22_23), names_to = "names", values_to = "n" ) %>%
  pivot_wider(names_from = year, values_from = n) %>% mutate(type= "multiple")
  
df <- rbind(single_prop_2223, multi_prop_2223)
df <- df %>% select(type, names, everything())
knitr::kable(df)
saveRDS(df, "tables/wk22_23_by_year.rds")

#check missing####
annual_miss_gest <- gest_weeks %>% group_by(single_or_multi, year) %>% 
  summarise(total_births = sum(N), total_missing_gest =  sum(N[gestation_at_delivery_completed_weeks=="Unknown"])) %>%
  mutate(percent_missing = total_missing_gest/total_births *100)
saveRDS(annual_miss_gest,"numbers_missing/total_missing_annual.rds" )  
total_miss_gest <- gest_weeks %>% mutate(year="all") %>% group_by(single_or_multi) %>% 
  summarise(total_births = sum(N), total_missing_gest =  sum(N[gestation_at_delivery_completed_weeks=="Unknown"])) %>%
  mutate(percent_missing = total_missing_gest/total_births *100)
saveRDS(total_miss_gest,"numbers_missing/total_missing.rds" )  
total_miss_age <- gest_by_age %>% group_by(single_or_multi) %>% 
  summarise(total_births = sum(N), total_missing_gest =  sum(N[gestation_at_delivery_completed_weeks=="Unknown"]), 
            total_missing_age = sum(N[maternal_age_at_delivery_years=="Unknown"]), 
            total_missing_age_or_gest = sum(N[maternal_age_at_delivery_years=="Unknown"|gestation_at_delivery_completed_weeks=="Unknown" ])) 
saveRDS(total_miss_age,"numbers_missing/total_missing_age.rds" )    

tot_miss_ethn <- gest_by_ethn %>% 
  filter(year_group =="2015_2020") %>% group_by(single_or_multi) %>% 
  summarise(total_births = sum(N), total_missing_gest =  sum(N[gestation_at_delivery_completed_weeks=="Unknown"]), 
            total_missing_ethnicity = sum(N[maternal_ethnicity_from_smr02_delivery_record=="Unknown"]), 
            total_missing_ethnicity_or_gest = sum(N[maternal_ethnicity_from_smr02_delivery_record=="Unknown"|gestation_at_delivery_completed_weeks=="Unknown" ])) 

saveRDS(tot_miss_ethn,"numbers_missing/total_missing_ethnicity.rds" )  

tot_miss_onset <- gest_by_onset %>% group_by(single_or_multi) %>% 
  filter(onset_of_delivery != "Emergency" &onset_of_delivery != "Elective" ) %>% #remove duplicates for CS categories
  summarise(total_births = sum(N), total_missing_gest =  sum(N[gestation_at_delivery_completed_weeks=="Unknown"]), 
            total_missing_onset = sum(N[onset_of_delivery=="Unknown"]), 
            total_missing_onset_or_gest = sum(N[onset_of_delivery=="Unknown"|gestation_at_delivery_completed_weeks=="Unknown" ])) 

saveRDS(tot_miss_onset,"numbers_missing/total_missing_onset.rds" )  

tot_miss_simd <- gest_by_simd %>% group_by(single_or_multi) %>% 
  summarise(total_births = sum(N), total_missing_gest =  sum(N[gestation_at_delivery_completed_weeks=="Unknown"]), 
            total_missing_simd = sum(N[maternal_simd_quintile_from_postcode_of_residence=="Unknown"]), 
            total_missing_simd_or_gest = sum(N[maternal_simd_quintile_from_postcode_of_residence=="Unknown"|gestation_at_delivery_completed_weeks=="Unknown" ])) 
saveRDS(tot_miss_simd,"numbers_missing/total_missing_simd.rds" ) 

##median and IQR####
#grouped data so median doesnt just work....
 gest_weeks_filter <- single_gest_weeks %>% filter(gestation_at_delivery_completed_weeks
                                                    !="Unknown")
 multi_gest_weeks_filter <- multi_gest_weeks %>% filter(gestation_at_delivery_completed_weeks
                                       !="Unknown")
 
#median and iqr singles
  vect<- 0
  for(i in 1:nrow(gest_weeks_filter) ){
    out <- rep(as.numeric(gest_weeks_filter$gestation_at_delivery_completed_weeks[i]),
               gest_weeks_filter$N[i] )
    vect <- c(vect,out)
  }
  vect <- vect[2:length(vect)] # dump the 0 used to intiate vector
#med_single <-   median(vect)
iqr_singleton <- quantile(vect)

vect<- 0
for(i in 1:nrow(multi_gest_weeks_filter) ){
  out <- rep(as.numeric(multi_gest_weeks_filter$gestation_at_delivery_completed_weeks[i]),
             multi_gest_weeks_filter$N[i] )
  vect <- c(vect,out)
}
vect <- vect[2:length(vect)] # dump the 0 used to intiate vector
#med_single <-   median(vect)
iqr_multiples <- quantile(vect)
total_df <- as.data.frame(rbind(iqr_singleton, iqr_multiples))
type <- rownames(total_df)
total_df <- total_df %>% mutate(year = "total", 
                                median_iqr = paste0(`50%`, " (", `25%`, " - ", `75%`,")"), 
                                type = type) %>% mutate(type = substr(type, 5,100))
##by year singles
list1 <- list()
 for(x in 2005:2020 ){
 # x<-2005
     df <- gest_weeks_filter %>% filter(year==x)
 
  vect<- 0
  for(i in 1:nrow(df) ){
    out <- rep(as.numeric(df$gestation_at_delivery_completed_weeks[i]),
               df$N[i] )
    vect <- c(vect,out)
  }
  vect <- vect[2:length(vect)]
  y <- as.character(x)
  list1[[y]] <- quantile(vect) 
  }

df<- as.data.frame((do.call("rbind", list1)))
df$year <- rownames(df)
df$median_iqr <- paste0(df$`50%`, " (", df$`25%`, " - ", df$`75%`,")")
df$type <- "singleton"
annual_iqr_single <- df

#by year multiples
list1 <- list()
for(x in 2005:2020 ){
  # x<-2005
  df <- multi_gest_weeks_filter %>% filter(year==x)
  
  vect<- 0
  for(i in 1:nrow(df) ){
    out <- rep(as.numeric(df$gestation_at_delivery_completed_weeks[i]),
               df$N[i] )
    vect <- c(vect,out)
  }
  vect <- vect[2:length(vect)]
  y <- as.character(x)
  list1[[y]] <- quantile(vect) 
}

df<-  as.data.frame((do.call("rbind", list1)))
df$year <- rownames(df)
df$median_iqr <- paste0(df$`50%`, " (", df$`25%`, " - ", df$`75%`,")")
df$type <- "multiples"

annual_iqr <- rbind(annual_iqr_single, df)
annual_iqr <- annual_iqr %>% select(year, median_iqr, type) %>%
  pivot_wider(type, names_from = year, values_from = median_iqr)

total_df <- total_df %>% select(year, median_iqr, type) %>%
  pivot_wider(type, names_from = year, values_from = median_iqr)

annual_iqr_tab <- left_join(annual_iqr, total_df)
saveRDS(annual_iqr_tab, "tables/IQR_by_year.rds")

#plots####

#Term groups over time trends####
#•	Plot % of distribution of gestational age grouped by preterm term, post term by year, singletons
single_by_birth_grp <- single_gest_weeks %>% group_by(year, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)


ggplot(data=single_by_birth_grp,
       aes(x = year, y=N_births,group= birth_category, colour=birth_category) ) +
  geom_line()


ggplot(data=single_by_birth_grp,
       aes(x = year, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()

##term births dominate, so plot just non-term births - to make the pattern clearer
ggplot(data=single_by_birth_grp[single_by_birth_grp$birth_category != "Term",],
       aes(x = year, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()

ggplot(data=single_by_birth_grp[single_by_birth_grp$birth_category == "Term",],
       aes(x = year, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()

##multiples 
multi_by_birth_grp <- multi_gest_weeks %>% group_by(year, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)


ggplot(data= multi_by_birth_grp,
       aes(x = year, y=N_births,group= birth_category, colour=birth_category) ) +
  geom_line()


ggplot(data=multi_by_birth_grp,
       aes(x = year, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()


###Preterm only - trends in sub grousp of gestation
single_by_preterm_grp <- single_gest_weeks %>%
  filter(birth_category=="Preterm") %>%
  group_by(year, preterm_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(tot_preterm_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_preterm_births_in_year)

ggplot(data=single_by_preterm_grp,
       aes(x = year, y=N_births,group= preterm_category, colour=preterm_category) ) +
  geom_line()

ggplot(data = single_by_preterm_grp,
       aes(x = year, y=prop_births,group= preterm_category, colour=preterm_category) ) +
  geom_line()

multi_by_preterm_grp <- multi_gest_weeks %>%
  filter(birth_category=="Preterm") %>%
  group_by(year, preterm_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(tot_preterm_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_preterm_births_in_year)

ggplot(data=multi_by_preterm_grp,
       aes(x = year, y=N_births,group= preterm_category, colour=preterm_category) ) +
  geom_line()

ggplot(data = multi_by_preterm_grp,
       aes(x = year, y=prop_births,group= preterm_category, colour=preterm_category) ) +
  geom_line()


#•	Plot % of distribution of gestational age grouped by preterm term, post term by year group (2005-09, 2010-14, 2015-20), singletons
single_gest_weeks <-single_gest_weeks  %>% mutate(year_group = case_when(year %in% 2005:2009~ "2005_2009", 
                                                                       year %in% 2010:2014~ "2010_2014",
                                                                       year %in% 2015:2020~ "2015_2020"))


single_by_birth_grp_yr_grp <- single_gest_weeks %>% group_by(year_group, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year_group) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)


ggplot(data=single_by_birth_grp_yr_grp,
       aes(x = year_group, y=N_births,group= birth_category, colour=birth_category) ) +
  geom_line()


ggplot(data=single_by_birth_grp_yr_grp,
       aes(x = year_group, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()

##term births dominate, so plot just non-term births - to make the pattern clearer
ggplot(data=single_by_birth_grp_yr_grp[single_by_birth_grp_yr_grp$birth_category != "Term",],
       aes(x = year_group, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()

ggplot(data = single_by_birth_grp_yr_grp[single_by_birth_grp_yr_grp$birth_category == "Term",],
       aes(x = year_group, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()


#•	Plot %  of distribution of gestational age grouped by preterm term, post term by year, multiples
multi_by_birth_grp <- multi_gest_weeks %>% group_by(year, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)


ggplot(data=multi_by_birth_grp,
       aes(x = year, y=N_births,group= birth_category, colour=birth_category) ) +
  geom_line()

##ok definite issue with 2019 data here!!
ggplot(data=multi_by_birth_grp,
       aes(x = year, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()

#•	Plot % of distribution of gestational age grouped by preterm term, post term by year group, (2005-09, 2010-14, 2015-20) , multiples
multi_gest_weeks <- multi_gest_weeks %>% mutate(year_group = case_when(year %in% 2005:2009~ "2005_2009", 
                                                                       year %in% 2010:2014~ "2010_2014",
                                                                       year %in% 2015:2020~ "2015_2020"))
multi_by_birth_grp_yr_grp <- multi_gest_weeks %>% group_by(year_group, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year_group) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)


ggplot(data=multi_by_birth_grp_yr_grp ,
       aes(x = year_group, y=N_births,group= birth_category, colour=birth_category) ) +
  geom_line()

##
ggplot(data=multi_by_birth_grp_yr_grp ,
       aes(x = year_group, y=prop_births,group= birth_category, colour=birth_category) ) +
  geom_line()




###plots by age####
single_by_birth_grp_age_total <- gest_by_age_single %>% 
  group_by(maternal_age_at_delivery_years, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(maternal_age_at_delivery_years) %>% 
  mutate(tot_births_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_simd)

multi_by_birth_grp_age_total <- gest_by_age_multi %>% 
  group_by(maternal_age_at_delivery_years, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(maternal_age_at_delivery_years) %>% 
  mutate(tot_births_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_simd)

##
df <- single_by_birth_grp_age_total
df <- df %>%
  mutate(maternal_age_at_delivery_years = 
factor(maternal_age_at_delivery_years, ordered = TRUE, 
       levels = c("<20",   "20-24" ,  "25-29",   "30-34",   "35-39", "≥40"  , "Unknown")) )

ggplot(df, aes(x=birth_category, y= prop_births)) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_age_at_delivery_years))


ggplot(df, aes(x=birth_category, y= N_births)) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_age_at_delivery_years))

df <- multi_by_birth_grp_age_total
df <- df %>%
  mutate(maternal_age_at_delivery_years = 
           factor(maternal_age_at_delivery_years, ordered = TRUE, 
                  levels = c("<20",   "20-24" ,  "25-29",   "30-34",   "35-39", "≥40"  , "Unknown")) )

ggplot(df, aes(x=birth_category, y= prop_births)) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_age_at_delivery_years))

ggplot(df, aes(x=birth_category, y= N_births)) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_age_at_delivery_years))
#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year, singletons – by age
single_by_birth_grp_age <- gest_by_age_single %>% 
  group_by(year,maternal_age_at_delivery_years, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, maternal_age_at_delivery_years) %>% 
  mutate(tot_births_in_year_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year_simd) %>%
  mutate(maternal_age_at_delivery_years = 
           factor(maternal_age_at_delivery_years, ordered = TRUE, 
                  levels = c("<20",   "20-24" ,  "25-29",   "30-34",   "35-39", "≥40"  , "Unknown")) )

single_by_birth_grp_age_table <- gest_by_age_single %>% 
  group_by(year,maternal_age_at_delivery_years, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, maternal_age_at_delivery_years) %>% 
  mutate(tot_births_in_year_age = sum(N_births)) %>%
  ungroup() %>%
  mutate(births_per_1k = N_births/tot_births_in_year_age*1000) %>%
  mutate(maternal_age_at_delivery_years = 
           factor(maternal_age_at_delivery_years, ordered = TRUE, 
                  levels = c("<20",   "20-24" ,  "25-29",   "30-34",   "35-39", "≥40"  , "Unknown")) ) %>%
  select(-tot_births_in_year_age) %>%
  pivot_wider(names_from = year, values_from = c(N_births, births_per_1k))
saveRDS(single_by_birth_grp_age_table, "tables/single_by_age_term_year.rds")

knitr::kable(single_by_birth_grp_age_table)

ggplot(data=single_by_birth_grp_age,
       aes(x = year, y=N_births,
           group=maternal_age_at_delivery_years,
           colour= maternal_age_at_delivery_years) ) +
  geom_line() +
  facet_grid(cols = vars(birth_category))


ggplot(data=single_by_birth_grp_age,
       aes(x = year, y=prop_births,
           group=maternal_age_at_delivery_years,
           colour= maternal_age_at_delivery_years) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")

#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year group (2005-09, 2010-14, 2015-20), singletons– by Age

single_by_birth_grp_age <- single_by_birth_grp_age %>%
  mutate(year_group = case_when(year %in% 2005:2009~ "2005_2009", 
                                year %in% 2010:2014~ "2010_2014",
                                year %in% 2015:2020~ "2015_2020"))
single_by_birth_grp_age_yrgrp <- single_by_birth_grp_age %>%
  group_by(year_group, maternal_age_at_delivery_years, birth_category) %>%
  summarise(N_births = sum(N_births)) %>% ungroup() %>%
  group_by(year_group, maternal_age_at_delivery_years) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)


ggplot(data=single_by_birth_grp_age_yrgrp,
       aes(x = year_group, y=prop_births,
           group= maternal_age_at_delivery_years,
           colour= maternal_age_at_delivery_years) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")

#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year, multiples– by Age
multi_by_birth_grp_age <- gest_by_age_multi %>% 
  group_by(year,maternal_age_at_delivery_years, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, maternal_age_at_delivery_years) %>% 
  mutate(tot_births_in_year_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year_simd) %>%
  mutate(maternal_age_at_delivery_years = 
           factor(maternal_age_at_delivery_years, ordered = TRUE, 
                  levels = c("<20",   "20-24" ,  "25-29",   "30-34",   "35-39", "≥40"  , "Unknown")) )

ggplot(data = multi_by_birth_grp_age,
       aes(x = year, y=N_births,
           group=maternal_age_at_delivery_years,
           colour= maternal_age_at_delivery_years) ) +
  geom_line() +
  facet_grid(cols = vars(birth_category))


ggplot(data= multi_by_birth_grp_age,
       aes(x = year, y=prop_births,
           group=maternal_age_at_delivery_years,
           colour= maternal_age_at_delivery_years) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")
#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year group, (2005-09, 2010-14, 2015-20), multiples– by age
multi_by_birth_grp_age <- multi_by_birth_grp_age %>%
  mutate(year_group = case_when(year %in% 2005:2009~ "2005_2009", 
                                year %in% 2010:2014~ "2010_2014",
                                year %in% 2015:2020~ "2015_2020"))
multi_by_birth_grp_age_yrgrp <- multi_by_birth_grp_age %>%
  group_by(year_group, maternal_age_at_delivery_years, birth_category) %>%
  summarise(N_births = sum(N_births)) %>% ungroup() %>%
  group_by(year_group, maternal_age_at_delivery_years) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)


ggplot(data=multi_by_birth_grp_age_yrgrp,
       aes(x = year_group, y=prop_births,
           group= maternal_age_at_delivery_years,
           colour= maternal_age_at_delivery_years) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")

###plots by simd####
single_by_birth_grp_simd_total <- gest_by_simd_single %>% 
  group_by(maternal_simd_quintile_from_postcode_of_residence, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(tot_births_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_simd)

multi_by_birth_grp_simd_total <- gest_by_simd_multi %>% 
  group_by(maternal_simd_quintile_from_postcode_of_residence, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(tot_births_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_simd)

single_by_birth_grp_simd_table <- gest_by_simd_single %>% 
  group_by(year,maternal_simd_quintile_from_postcode_of_residence, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(tot_births_in_year_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(births_per_1k = N_births/tot_births_in_year_simd*1000) %>%
  mutate( maternal_simd_quintile_from_postcode_of_residence = 
           factor(maternal_simd_quintile_from_postcode_of_residence, ordered = TRUE, 
                  levels = c("1",   "2" ,  "3",   "4",   "5", "Unknown")) ) %>%
  select(-tot_births_in_year_simd) %>%
  pivot_wider(names_from = year, values_from = c(N_births, births_per_1k))

saveRDS(single_by_birth_grp_simd_table, "tables/single_by_simd_term_year.rds")

##term on x an colour by simd
ggplot(single_by_birth_grp_simd_total, 
       aes(x=birth_category, y= prop_births
           )) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_simd_quintile_from_postcode_of_residence))

ggplot(multi_by_birth_grp_simd_total, 
       aes(x=birth_category, y= prop_births
       )) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_simd_quintile_from_postcode_of_residence))
##simd on x an colour by term
ggplot(single_by_birth_grp_simd_total, 
       aes(x=maternal_simd_quintile_from_postcode_of_residence, y= prop_births
       )) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=birth_category))

##•	Plot %  of distribution of gestational age grouped by 
#preterm term, post term by year, singletons – by SIMD
single_by_birth_grp_simd <- gest_by_simd_single %>% 
  group_by(year,maternal_simd_quintile_from_postcode_of_residence, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(tot_births_in_year_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year_simd)

ggplot(data=single_by_birth_grp_simd,
       aes(x = year, y=N_births,
           group=maternal_simd_quintile_from_postcode_of_residence,
           colour= maternal_simd_quintile_from_postcode_of_residence) ) +
  geom_line() +
  facet_grid(cols = vars(birth_category))
#facet by category and free scales probably the best way to see trends by SIMD
ggplot(data=single_by_birth_grp_simd,
       aes(x = year, y=prop_births,
           group=maternal_simd_quintile_from_postcode_of_residence,
           colour= maternal_simd_quintile_from_postcode_of_residence) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")

ggplot(data=single_by_birth_grp_simd,
       aes(x = year, y=prop_births,
           group=interaction(birth_category, maternal_simd_quintile_from_postcode_of_residence),
           colour= maternal_simd_quintile_from_postcode_of_residence) ) +
  geom_line() +
  geom_point(aes(shape = factor(birth_category)), size = 2)

#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year group (2005-09, 2010-14, 2015-20), singletons– by SIMD
#facet by category and free scales probably the best way to see trends by SIMD

single_by_birth_grp_simd <- single_by_birth_grp_simd %>%
  mutate(year_group = case_when(year %in% 2005:2009~ "2005_2009", 
                              year %in% 2010:2014~ "2010_2014",
                              year %in% 2015:2020~ "2015_2020"))
single_by_birth_grp_simd_yrgrp <- single_by_birth_grp_simd %>%
  group_by(year_group, maternal_simd_quintile_from_postcode_of_residence, birth_category) %>%
  summarise(N_births = sum(N_births)) %>% ungroup() %>%
  group_by(year_group, maternal_simd_quintile_from_postcode_of_residence) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)

  
ggplot(data=single_by_birth_grp_simd_yrgrp,
       aes(x = year_group, y=prop_births,
           group=maternal_simd_quintile_from_postcode_of_residence,
           colour= maternal_simd_quintile_from_postcode_of_residence) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")


#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year, multiples– by SIMD
multi_by_birth_grp_simd <- gest_by_simd_multi %>% 
  group_by(year,maternal_simd_quintile_from_postcode_of_residence, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(tot_births_in_year_simd = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year_simd)

ggplot(data=multi_by_birth_grp_simd,
       aes(x = year, y=N_births,
           group=maternal_simd_quintile_from_postcode_of_residence,
           colour= maternal_simd_quintile_from_postcode_of_residence) ) +
  geom_line() +
  facet_grid(cols = vars(birth_category))
#facet by category and free scales probably the best way to see trends by SIMD
ggplot(data=multi_by_birth_grp_simd,
       aes(x = year, y=prop_births,
           group=maternal_simd_quintile_from_postcode_of_residence,
           colour= maternal_simd_quintile_from_postcode_of_residence) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")

ggplot(data=multi_by_birth_grp_simd,
       aes(x = year, y=prop_births,
           group=interaction(birth_category, maternal_simd_quintile_from_postcode_of_residence),
           colour= maternal_simd_quintile_from_postcode_of_residence) ) +
  geom_line() +
  geom_point(aes(shape = factor(birth_category)), size = 2)


#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year group, (2005-09, 2010-14, 2015-20) , multiples– by SIMD

multi_by_birth_grp_simd <- multi_by_birth_grp_simd %>%
  mutate(year_group = case_when(year %in% 2005:2009~ "2005_2009", 
                                year %in% 2010:2014~ "2010_2014",
                                year %in% 2015:2020~ "2015_2020"))
multi_by_birth_grp_simd_yrgrp <- multi_by_birth_grp_simd %>%
  group_by(year_group, maternal_simd_quintile_from_postcode_of_residence, birth_category) %>%
  summarise(N_births = sum(N_births)) %>% ungroup() %>%
  group_by(year_group, maternal_simd_quintile_from_postcode_of_residence) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)


ggplot(data=multi_by_birth_grp_simd_yrgrp,
       aes(x = year_group, y=prop_births,
           group=maternal_simd_quintile_from_postcode_of_residence,
           colour= maternal_simd_quintile_from_postcode_of_residence) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")




#Ethnicity####
#•	Plot of distribution of gestational age grouped by
#preterm term, post term by singleton and multiples
#(NB cannot do trends for ethnicity as only have grouped numbers).
gest_by_ethn_single <- gest_by_ethn_single %>%
  filter(year_group=="2015_2020") %>%
  group_by(maternal_ethnicity_from_smr02_delivery_record, birth_category) %>%
  summarise(N_births = sum(N)) %>%
  ungroup() %>%
  group_by(maternal_ethnicity_from_smr02_delivery_record) %>%
  mutate(total_births_ethn = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/total_births_ethn)

ggplot(gest_by_ethn_single, 
       aes(x=birth_category, y= N_births
       )) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_ethnicity_from_smr02_delivery_record))
p<-ggplot(gest_by_ethn_single, 
       aes(x=birth_category, y= prop_births
       )) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_ethnicity_from_smr02_delivery_record))
p + labs(fill = "maternal ethnicity")

gest_by_ethn_multi <- gest_by_ethn_multi %>%
  filter(year_group=="2015_2020") %>%
  group_by(maternal_ethnicity_from_smr02_delivery_record, birth_category) %>%
  summarise(N_births = sum(N)) %>%
  ungroup() %>%
  group_by(maternal_ethnicity_from_smr02_delivery_record) %>%
  mutate(total_births_ethn = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/total_births_ethn)

ggplot(gest_by_ethn_multi, 
       aes(x=birth_category, y= N_births
       )) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_ethnicity_from_smr02_delivery_record))
ggplot(gest_by_ethn_multi, 
       aes(x=birth_category, y= prop_births
       )) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=maternal_ethnicity_from_smr02_delivery_record))

#Onset####
#(careful with CS - totals and splits by emergency/electives)
##total by gest age group
single_onset_grp <- gest_by_onset_single %>% 
  filter(onset_of_delivery != "Emergency" & onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>%
  group_by(onset_group, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(birth_category) %>% 
  mutate(tot_births = sum(N_births)) %>%
  ungroup() %>% arrange(birth_category) %>%
  mutate(prop_births = N_births/tot_births)

df <-single_onset_grp

ggplot(df, aes(x=birth_category, y= prop_births)) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=onset_group))

multi_onset_grp <- gest_by_onset_multi %>% 
  filter(onset_of_delivery != "Emergency" & onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>%
  group_by(onset_group, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(birth_category) %>% 
  mutate(tot_births = sum(N_births)) %>%
  ungroup() %>% arrange(birth_category) %>%
  mutate(prop_births = N_births/tot_births)

df <-multi_onset_grp

ggplot(df, aes(x=birth_category, y= prop_births)) +
  geom_bar(position = "dodge", stat="identity",
           aes(fill=onset_group))

#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year, singletons – by Onset
single_by_birth_grp_onset <- gest_by_onset_single %>% 
  filter(onset_of_delivery != "Emergency" & onset_of_delivery != "Elective") %>%
   mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                  onset_of_delivery == "Induction of labour" |
                                    onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                  TRUE ~"Unknown")) %>%
  group_by(year, onset_group, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, birth_category) %>% 
  mutate(tot_births_in_year_onset = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year_onset)

df <- single_by_birth_grp_onset %>% filter(birth_category != "Unknown" )
ggplot(data=df,
       aes(x = year, y=prop_births,
           group=onset_group,
           colour= onset_group) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")
#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year group (2005-09, 2010-14, 2015-20), singletons– by Onset 
single_by_birth_grp_onset <- single_by_birth_grp_onset %>%
  mutate(year_group = case_when(year %in% 2005:2009~ "2005_2009", 
                                year %in% 2010:2014~ "2010_2014",
                                year %in% 2015:2020~ "2015_2020"))

single_by_birth_grp_onset_yrgrp <- single_by_birth_grp_onset %>%
  group_by(year_group, onset_group, birth_category) %>%
  summarise(N_births = sum(N_births)) %>% ungroup() %>%
  group_by(year_group, birth_category) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)

df <- single_by_birth_grp_onset_yrgrp
ggplot(data=df,
       aes(x = year_group, y=prop_births,
           group=onset_group,
           colour=onset_group) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")


##alternative view - what proportion of each term category is spotaneous or intiated.
df <- single_by_birth_grp_onset %>%
  group_by(year_group, onset_group, birth_category) %>%
  summarise(N_births = sum(N_births)) %>% ungroup() %>%
  group_by(year_group, birth_category) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)
ggplot(data=df,
       aes(x =year_group, y= prop_births,
           group=birth_category,
           colour= birth_category) ) +
  geom_line() +
  facet_grid(rows = vars(onset_group), scales="free")

#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year, multiples– by Onset
multi_by_birth_grp_onset <- gest_by_onset_multi %>% 
  filter(onset_of_delivery != "Emergency" & onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>%
  group_by(year, onset_group, birth_category) %>%
  summarise(N_births = sum(N)) %>% ungroup() %>%
  group_by(year, birth_category) %>% 
  mutate(tot_births_in_year_onset = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year_onset)

df <- multi_by_birth_grp_onset %>% filter(birth_category != "Unknown" & birth_category != "Post-term")
ggplot(data=df,
       aes(x = year, y=prop_births,
           group=onset_group,
           colour= onset_group) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")
#•	Plot of distribution of gestational age grouped by
#preterm term, post term by year group, (2005-09, 2010-14, 2015-20), multiples– by Onset
multi_by_birth_grp_onset <- multi_by_birth_grp_onset %>%
  mutate(year_group = case_when(year %in% 2005:2009~ "2005_2009", 
                                year %in% 2010:2014~ "2010_2014",
                                year %in% 2015:2020~ "2015_2020"))

multi_by_birth_grp_onset_yrgrp <- multi_by_birth_grp_onset %>%
  group_by(year_group, onset_group, birth_category) %>%
  summarise(N_births = sum(N_births)) %>% ungroup() %>%
  group_by(year_group, birth_category) %>% mutate(tot_births_in_year = sum(N_births)) %>%
  ungroup() %>%
  mutate(prop_births = N_births/tot_births_in_year)

df <- multi_by_birth_grp_onset_yrgrp %>% filter(birth_category != "Unknown" & birth_category != "Post-term")
ggplot(data=df,
       aes(x = year_group, y=prop_births,
           group=onset_group,
           colour=onset_group) ) +
  geom_line() +
  facet_grid(rows = vars(birth_category), scales="free")
