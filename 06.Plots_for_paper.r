###setup####
source(paste0(here::here(), "/00_setup.R"))
fig_path <- "/data/HPS/COPS_non_confi/preterm_births_trends/final_plots"

#load data.####
single_gest_weeks <- readRDS(paste0(data_path, "2_working_data/gest_wks_single.rds"))%>% filter(year!="2020")
single_gest_age <- readRDS(paste0(data_path, "2_working_data/gest_by_age_single.rds"))%>% filter(year!="2020")
single_gest_simd <-  readRDS(paste0(data_path,"2_working_data/gest_by_simd_single.rds"))%>% filter(year!="2020")
single_gest_onset <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_single.rds"))%>% filter(year!="2020")

multi_gest_weeks <- readRDS(paste0(data_path, "2_working_data/gest_wks_multi.rds"))%>% filter(year!="2020")
multi_gest_age <- readRDS(paste0(data_path, "2_working_data/gest_by_age_multi.rds"))%>% filter(year!="2020")
multi_gest_simd <-  readRDS(paste0(data_path,"2_working_data/gest_by_simd_multi.rds"))%>% filter(year!="2020")
multi_gest_onset <- readRDS(paste0(data_path,"2_working_data/gest_by_onset_multi.rds"))%>% filter(year!="2020")

####Table 1 numbers####
##numbers and rates by term group. Preterms by onset and by subgroup and nonset
##need to also add model p values for each as well at some point.
single_gest_groups <- single_gest_weeks %>%
  filter(birth_category != "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  mutate(total_births_yr = sum(n_births)) %>%
  ungroup()  %>%
  mutate(rate_per_100 = n_births/total_births_yr *100) %>%
  mutate(onset_group ="all", preterm_category = " ") 


#paste unknown on the end but dont calculate totals, rates etc
single_gest_unkn <- single_gest_weeks %>%
  filter(birth_category == "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  mutate(total_births_yr = NA) %>%
  ungroup()  %>%
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
  group_by(year) %>% 
  mutate(total_births_yr = sum(N)) %>%
  ungroup() %>% 
  filter(birth_category=="Preterm") %>% 
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100)

single_ptb_onset <- single_ptb_subgrp_onset %>% mutate(preterm_category="All") %>%
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100)

single_ptb_subgroup <- single_ptb_subgrp_onset %>% mutate( onset_group ="All onset") %>%
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100)

############################
###MULTIPLES##################
#################
####Table 1 numbers####
##numbers and rates by term group. Preterms by onset and by subgroup and onset
##need to also add model p values for each as well at some point.
multi_gest_groups <- multi_gest_weeks  %>% 
  filter(birth_category!="Unknown")%>%
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>% # weighting is by year group total for mode with just year in it
  mutate(total_births_yr = sum(n_births)) %>%
  ungroup()  %>%
  mutate(rate_per_100 = n_births/total_births_yr *100) %>%
  mutate(onset_group ="all", preterm_category = " ") 

#paste unknown on the end but dont calculate totals, rates etc
multi_gest_unkn <- multi_gest_weeks  %>% 
  filter(birth_category == "Unknown") %>% 
  group_by(year, birth_category) %>%
  summarise(n_births = sum(N)) %>%
  group_by(year) %>%     # weighting is by year group total for mode with just year in it
  mutate(total_births_yr = NA) %>%
  ungroup()  %>%
  mutate(rate_per_100 = NA) %>%
  mutate(onset_group ="all", preterm_category = " ") 

multi_gest_groups <- rbind(multi_gest_groups, multi_gest_unkn )


multi_ptb_subgrp_onset <- multi_gest_onset %>%
  filter(birth_category!="Unknown")%>%
  filter(onset_of_delivery != "Emergency" & # remove CS breakdown to avoid double counting
           onset_of_delivery != "Elective") %>%
  mutate(onset_group = case_when(onset_of_delivery == "Spontaneous with PROM" |
                                   onset_of_delivery == "Spontaneous without PROM" ~"Spontaneous", 
                                 onset_of_delivery == "Induction of labour" |
                                   onset_of_delivery == "Pre-labour CS" ~ "Provider initiated" , 
                                 TRUE ~"Unknown")) %>% 
  group_by(year) %>% 
  mutate(total_births_yr = sum(N)) %>%
  ungroup() %>% 
  filter(birth_category=="Preterm") %>% 
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(N)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100)

multi_ptb_onset <- multi_ptb_subgrp_onset %>% mutate(preterm_category="All") %>%
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100)

multi_ptb_subgroup <- multi_ptb_subgrp_onset %>% mutate( onset_group ="All onset") %>%
  group_by(year,total_births_yr,birth_category, preterm_category, onset_group) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr *100)

###graphs####
##figure1####
single_gest_groups <- single_gest_groups %>% mutate(birth_category = factor(birth_category, levels = c("Term", "Preterm", "Post-term")))

#tab1a <- single_gest_groups %>% pivot_wider(id_col = c(birth_category), names_from = year, values_from = n_births)

p_ga1 <- ggplot(single_gest_groups %>% filter(birth_category!="Unknown"), 
               aes(x=as.numeric(year), y=rate_per_100 , col=birth_category)) +
  geom_line() +
  ylab("% singleton live births")+
  scale_color_manual(values=c(2, 3, 4))+
  # scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2018))+
  theme_bw()+ ylim(0,94)+
  xlab("Year") +scale_y_break(c(8, 90)) + 
  labs(color='Gestational\n age')+theme(legend.position="bottom", 
                                        axis.text = element_text(size=8),
                                        legend.text= element_text(size=8), 
                                        legend.title = element_text(size=8), 
                                        axis.title = element_text(size=8), 
                                        axis.text.y.right = element_blank(), 
                                        axis.ticks.y.right = element_blank())


multi_gest_groups <- multi_gest_groups %>% mutate(birth_category = factor(birth_category, levels = c("Term", "Preterm", "Post-term")))
p_ga2 <- ggplot(multi_gest_groups %>% filter(birth_category!="Unknown" & birth_category!="Post-term" ), 
                aes(x=as.numeric(year), y=rate_per_100 , col=birth_category)) +
  geom_line() +
  ylab("% multiple live births")+
  scale_color_manual(values=c(3, 4))+
  # scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2018))+
  theme_bw()+ylim(0,70)+
  xlab("Year")  + labs(color='Gestational\n age')+
  theme(legend.position="bottom", 
                                                        axis.text = element_text(size=8),
                                                        legend.text= element_text(size=8), 
                                                        legend.title = element_text(size=8), 
                                                        axis.title = element_text(size=8), 
                                                        axis.text.y.right = element_blank(), 
                                                        axis.ticks.y.right = element_blank())

p_ga2

#legend <- gglegend(p_ga1 + theme(legend.position='bottom'))
p <- grid.arrange(p_ga1, p_ga2, ncol=2, nrow =1)

p <-ggpubr::ggarrange(p_ga1, p_ga2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggpubr::ggarrange(p_ga1, p_ga2, common.legend = TRUE, legend="bottom")

# Extract the legend. Returns a gtable
leg <- get_legend(p_ga1)

#p_ga1 <- p_ga1+ theme(legend.position="none")
p <- aplot::plot_list(p_ga1, p_ga2, as_ggplot(leg), ncol=3, nrow=1, widths =  c(2,2,1))
p

ggsave(plot=p, filename="fig1_ga_p.tiff",width = 6.75, height = 3.75, device='tiff', dpi=300)
#ggsave(plot=p, filename="final_named_figs_for_publication/Fig2.tiff",
#       width = 6.75, height = 3.75, device='tiff', dpi=300, compression = "lzw")

## ####
##weekly GA histograms#####
single_gest_weeks_selected  <- single_gest_weeks %>% 
  filter(birth_category!="Unknown"& (year==2005 | year==2009 | year==2015|year==2019)) %>% 
  group_by(year) %>%
  mutate(total_births_yr = sum(N)) %>%
  ungroup() %>%
  mutate(prop_births = N/total_births_yr*100)
multi_gest_weeks_selected  <- multi_gest_weeks %>% 
  filter(birth_category!="Unknown"& (year==2005 | year==2009 | year==2015|year==2019)) %>% 
  group_by(year) %>%
  mutate(total_births_yr = sum(N)) %>%
  ungroup() %>%
  mutate(prop_births = N/total_births_yr*100)

ga_hist_p1 <- ggplot(single_gest_weeks_selected , 
                   aes(x=as.numeric(gestation_at_delivery_completed_weeks), y=prop_births , col=year)) +
  geom_line() +
  # scale_x_break(c(25,32))+
  ylab("% singleton live births")+
  xlab("Gestational age (weeks)")+theme_bw()+xlim(c(22,44))+
  scale_x_continuous(breaks=seq(22, 44, 2)) +theme(panel.grid.minor = element_blank(), 
                                                   legend.position = "bottom")
                     
ga_hist_p1
ga_hist_p2 <- ggplot(multi_gest_weeks_selected , 
                    aes(x=as.numeric(gestation_at_delivery_completed_weeks), y=prop_births , col=year)) +
  geom_line() +
  # scale_x_break(c(25,32))+
  ylab("% multiple live births")+
  xlab("Gestational age (weeks)")+theme_bw()+xlim(c(22,44))+ scale_x_continuous(breaks=seq(22, 44, 2))+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ga_hist_p1 <- ga_hist_p1 + scale_x_continuous(expand = c(0, 0),breaks=seq(22, 44, 2))  + scale_y_continuous(expand = c(0, 0))
ga_hist_p2 <- ga_hist_p2 + scale_x_continuous(expand = c(0, 0),breaks=seq(22, 44, 2))  + scale_y_continuous(expand = c(0, 0))

 p_ga1 + scale_x_continuous(expand = c(0, 0))  +
   scale_y_continuous(expand = c(0, 0))

#leg <- get_legend(ga_hist_p1)

#ga_hist_p1 <- ga_hist_p1+ theme(legend.position="none")
p <- aplot::plot_list(ga_hist_p1, ga_hist_p2, as_ggplot(leg), ncol=3, nrow=1, widths = c(2,2,1))
p
#p <- grid.arrange(ga_hist_p1, ga_hist_p2, ncol=2, nrow =1)
ggsave(plot=p, filename="fig2_all_ga_hist_p.tiff",width = 6.75, height = 3.75, device='tiff', dpi=300,  compression = "lzw")
#ggsave(plot=p, filename="final_named_figs_for_publication/Fig1.tiff",width = 6.75, height = 3.75, device='tiff', dpi=300,  compression = "lzw")

##new figures for singletons only in main paper

#grid.arrange(ga_hist_p1, p_ga1, ncol=2, nrow =1)

p_ga1+ scale_x_continuous(expand = c(0, 0))  +
  scale_y_continuous(expand = c(0, 0), limits=c(0,93))

p_ga2 + scale_x_continuous(expand = c(0, 0))  +
  scale_y_continuous(expand = c(0, 0), limits=c(0,70))


p <- aplot::plot_list(ga_hist_p1, p_ga1+ scale_x_continuous(expand = c(0, 0))  +
                        scale_y_continuous(expand = c(0, 0), limits=c(0,93)),  ncol=2, nrow=1, widths = c(1,1), heights=c(1,1.1))
ggsave(plot=p, filename="final_named_figs_for_publication/Fig1.tiff",width = 7.75, height = 3.75, device='tiff', dpi=300,  compression = "lzw")

ggsave(plot=p, filename=paste0(fig_path, "Fig1.tiff"),
       width = 7.75, height = 3.75, device='tiff', dpi=300,  compression = "lzw")

p2 <- aplot::plot_list(ga_hist_p2, p_ga2 + scale_x_continuous(expand = c(0, 0))  +
                         scale_y_continuous(expand = c(0, 0), limits=c(0,70)),  ncol=2, nrow=1, widths = c(1,1), heights=c(1,1.1))
ggsave(plot=p2, filename="final_named_figs_for_publication/Supp_Fig1.tiff",width = 7.75, height = 3.75, device='tiff', dpi=300,  compression = "lzw")
ggsave(plot=p2, filename=paste0(fig_path, "Supp_Fig1.tiff"),
       width = 7.75, height = 3.75, device='tiff', dpi=300,  compression = "lzw") 

ga_hist_p1+ggplot2::guides(fill=ggplot2::guide_legend(ncol=2))

hist_table1 <- single_gest_weeks_selected %>% pivot_wider(id_cols = c(single_or_multi, year), names_from = gestation_at_delivery_completed_weeks, values_from = prop_births)
hist_table2 <- multi_gest_weeks_selected %>% pivot_wider(id_cols = c(single_or_multi, year), names_from = gestation_at_delivery_completed_weeks, values_from = prop_births)
hist_table <- rbind(hist_table1, hist_table2)
write_csv(hist_table, "tables/supp_tab_2.csv")

hist_table1 <- single_gest_weeks_selected %>% pivot_wider(id_cols = c(single_or_multi, year), names_from = gestation_at_delivery_completed_weeks, values_from = N)
hist_table2 <- multi_gest_weeks_selected %>% pivot_wider(id_cols = c(single_or_multi, year), names_from = gestation_at_delivery_completed_weeks, values_from = N)
hist_table <- rbind(hist_table1, hist_table2)
write_csv(hist_table, "tables/supp_tab_2a.csv")



##SIMD and age combined preg types####
#adapt so that I can use EGs method to set the axis limits
##age
dfm <- multi_gest_age

df_age_term_m <-  dfm %>% group_by(year,maternal_age_at_delivery_years, birth_category ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year, maternal_age_at_delivery_years) %>% 
  mutate(total_births_year_age = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year_age*100)

###need to order factors for age groups and for preterm groups to get facets in right order. 
df_age_term_m <- df_age_term_m %>% filter(birth_category !="Post-term") %>%
  mutate(year= as.numeric(year), birth_category = factor(birth_category, levels=c("Preterm", "Term")),
         maternal_age_at_delivery_years = factor(maternal_age_at_delivery_years, levels = c("<20","20-24", "25-29", "30-34", "35-39", "≥40", "Unknown")))
df_age_term_m <- df_age_term_m %>% mutate(type = "multiple") 


df <- single_gest_age %>% 
  filter(birth_category!="Unknown")

df_age_term <-  df %>% group_by(year,maternal_age_at_delivery_years, birth_category ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year, maternal_age_at_delivery_years) %>% 
  mutate(total_births_year_age = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year_age*100)
df_age_term <- df_age_term %>%
  mutate(year= as.numeric(year), birth_category = factor(birth_category, levels=c("Preterm", "Term", "Post-term")),
         maternal_age_at_delivery_years = 
           factor(maternal_age_at_delivery_years, levels = c("<20","20-24", "25-29", "30-34", "35-39", "≥40", "Unknown"))) %>%
  mutate(type = "singleton")

df_age_all <- rbind(df_age_term, df_age_term_m) %>% mutate(type = factor(type, levels = c("singleton", "multiple")))

##singles and multiples separately
p_age_s <- ggplot(df_age_term %>% filter(birth_category!="Unknown" & maternal_age_at_delivery_years != "Unknown"), 
                    aes(x=year, y=percent_births, colour= maternal_age_at_delivery_years, group = maternal_age_at_delivery_years)) +
  geom_line() + 
  ylab("rate per 100 live births")+
  scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2018 ))+
  facet_wrap((~ birth_category), scales="free") +  theme_bw()+
  guides(col= guide_legend(title= "Maternal age"))+
  theme(axis.text = element_text(size=8),
      legend.text= element_text(size=8), 
      legend.title = element_text(size=8), 
      axis.title = element_text(size=8), 
      axis.text.y.right = element_blank(), 
      axis.ticks.y.right = element_blank(),
      legend.position='bottom', 
      panel.grid.minor = element_blank())

p_age_m <- ggplot(df_age_term_m %>% filter(birth_category!="Unknown" & maternal_age_at_delivery_years != "Unknown"), 
                  aes(x=year, y=percent_births, colour= maternal_age_at_delivery_years, group = maternal_age_at_delivery_years)) +
  geom_line() + 
  ylab("rate per 100 live births")+
  scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2018 ))+
  facet_wrap((~ birth_category), scales="free") +  theme_bw()+
  guides(col= guide_legend(title= "Maternal age"))+
  theme(axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        axis.title = element_text(size=8), 
        axis.text.y.right = element_blank(), 
        axis.ticks.y.right = element_blank(),
        legend.position='bottom', 
        panel.grid.minor = element_blank())

ggsave(plot=p_age_s,
       filename="final_named_figs_for_publication/Fig3.tiff",width = 7.75, height = 3.75, device='tiff', dpi=300)

ggsave(plot=p_age_m,
       filename="final_named_figs_for_publication/Supp_Fig3.tiff",width = 7.75, height = 3.75, device='tiff', dpi=300)

##simd
dfs <- single_gest_simd  %>% 
  filter(birth_category!="Unknown")

df_term_s <-  dfs %>% group_by(year,maternal_simd_quintile_from_postcode_of_residence, birth_category ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(total_births_year_age = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year_age*100)

##
df_term_s<- df_term_s %>%
  mutate(year= as.numeric(year),birth_category = factor(birth_category, levels=c("Preterm", "Term", "Post-term"))) %>%
  mutate(type = "singleton")


df <- multi_gest_simd  %>% filter(birth_category!="Unknown")

df_term_m <-  df %>% group_by(year,maternal_simd_quintile_from_postcode_of_residence, birth_category ) %>% 
  summarise(n_births= sum(N)) %>% ungroup() %>%
  group_by(year, maternal_simd_quintile_from_postcode_of_residence) %>% 
  mutate(total_births_year_age = sum(n_births)) %>% ungroup() %>%
  mutate(percent_births = n_births/total_births_year_age*100)

df_term_m<- df_term_m %>% filter(birth_category !="Post-term") %>%
  mutate(year= as.numeric(year),birth_category = factor(birth_category, levels=c("Preterm", "Term"))) %>% mutate(type = "multiple") 


df_simd_all <- rbind(df_term_s, df_term_m) %>% mutate(type = factor(type, levels = c("singleton", "multiple")))


p_simd_single <- ggplot(df_term_s %>% filter(birth_category!="Unknown" & maternal_simd_quintile_from_postcode_of_residence!="Unknown"), 
       aes(x=year, y=percent_births, colour= maternal_simd_quintile_from_postcode_of_residence, group = maternal_simd_quintile_from_postcode_of_residence)) +
  geom_line()+guides(col= guide_legend(title= "Maternal SIMD"))+
  ylab("rate per 100 live births")+
  scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2018 ))+
  facet_wrap((~ birth_category), scales="free")+
  theme_bw()+
    theme(axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        axis.title = element_text(size=8), 
        axis.text.y.right = element_blank(), 
        axis.ticks.y.right = element_blank(),
        legend.position='bottom', 
        panel.grid.minor = element_blank())


p_simd_multi <- ggplot(df_term_m %>% filter(birth_category!="Unknown" & maternal_simd_quintile_from_postcode_of_residence!="Unknown"), 
                        aes(x=year, y=percent_births, colour= maternal_simd_quintile_from_postcode_of_residence, group = maternal_simd_quintile_from_postcode_of_residence)) +
  geom_line()+guides(col= guide_legend(title= "Maternal SIMD"))+
  ylab("rate per 100 live births")+
  scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2018 ))+
  facet_wrap((~ birth_category), scales="free")+
  theme_bw()+
  theme(axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        axis.title = element_text(size=8), 
        axis.text.y.right = element_blank(), 
        axis.ticks.y.right = element_blank(),
        legend.position='bottom', 
        panel.grid.minor = element_blank())


ggsave(plot=p_simd_single+ scale_colour_manual(values =c(2,3,4,5,6) , 
                                            limits =  c("1", "2", "3", "4", "5")),
       filename="final_named_figs_for_publication/fig4.tiff", 
       width = 8.75, height = 3.75, device='tiff', dpi=300, compression="lzw")


ggsave(plot=p_simd_multi+ scale_colour_manual(values =c(2,3,4,5,6) , 
                                               limits =  c("1", "2", "3", "4", "5")),
       filename="final_named_figs_for_publication/Supp_fig4.tiff", 
       width = 5.75, height = 3.75, device='tiff', dpi=300, compression="lzw")

p_simd_all <- ggplot(df_simd_all %>% filter(birth_category!="Unknown" & maternal_simd_quintile_from_postcode_of_residence!="Unknown"), 
                     aes(x=year, y=percent_births, colour= maternal_simd_quintile_from_postcode_of_residence, group = maternal_simd_quintile_from_postcode_of_residence)) +
  geom_line() +
  guides(col= guide_legend(title= "Maternal SIMD"))+
  ylab("rate per 100 live births")+
  scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2018 ))+
  facet_wrap((~type + birth_category), scales="free")+
  theme_bw()+
  theme(legend.position='top')

p_simd_all

names(df_age_all)
names(df_simd_all)

df_simd_all <- df_simd_all %>% rename(percent_births_simd  = percent_births)
df_age_all <- df_age_all %>% rename(percent_births_age  = percent_births)

df_all_demog <- bind_rows(df_simd_all, df_age_all)

##this is not working yet - to avoid having a plot labelled NA I think I need to combine approaches
##and add rows to each dataframe with the facet names and values fir the other percent columne only
#and use hline
df_all_demog <- df_all_demog %>%
#  mutate(maternal_simd_quintile_from_postcode_of_residence = case_when(maternal_simd_quintile_from_postcode_of_residence=="Unknown" ~"", 
#                                                    T~maternal_simd_quintile_from_postcode_of_residence)) %>%
  mutate(maternal_simd_quintile_from_postcode_of_residence = 
           factor(maternal_simd_quintile_from_postcode_of_residence,
                  levels = c("1", "2", "3", "4", "5", "Unknown"))) %>%
  filter(birth_category!="Unknown" ) %>% filter(maternal_simd_quintile_from_postcode_of_residence!="Unknown"| is.na(maternal_simd_quintile_from_postcode_of_residence)) %>%
  filter(maternal_age_at_delivery_years !="Unknown" | is.na(maternal_age_at_delivery_years))

p_simd_all <- ggplot(df_all_demog %>% filter(birth_category!="Unknown"  & !is.na(maternal_simd_quintile_from_postcode_of_residence)), 
                     aes(x=year, y=percent_births, colour= maternal_simd_quintile_from_postcode_of_residence, group = maternal_simd_quintile_from_postcode_of_residence)) +
  geom_line() +facet_wrap((~type + birth_category), scales="free")+
  guides(col= guide_legend(title= "Maternal SIMD"))+
  ylab("rate per 100 live births")+
  scale_x_continuous(breaks=c(2008,  2012,  2016))+
  #geom_blank(data = df_all_demog,
  #           aes(y = percent_births_age)) +
  theme_bw()+
  theme(legend.position='top', axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        strip.text.x = element_text(size=8))
p_simd_all+ scale_colour_manual(values =c(2,3,4,5,6, "white") , 
                                limits =  c("1", "2", "3", "4", "5", ""))
##Leave out, replaced with separate figures for multiples and singletons
#ggsave(plot=p_simd_all+ scale_colour_manual(values =c(2,3,4,5,6, "white") , 
#                                            limits =  c("1", "2", "3", "4", "5", "")),
#       filename="fig4_simd_p.tiff",width = 6.75, height = 3.75, device='tiff', dpi=300)

#ggsave(plot=p_simd_all+ scale_colour_manual(values =c(2,3,4,5,6, "white") , 
#                                            limits =  c("1", "2", "3", "4", "5", "")),
#       filename="final_named_figs_for_publication/fig4.tiff", 
#       width = 6.75, height = 3.75, device='tiff', dpi=300, compression="lzw")

df_all_demog <- bind_rows(df_simd_all, df_age_all)
df_age <- df_age_all %>%
  mutate(maternal_age_at_delivery_years = case_when(maternal_age_at_delivery_years=="Unknown" ~"", 
                                                    T~maternal_age_at_delivery_years)) %>%
  mutate(maternal_age_at_delivery_years = 
           factor(maternal_age_at_delivery_years,
                  levels = c("<20", "20-24", "25-29", "30-34", "35-39", "≥40", ""))) %>%
   filter(birth_category!="Unknown" )  %>%
  filter(maternal_age_at_delivery_years !="")
  
p_age_all <- ggplot(df_age %>% filter(birth_category!="Unknown" &  
                                              !is.na(maternal_age_at_delivery_years)), 
             aes(x=year, y=percent_births, colour= maternal_age_at_delivery_years, group = maternal_age_at_delivery_years)) +
  geom_line() +facet_wrap((~type + birth_category), scales="free")+
  guides(col= guide_legend(title= "Maternal age"))+
  ylab("rate per 100 live births")+
  scale_x_continuous(breaks=c( 2008,  2012,  2016 ))+
 # geom_blank(data = df_all_demog,
#             aes(y = percent_births_simd,x=year)) +
  theme_bw()+
  theme(legend.position='top', axis.text = element_text(size=6),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        strip.text.x = element_text(size=8))

p_age_all + scale_colour_manual(values =c(2,3,8,4,5,6, "white") , 
                                limits = c("<20", "20-24", "25-29", "30-34", "35-39", "≥40", ""))





ggsave(plot=p_age_all + scale_colour_manual(values =c(2,3,8,4,5,6, "white") , 
                                            limits = c("<20", "20-24", "25-29", "30-34", "35-39", "≥40", "")),
       filename="fig3_age_p.tiff",width = 6.75, height = 3.75, device='tiff', dpi=300)

ggsave(plot=p_age_all + scale_colour_manual(values =c(2,3,8,4,5,6, "white") , 
                                            limits = c("<20", "20-24", "25-29", "30-34", "35-39", "≥40", "")),
       filename="final_named_figs_for_publication/fig3.tiff", width = 6.75, height = 3.75, device='tiff', dpi=300, 
       compression="lzw")


###Figure 2 &  Supp fig 2####
#ga  (ptb only)
##do singles and multiples in one plot

ga_subgrp_s <- single_ptb_subgroup  %>% filter(onset_group!="Unknown")  %>%
  mutate(year= as.numeric(year), 
         preterm_category = 
           factor(preterm_category, levels=c("Moderate-Late Preterm", "Very Preterm","Extremely Preterm" )))

ga_subgrp_p1 <- ggplot(ga_subgrp_s, 
                      aes(x=as.numeric(year), y=rate_per_100, colour=preterm_category, group = preterm_category)) +
  geom_line(aes( colour = preterm_category)) +
  guides(col= guide_legend(title= c("Gestation")),
         linetype=guide_legend("Onset")) +
  ylab("% singleton live births")+
  scale_x_continuous(breaks=c(2006, 2008, 2010, 2012,2014, 2016,2018 ))+
  theme_bw()+
  #  theme(legend.position='top')+
  xlab("year")+ylim(0,7)+ 
  scale_color_discrete(name = "Gestation", labels = c("Moderate-late\n preterm", "Very preterm",
                                                      "Extremely \npreterm"))+
  theme(axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        axis.title = element_text(size=8), 
        axis.text.y.right = element_blank(), 
        axis.ticks.y.right = element_blank())

ga_subgrp_p1  +scale_y_break(c(1,4))


ga_subgrp_m <- multi_ptb_subgroup  %>% filter(onset_group!="Unknown")  %>%
  mutate(year= as.numeric(year), 
         preterm_category = 
           factor(preterm_category, levels=c("Moderate-Late Preterm", "Very Preterm","Extremely Preterm" )))
ga_subgrp_p2 <- ggplot(ga_subgrp_m, 
                      aes(x=as.numeric(year), y=rate_per_100, colour=preterm_category, group = preterm_category)) +
  geom_line(aes( colour = preterm_category)) +
  guides(col= guide_legend(title= c("Gestation")),
         linetype=guide_legend("Onset")) +
  ylab("%  multiple live births")+
  scale_x_continuous(breaks=c( 2006,2008, 2010, 2012,2014, 2016,2018 ))+
  theme_bw()+
  #  theme(legend.position='top')+
  xlab("year")+ylim(0,60)+ 
  scale_color_discrete(name = "Gestation", labels = c("Moderate-late\n preterm", "Very preterm", 
                                                      "Extremely \npreterm"))+
  theme(axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        axis.title = element_text(size=8), 
        axis.text.y.right = element_blank(), 
        axis.ticks.y.right = element_blank())

ga_subgrp_p2  +scale_y_break(c(10,40))

#leg <- get_legend(ga_subgrp_p2)
##ga_subgrp_p2 <- ga_subgrp_p2+ theme(legend.position="none") +scale_y_break(c(10,40))
#ga_subgrp_p1 <- ga_subgrp_p1+ theme(legend.position="none") +scale_y_break(c(1,4))
#p <- aplot::plot_list(ga_subgrp_p1, ga_subgrp_p2, as_ggplot(leg), ncol=3, nrow=1, widths = c(2.5,2.5,1))
#png(filename = "fig5_ptb_ga_all.png",width = 6.75, height = 3.75, res = 300, units = "in")
#p  
#dev.off()
#ggsave(plot=p, filename="final_named_figs_for_publication/Fig5.tiff",
 ga_subgrp_p1  +scale_y_break(c(1,4))
ggsave(plot= ga_subgrp_p1  +scale_y_break(c(1,4)), filename="final_named_figs_for_publication/Fig2.tiff",
       width = 5.75, height = 3.75, device='tiff', dpi=300, compression="lzw")
ggsave(plot= ga_subgrp_p2  + scale_y_break(c(10,40)), filename="final_named_figs_for_publication/Supp_Fig2.tiff",
       width = 5.75, height = 3.75, device='tiff', dpi=300, compression="lzw")

###Figure 6####
###Version 1 (2 graphs)
#ga and onset (ptb only)####
ga_onset_s <- single_ptb_subgrp_onset %>% filter(onset_group!="Unknown")  %>%
  mutate(year= as.numeric(year), 
         preterm_category = factor(preterm_category, levels=c("Moderate-Late Preterm", "Very Preterm","Extremely Preterm" )), 
         onset_group = factor(onset_group, levels = c("Spontaneous", "Provider initiated")))


ga_onset_s <- ga_onset_s %>% mutate(pt_onset  =paste0(preterm_category, onset_group) )
ga_onset_p1 <- ggplot(ga_onset_s, 
                     aes(x=as.numeric(year), y=rate_per_100, colour=pt_onset , group = pt_onset)) +
  geom_line(aes(linetype=onset_group, colour = preterm_category)) +
  guides(col= guide_legend(title= c("Gestation")),
         linetype=guide_legend("Onset")) +
  ylab("rate per 100 singleton live births")+
  scale_x_continuous(breaks=c( 2008,  2012, 2016 ))+
  theme_bw()+
  #  theme(legend.position='top')+
  xlab("year")+ylim(0,4)+
  theme(axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        axis.title = element_text(size=8))

ga_onset_p1 +scale_y_break(c(0.6,1.7))


ga_onset_m <- multi_ptb_subgrp_onset %>% filter(onset_group!="Unknown")  %>%
  mutate(year= as.numeric(year), 
         preterm_category = factor(preterm_category, levels=c("Moderate-Late Preterm", "Very Preterm","Extremely Preterm" )), 
         onset_group = factor(onset_group, levels = c("Spontaneous", "Provider initiated")))


ga_onset_m  <- ga_onset_m  %>% mutate(pt_onset  =paste0(preterm_category, onset_group) )
ga_onset_p2 <- ggplot(ga_onset_m , 
                     aes(x=as.numeric(year), y=rate_per_100, colour=pt_onset , group = pt_onset)) +
  geom_line(aes(linetype=onset_group, colour = preterm_category)) +
  guides(col= guide_legend(title= c("Gestation")),
         linetype=guide_legend("Onset")) +
  ylab("rate per 100 multiple live births")+
  scale_x_continuous(breaks=c( 2008,  2012, 2016 ))+
  theme_bw()+
  #  theme(legend.position='top')+
  xlab("year")+
  theme(axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        axis.title = element_text(size=8))


ga_onset_p2 +scale_y_break(c(7,15))


leg <- get_legend(ga_onset_p2 )

ga_onset_p2 <- ga_onset_p2 + theme(legend.position="none") +scale_y_break(c(7,15))
ga_onset_p1  <- ga_onset_p1 + theme(legend.position="none") +scale_y_break(c(0.6,1.7))
p <- aplot::plot_list(ga_onset_p1, ga_onset_p2, as_ggplot(leg), ncol=3, nrow=1, widths = c(2.5,2.5,1))
p


png(filename = "fig6_ptb_ga_onset.png",width = 6.75, height = 3.75, res = 300, units = "in")
p  
dev.off()


##version 2 facet so that each multiple/singleton subgroup has a panel with 2 lines for onset groups
ga_onset_m$type <-"multiple"
ga_onset_s$type <-"singleton"
ga_onset_all <- rbind(ga_onset_m, ga_onset_s)

p<- ggplot(ga_onset_all  , 
       aes(x=as.numeric(year), y=rate_per_100, colour=onset_group , group = onset_group)) +
  geom_line(aes(colour=onset_group)) +
  facet_wrap((~ type+preterm_category), scales="free") +  theme_bw()+
  guides(col= guide_legend(title= ""))+
  theme(legend.position='top') +
  ylab("rate per 100 multiple live births")+
  xlab("Year")+  guides(col= guide_legend(title= c("Onset of labour"))) +
  theme(legend.position='top', axis.text = element_text(size=6),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        strip.text.x = element_text(size=8),  axis.title = element_text(size=8))

  p
png(filename = "fig6_ptb_ga_onset_v2.png",width = 6.75, height = 3.75, res = 300, units = "in")
p  
dev.off()



###combined fig 5 and 6 
ga_onset_s$type <- "singleton"
ga_onset_m$type <- "multiple"

ga_onset_subgrp_all <- ga_onset_all %>% group_by(year,total_births_yr, birth_category, preterm_category, type) %>%
  summarise(n_births = sum(n_births)) %>%
  ungroup() %>%
  mutate(rate_per_100 = n_births/total_births_yr*100, onset_group= "All onset") 




ga_onset_subgrp_all <- rbind(ga_onset_subgrp_all , ga_onset_s, ga_onset_m)


p<- 
  ggplot(ga_onset_subgrp_all, 
       aes(x=as.numeric(year), y=rate_per_100, colour=onset_group , group = onset_group)) +
  geom_line(aes(colour=onset_group)) +
  facet_wrap((~ type+preterm_category), scales="free") +  theme_bw()+
  guides(col= guide_legend(title= ""))+
  theme(legend.position='top') +
  ylab("rate per 100 multiple live births")+
  xlab("Year")+  guides(col= guide_legend(title= c("Onset of labour"))) +
  theme(legend.position='top', axis.text = element_text(size=8),
        legend.text= element_text(size=8), 
        legend.title = element_text(size=8), 
        strip.text.x = element_text(size=8),  axis.title = element_text(size=8))

p + scale_colour_brewer(palette="Set2")
p + scale_colour_manual(values = c("#444444", "#0066CC", "#99CC66"))

png(filename = "fig5_6_combined.png",width = 6.75, height = 3.75, res = 300, units = "in")
p  
dev.off()

ggsave(plot=p + scale_colour_manual(values = c("#666666", "#0066CC", "#99CC66")),
       filename="final_named_figs_for_publication/Fig6.tiff",
       width = 6.75, height = 3.75, device='tiff', dpi=300, compression="lzw")

