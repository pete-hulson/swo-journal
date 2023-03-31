# load ----
library(tidytable)
library(ggplot2)
library(gg.layers)
theme_set(afscassess::theme_report())

# data ----
# species_code and common names
spec <- vroom::vroom(here::here('data', 'species_code_name.csv')) %>% 
  tidytable::mutate(species_name = stringr::str_to_sentence(species_name))

age_iss <- vroom::vroom(here::here('output', 'agesub_iss.csv')) %>% 
  tidytable::filter(!(species_code %in% c(10112, 10115, 10180))) %>% 
  tidytable::mutate(p_base = sub_iss_age / base_iss_age,
                    sub_samp = gsub("a", "", sub_samp),
                    prop_samp = as.numeric(sub_samp) * .01,
                    sub_samp = factor(sub_samp, level = c('25', '50', '75', '90')),
                    surv_labs = case_when(region == 'goa' ~ "GOA",
                                          region == 'ai' ~ "AI",
                                          region == 'bs' ~ "EBS")) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::drop_na()
  
age_ess <- vroom::vroom(here::here('output', 'agesub_ess.csv')) %>% 
  tidytable::filter(!(species_code %in% c(10112, 10115, 10180))) %>% 
  tidytable::mutate(p_base = sub_ess_age / base_ess_age,
                    sub_samp = gsub("a", "", sub_samp),
                    sub_samp = factor(sub_samp, level = c('25', '50', '75', '90')),
                    surv_labs = case_when(region == 'goa' ~ "GOA",
                                          region == 'ai' ~ "AI",
                                          region == 'bs' ~ "EBS")) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::drop_na()

len_iss <- vroom::vroom(here::here('output', 'totlen_iss.csv')) %>% 
  tidytable::filter(!(species_code %in% c(10112, 10115, 10180))) %>% 
            tidytable::mutate(p_base = sub_iss_length / base_iss_length,
                              sub_samp = gsub("t", "", sub_samp),
                              sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250')),
                              surv_labs = case_when(region == 'goa' ~ "GOA",
                                                    region == 'ai' ~ "AI",
                                                    region == 'bs' ~ "EBS")) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::drop_na()
            
len_ess <- vroom::vroom(here::here('output', 'totlen_ess.csv')) %>% 
                tidytable::filter(!(species_code %in% c(10112, 10115, 10180))) %>% 
                tidytable::mutate(p_base = sub_ess_length / base_ess_length,
                                  sub_samp = gsub("t", "", sub_samp),
                                  sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250')),
                                  surv_labs = case_when(region == 'goa' ~ "GOA",
                                                        region == 'ai' ~ "AI",
                                                        region == 'bs' ~ "EBS"))  %>% 
  tidytable::left_join(spec) %>% 
  tidytable::drop_na()

surv_labs <- c("Aleutian Islands", "Bering Sea Shelf", "Gulf of Alaska")
names(surv_labs) <- c("ai", "bs", "goa")

# iss plots ----
# goa ----
# goa age iss
age_iss %>% 
  tidytable::filter(region == 'goa') %>% 
  ggplot(aes(sub_samp, sub_iss_age)) + 
  geom_boxplot2(fill = "lightgray", width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Age composition input sample size\n") 

# goa length iss
len_iss %>% 
  tidytable::filter(region == 'goa', !is.na(species_name)) %>% 
  ggplot(aes(sub_samp, sub_iss_length)) + 
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Length composition input sample size\n") 

# ebs ----
# ebs age iss
age_iss %>% 
  tidytable::filter(region == 'bs') %>% 
  ggplot(aes(sub_samp, sub_iss_age)) + 
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Age composition input sample size\n") 

# ebs length iss
len_iss %>% 
  tidytable::filter(region == 'bs', !is.na(species_name)) %>% 
  ggplot(aes(sub_samp, sub_iss_length)) + 
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Length composition input sample size\n") 

# ai ----
# ai age iss
age_iss %>% 
  tidytable::filter(region == 'ai') %>% 
  ggplot(aes(sub_samp, sub_iss_age)) + 
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Age composition input sample size\n") 


# ai length iss
len_iss %>% 
  tidytable::filter(region == 'ai', !is.na(species_name)) %>% 
  ggplot(aes(sub_samp, sub_iss_length)) + 
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Length composition input sample size\n") 

# all ----
# all regions/species length iss
len_iss %>% 
  tidytable::filter(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                       "Pacific cod", "Pacific ocean perch")) %>% 
  ggplot(aes(sub_samp, p_base)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar=0, fill = "lightgray") +
  facet_grid(species_name ~ surv_labs) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Proportion of full dataset input sample size\n")

# all regions/species age iss
age_iss %>% 
  tidytable::filter(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                       "Pacific cod", "Pacific ocean perch")) %>% 
  ggplot(aes(sub_samp, p_base)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ surv_labs) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("Age sub-sampling level (%)") +
  ylab("Proportion of full dataset input sample size\n")

# slope ----
# all regions/species length iss
age_iss %>% 
  tidytable::summarise(p_base = mean(p_base),
                       prop_samp = mean(prop_samp),
                       .by = c(species_code, comp_type, region, species_name, species_type, sub_samp)) %>% 
  ggplot(aes(prop_samp, p_base, shape = species_name, color = comp_type)) +
  geom_point() +
  scale_shape_manual(name = "Species", values=seq(0,14)) +
  facet_grid(region ~ species_type,
             labeller = labeller(region = surv_labs)) +
  geom_abline(slope = 1, intercept = 0, lty = 3) +
  expand_limits(x=c(0.25,1), y = c(0.25,1)) + 
  scico::scale_color_scico_d(name = "Composition type", 
                             palette = 'roma') +
  xlab("\nProportion of age sub-sampling") +
  ylab("Proportion of full dataset input sample size\n")

# ess plots ----
# goa
# goa ----
# goa age ess
age_ess %>% 
  tidytable::filter(region == 'goa') %>% 
  ggplot(aes(sub_samp, sub_ess_age)) + 
  geom_boxplot2(fill = "lightgray", width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Age composition effective sample size\n") 

# goa length ess
len_ess %>% 
  tidytable::filter(region == 'goa', !is.na(species_name)) %>% 
  ggplot(aes(sub_samp, sub_ess_length)) + 
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Length composition effective sample size\n") 

# ebs ----
# bs age iss
age_ess %>% 
  tidytable::filter(region == 'bs') %>% 
  ggplot(aes(sub_samp, sub_ess_age)) + 
  geom_boxplot2(fill = "lightgray", width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Age composition effective sample size\n") 

# bs length ess
len_ess %>% 
  tidytable::filter(region == 'bs', !is.na(species_name)) %>% 
  ggplot(aes(sub_samp, sub_ess_length)) + 
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Length composition effective sample size\n") 

# ai ----
# ai age ess
age_ess %>% 
  tidytable::filter(region == 'ai') %>% 
  ggplot(aes(sub_samp, sub_ess_age)) + 
  geom_boxplot2(fill = "lightgray", width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Age composition effective sample size\n") 


# ai length ess
len_ess %>% 
  tidytable::filter(region == 'ai', !is.na(species_name)) %>% 
  ggplot(aes(sub_samp, sub_ess_length)) + 
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Length composition effective sample size\n") 
  
# all ----
# all regions/species length iss
len_ess %>% 
  tidytable::filter(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                       "Pacific cod", "Pacific ocean perch")) %>% 
  ggplot(aes(sub_samp, p_base)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar=0, fill = "lightgray") +
  facet_grid(species_name ~ surv_labs) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Proportion of full dataset effective sample size\n")

# all regions/species age iss
age_ess %>% 
  tidytable::filter(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                       "Pacific cod", "Pacific ocean perch")) %>% 
  ggplot(aes(sub_samp, p_base)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ surv_labs) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("Age sub-sampling level (%)") +
  ylab("Proportion of full dataset effective sample size\n")
