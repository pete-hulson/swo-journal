# load ----
library(tidytable)
library(ggplot2)
library(gg.layers)
library(extrafont)
theme_set(afscassess::theme_report())
# font_import()
loadfonts(device="win")   

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

# functions ----
iss_plot <- function(data, type='age', region='goa') {
  
  if(type=="age"){
    data %>% 
      tidytable::filter(region == region) %>% 
      ggplot(aes(sub_samp, sub_iss_age)) + 
      geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
      facet_grid(species_name ~ comp_type, 
                 scales = "free",
                 labeller = label_wrap_gen(10)) +
      theme(strip.text.y.right = element_text(angle = 0)) +
      xlab("\nAge sub-sampling level (%)") +
      ylab("Age composition input sample size\n") 
    
  } else {
    
    data %>% 
      tidytable::filter(region == region) %>% 
      ggplot(aes(sub_samp, sub_iss_length)) + 
      geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
      facet_grid(species_name ~ comp_type, 
                 scales = "free",
                 labeller = label_wrap_gen(10)) +
      theme(strip.text.y.right = element_text(angle = 0)) +
      xlab("\nSexed lengths sample size") +
      ylab("Length composition input sample size\n") 
  }
}

ess_plot <- function(data, type='age', region='goa') {
  
  if(type=="age"){
    data %>% 
      tidytable::filter(region == region) %>% 
      ggplot(aes(sub_samp, sub_ess_age)) + 
      geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
      facet_grid(species_name ~ comp_type, 
                 scales = "free",
                 labeller = label_wrap_gen(10)) +
      theme(strip.text.y.right = element_text(angle = 0)) +
      xlab("\nAge sub-sampling level (%)") +
      ylab("Age composition effective sample size\n") 
    
  } else {
    
    data %>% 
      tidytable::filter(region == region) %>% 
      ggplot(aes(sub_samp, sub_ess_length)) + 
      geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
      facet_grid(species_name ~ comp_type, 
                 scales = "free",
                 labeller = label_wrap_gen(10)) +
      theme(strip.text.y.right = element_text(angle = 0)) +
      xlab("\nSexed lengths sample size") +
      ylab("Length composition effective sample size\n") 
  }
}


# iss plots by region ----
# likely show 1 and the rest go in supplemental data?
## age ----
png(filename=here::here("figs", "alt_age_iss_goa.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

iss_plot(age_iss, type="age", region = "goa")

dev.off()

png(filename=here::here("figs", "alt_age_iss_bs.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

iss_plot(age_iss, type="age", region = "bs")

dev.off()

png(filename=here::here("figs", "alt_age_iss_ai.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

iss_plot(age_iss, type="age", region = "ai")

dev.off()

## length ----

png(filename=here::here("figs", "alt_len_iss_goa.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

iss_plot(len_iss, type="length", region = "goa")

dev.off()

png(filename=here::here("figs", "alt_len_iss_bs.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

iss_plot(len_iss, type="length", region = "bs")

dev.off()

png(filename=here::here("figs", "alt_len_iss_ai.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

iss_plot(len_iss, type="length", region = "ai")

dev.off()

# ess plots by region ----
## age ----
png(filename=here::here("figs", "alt_age_ess_goa.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

ess_plot(age_ess, type="age", region = "goa")

dev.off()

png(filename=here::here("figs", "alt_age_ess_bs.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

ess_plot(age_ess, type="age", region = "bs")

dev.off()

png(filename=here::here("figs", "alt_age_ess_ai.png"), 
    width = 6.5, height = 8.0,
    units = "in", res=200)

ess_plot(age_ess, type="age", region = "ai")

dev.off()

## length ----

png(filename=here::here("figs", "alt_len_ess_goa.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

ess_plot(len_ess, type="length", region = "goa")

dev.off()

png(filename=here::here("figs", "alt_len_ess_bs.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

ess_plot(len_ess, type="length", region = "bs")

dev.off()

png(filename=here::here("figs", "alt_len_ess_ai.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

ess_plot(len_ess, type="length", region = "ai")

dev.off()


# all ----
## all regions/species length iss
# selected a couple of example species - the rest can go in supplemental figs?
png(filename=here::here("figs", "alt_props_iss_length.png"), 
    width = 6.5, height = 6.5,
    units = "in", res=200)

len_iss %>% 
  tidytable::filter(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                       "Pacific cod", "Pacific ocean perch")) %>% 
  ggplot(aes(sub_samp, p_base, fill = comp_type)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar=0, 
                width = 0.9) +
  facet_grid(species_name ~ surv_labs,
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle=0)) +
  scico::scale_fill_scico_d("Composition type", 
                            palette='roma', alpha=0.5) +
  xlab("\nSexed lengths sample size") +
  ylab("Proportion of full dataset input sample size by haul\n") +
  theme(legend.position = 'bottom')

dev.off()

# all regions/species age iss
png(filename=here::here("figs", "alt_props_iss_age.png"), 
    width = 6.5, height = 6.5,
    units = "in", res=200)

age_iss %>% 
  tidytable::filter(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                       "Pacific cod", "Pacific ocean perch")) %>% 
  ggplot(aes(sub_samp, p_base, fill = comp_type)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9) +
  facet_grid(species_name ~ surv_labs,
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scico::scale_fill_scico_d("Composition type", 
                            palette='roma', alpha=0.5) +
  xlab("Age sub-sampling level (%)") +
  ylab("Proportion of full dataset input sample size\n") +
  theme(legend.position = 'bottom') +
  scale_y_continuous(breaks = seq(0.25, 1, 0.25))

dev.off()

# slope ----
# all regions/species length iss
png(filename=here::here("figs", "alt_p_age_iss_nss.png"), 
              width = 6.5, height = 6.5,
              units = "in", res=200)
age_iss %>% 
  tidytable::summarise(p_base = mean(p_base),
                       prop_samp = mean(prop_samp),
                       .by = c(comp_type, surv_labs, species_name, species_type, sub_samp)) %>% 
  ggplot(aes(prop_samp, p_base, shape = species_name, color = comp_type)) +
  geom_point() +
  scale_shape_manual(name = "Species", values=seq(0,14)) +
  facet_grid(species_type~surv_labs) +
  geom_abline(slope = 1, intercept = 0, lty = 3) +
  expand_limits(x=c(0.25,1), y = c(0.25,1)) + 
  scico::scale_color_scico_d(name = "Composition type", 
                             palette = 'roma') +
  xlab("\nProportion of age samples") +
  ylab("Proportion of age compostion input sample size\n")

dev.off()


# supplemental figs ----

# all regions/species length iss
png(filename=here::here("figs", "supp_mat_figs", "alt_props_iss_length.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

len_iss %>% 
  tidytable::filter(!(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                         "Pacific cod", "Pacific ocean perch"))) %>% 
  ggplot(aes(sub_samp, p_base, fill = comp_type)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar=0, 
                width = 0.9) +
  facet_grid(species_name ~ surv_labs,
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle=0)) +
  scico::scale_fill_scico_d("Composition type", 
                            palette='roma', alpha=0.5) +
  xlab("\nSexed lengths sample size") +
  ylab("Proportion of full dataset input sample size by haul\n") +
  theme(legend.position = 'bottom')

dev.off()

# all regions/species age iss
png(filename=here::here("figs", "supp_mat_figs", "alt_props_iss_age.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

age_iss %>% 
  tidytable::filter(!(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                         "Pacific cod", "Pacific ocean perch"))) %>% 
  ggplot(aes(sub_samp, p_base, fill = comp_type)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9) +
  facet_grid(species_name ~ surv_labs,
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scico::scale_fill_scico_d("Composition type", 
                            palette='roma', alpha=0.5) +
  xlab("Age sub-sampling level (%)") +
  ylab("Proportion of full dataset input sample size\n") +
  theme(legend.position = 'bottom') +
  scale_y_continuous(breaks = seq(0.25, 1, 0.25))

dev.off()


# all regions/species length ess
png(filename=here::here("figs", "supp_mat_figs", "alt_props_ess_length.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

len_ess %>% 
  tidytable::filter(!(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                       "Pacific cod", "Pacific ocean perch"))) %>% 
  ggplot(aes(sub_samp, p_base)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar=0, fill = "lightgray") +
  facet_grid(species_name ~ surv_labs,
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("\nSexed lengths sample size") +
  ylab("Proportion of full dataset effective sample size\n")

dev.off()

# all regions/species age ess
png(filename=here::here("figs", "supp_mat_figs", "alt_props_ess_age.png"), 
    width = 6.5, height = 8,
    units = "in", res=200)

age_ess %>% 
  tidytable::filter(!(species_name %in%c("Walleye pollock", "Arrowtooth flounder", 
                                       "Pacific cod", "Pacific ocean perch"))) %>% 
  ggplot(aes(sub_samp, p_base)) +
  geom_hline(yintercept = 0.9, lty = 3) +
  geom_boxplot2(width.errorbar = 0, fill = "lightgray") +
  facet_grid(species_name ~ surv_labs,
             labeller = label_wrap_gen(10)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  xlab("Age sub-sampling level (%)") +
  ylab("Proportion of full dataset effective sample size\n")
dev.off()
