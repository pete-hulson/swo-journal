
# load ----
library(tidyverse)
library(tidytable)
library(vroom)
library(here)
library(purrr)
library(rsample)
library(data.table)
library(scico)
library(extrafont)
library(cowplot)
library(egg)
library(gg.layers)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()
loadfonts(device="win")   

# theme_set(afscassess::theme_report())





# if you get a lot of messages like 'C:\Windows\Fonts\ALGER.TTF : No FontName. Skipping.'
# then load this package and then run font_import
# remotes::install_version("Rttf2pt1", version = "1.3.8")

# add fonts to all text (last line)
ggplot2::theme_set(
  ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # axis.ticks.length = grid::unit(base_ / 2.2, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      legend.background = ggplot2::element_rect(colour = NA, fill = NA),
      text = element_text(family = "Times New Roman")
    )
)

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
                    surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "Eastern Bering Sea Shelf")) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::drop_na()

len_iss <- vroom::vroom(here::here('output', 'totlen_iss.csv')) %>% 
  tidytable::filter(!(species_code %in% c(10112, 10115, 10180))) %>% 
  tidytable::mutate(p_base = sub_iss_length / base_iss_length,
                    sub_samp = gsub("t", "", sub_samp),
                    sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250')),
                    surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "Eastern Bering Sea Shelf")) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::drop_na()

surv_labs <- c("Aleutian Islands", "Bering Sea Shelf", "Gulf of Alaska")
names(surv_labs) <- c("ai", "bs", "goa")

# plot fcns for supplementary material ----

iss_plot <- function(data, type = 'age', reg = 'goa') {
  
  if(type == "age"){
    
    data %>% 
      tidytable::mutate(sub_iss_age = base_iss_age) %>% 
      tidytable::summarise(sub_iss_age = mean(sub_iss_age), 
                           .by = c(year, species_code, comp_type, species_name, surv_labs, species_type, region)) %>% 
      tidytable::mutate(base_iss_age = sub_iss_age,
                        sub_samp = '100',
                        p_base = 1,
                        prop_samp = 1) %>% 
      tidytable::bind_rows(data) %>% 
      tidytable::filter(species_type != 'other',
                        region == reg) %>% 
      ggplot(., aes(x = factor(sub_samp, level = c('25', '50', '75', '90', '100')), 
                    y = sub_iss_age, 
                    fill = comp_type)) +
      geom_boxplot2(width.errorbar = 0, 
                    width = 0.9,
                    position = position_dodge(preserve = "single"),
                    alpha = 0.5) +
      facet_grid(species_name ~ comp_type, 
                 scales = "free",
                 labeller = label_wrap_gen(10)) +
      theme(legend.position = "none",
            text = element_text(size = 13),
            strip.text.y.right = element_text(angle = 0)) +
      xlab("\nAge sub-sampling level (%)") +
      ylab("Age composition input sample size\n") +
      scale_fill_scico_d(palette = 'roma',
                         name = "Composition type")
    
  } else {
    
    data %>% 
      tidytable::mutate(sub_iss_length = base_iss_length) %>% 
      tidytable::summarise(sub_iss_length = mean(sub_iss_length), 
                           iss_age = mean(iss_age),
                           .by = c(year, species_code, comp_type, species_name, surv_labs, species_type, region)) %>% 
      tidytable::mutate(base_iss_length = sub_iss_length,
                        sub_samp = 'Full',
                        p_base = 1,
                        prop_samp = 1) %>% 
      tidytable::bind_rows(data) %>% 
      tidytable::filter(region == reg) %>% 
      ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'Full')), 
                    y = sub_iss_length, 
                    fill = comp_type)) +
      geom_boxplot2(width.errorbar = 0, 
                    width = 0.9,
                    position = position_dodge(preserve = "single"),
                    alpha = 0.5) +
      facet_grid(species_name ~ comp_type, 
                 scales = "free",
                 labeller = label_wrap_gen(10)) +
      theme(legend.position = "none",
            text = element_text(size = 13),
            strip.text.y.right = element_text(angle = 0)) +
      xlab("\nHaul length frequency sample size") +
      ylab("Length composition input sample size\n")  +
      scale_fill_scico_d(palette = 'roma',
                         name = "Composition type")
  }
}

prop_iss_plot <- function(data, type = 'age', reg = 'goa') {
  
  if(type == "age"){
    
    data %>% 
      tidytable::filter(species_type != 'other',
                        region == reg) %>% 
      ggplot(., aes(x = factor(sub_samp, level = c('25', '50', '75', '90')), 
                    y = p_base, 
                    fill = comp_type)) +
      geom_boxplot2(width.errorbar = 0, 
                    width = 0.9,
                    position = position_dodge(preserve = "single"),
                    alpha = 0.5) +
      facet_grid(species_name ~ comp_type,
                 labeller = label_wrap_gen(10)) +
      theme(legend.position = "bottom",
            text = element_text(size = 14),
            strip.text.y.right = element_text(angle = 0)) +
      xlab("\nAge sub-sampling level (%)") +
      ylab("Relative age composition input sample size\n") +
      scale_fill_scico_d(palette = 'roma',
                         name = "Composition type") +
      geom_abline(slope = 0, intercept = 0.9, colour = "grey")
    
  } else {
    
    data %>%  
      tidytable::filter(region == reg) %>% 
      ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                    y = p_base, 
                    fill = comp_type)) +
      geom_boxplot2(width.errorbar = 0, 
                    width = 0.9,
                    position = position_dodge(preserve = "single"),
                    alpha = 0.5) +
      facet_grid(species_name ~ comp_type,
                 labeller = label_wrap_gen(10)) +
      theme(legend.position = "none",
            text = element_text(size = 13),
            strip.text.y.right = element_text(angle = 0)) +
      xlab("\nHaul length frequency sample size") +
      ylab("Relative length composition input sample size\n") +
      scale_fill_scico_d(palette = 'roma',
                         name = "Composition type") +
      geom_abline(slope = 0, intercept = 0.9, colour = "grey")

  }
}



# plot length iss for type & sex ----

len_iss %>% 
  tidytable::mutate(sub_iss_length = base_iss_length) %>% 
  tidytable::summarise(sub_iss_length = mean(sub_iss_length), 
                       iss_age = mean(iss_age),
                       .by = c(year, species_code, comp_type, species_name, surv_labs, species_type, region)) %>% 
  tidytable::mutate(base_iss_length = sub_iss_length,
                    sub_samp = 'Full',
                    p_base = 1,
                    prop_samp = 1) %>% 
  tidytable::bind_rows(len_iss) %>% 
  tidytable::filter(species_type != 'other') %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'Full')), 
                y = sub_iss_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(~ species_type, 
             scales = "free") + 
  theme(text = element_text(size = 14),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank()) +
  xlab("\nHaul length frequency sample size") +
  ylab("Length input sample size\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> p1

len_iss %>% 
  tidytable::mutate(sub_iss_length = base_iss_length) %>% 
  tidytable::summarise(sub_iss_length = mean(sub_iss_length), 
                       iss_age = mean(iss_age),
                       .by = c(year, species_code, comp_type, species_name, surv_labs, species_type, region)) %>% 
  tidytable::mutate(base_iss_length = sub_iss_length,
                    sub_samp = 'Full',
                    p_base = 1,
                    prop_samp = 1) %>%  
  tidytable::bind_rows(len_iss) %>% 
  tidytable::filter(species_type != 'other') %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'Full')), 
                y = iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(~ species_type, 
             scales = "free") + 
  theme(text = element_text(size = 14),
        strip.text.x = element_blank(),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Age input sample size\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> p2

ggarrange(p1,
          p2,
          ncol= 1) -> iss_length

ggsave(here::here("figs", "length_iss.png"),
       iss_length,
       device = "png",
       width = 7,
       height = 7)

# plot regional length iss (supplementary material) ----

png(filename=here::here("figs", "supp_mat_figs", "length_iss_goa.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

iss_plot(len_iss, type = 'length', reg = 'goa')

dev.off()

png(filename=here::here("figs", "supp_mat_figs", "length_iss_ai.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

iss_plot(len_iss, type='length', reg = 'ai')

dev.off()

png(filename=here::here("figs", "supp_mat_figs", "length_iss_bs.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

iss_plot(len_iss, type='length', reg = 'bs')

dev.off()

# plot length subsample proportion of base by survey and species type ----

len_iss %>%  
  tidytable::filter(species_type != 'other') %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = p_base, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(surv_labs ~ species_type,
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        strip.text.y.right = element_text(angle = 0)) +
  xlab("\nHaul length frequency sample size") +
  ylab("Relative length composition input sample size\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") -> prop_iss_length

ggsave(here::here("figs", "length_iss_prop.png"),
       prop_iss_length,
       device = "png",
       width = 7,
       height = 7)

# plot length subsample proportion of base iss (supplementary material) ----

png(filename=here::here("figs", "supp_mat_figs", "rel_length_iss_goa.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

prop_iss_plot(len_iss, type = 'length', reg = 'goa')

dev.off()

png(filename=here::here("figs", "supp_mat_figs", "rel_length_iss_ai.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

prop_iss_plot(len_iss, type = 'length', reg = 'ai')

dev.off()

png(filename=here::here("figs", "supp_mat_figs", "rel_length_iss_bs.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

prop_iss_plot(len_iss, type = 'length', reg = 'bs')

dev.off()

# plot age iss subsample for type and region ----

age_iss %>% 
  tidytable::mutate(sub_iss_age = base_iss_age) %>% 
  tidytable::summarise(sub_iss_age = mean(sub_iss_age), 
                       .by = c(year, species_code, comp_type, species_name, surv_labs, species_type, region)) %>% 
  tidytable::mutate(base_iss_age = sub_iss_age,
                    sub_samp = '100',
                    p_base = 1,
                    prop_samp = 1) %>% 
  tidytable::bind_rows(age_iss) %>% 
  tidytable::filter(species_type != 'other') %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25', '50', '75', '90', '100')), 
                y = sub_iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(surv_labs ~ species_type,
        labeller = label_wrap_gen(10)) +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Age composition input sample size\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> iss_age

ggsave(here::here("figs", "age_iss.png"),
       iss_age,
       device = "png",
       width = 7,
       height = 7)

# plot age iss subsample (for goa)supplementary material ----

png(filename=here::here("figs", "supp_mat_figs", "age_iss_goa.png"), 
    width = 7, height = 8.0,
    units = "in", res=200)

iss_plot(age_iss, type = 'age', reg = 'goa')

dev.off()

png(filename=here::here("figs", "supp_mat_figs", "age_iss_ai.png"), 
    width = 7, height = 8.0,
    units = "in", res=200)

iss_plot(age_iss, type = 'age', reg = 'ai')

dev.off()

png(filename=here::here("figs", "supp_mat_figs", "age_iss_bs.png"), 
    width = 7, height = 8.0,
    units = "in", res=200)

iss_plot(age_iss, type = 'age', reg = 'bs')

dev.off()




# plot age subsample proportion ----

age_iss %>% 
  tidytable::filter(species_type != 'other') %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25', '50', '75', '90')), 
                y = p_base, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(surv_labs ~ species_type,
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        strip.text.y.right = element_text(angle = 0)) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Relative age composition input sample size\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") -> prop_iss_age

ggsave(here::here("figs", "age_iss_prop.png"),
       prop_iss_age,
       device = "png",
       width = 7,
       height = 7)

# plot age subsample proportion (supplementary material) ----

png(filename=here::here("figs", "supp_mat_figs", "rel_age_iss_goa.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

prop_iss_plot(age_iss, type = 'age', reg = 'goa')

dev.off()

png(filename=here::here("figs", "supp_mat_figs", "rel_age_iss_ai.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

prop_iss_plot(age_iss, type = 'age', reg = 'ai')

dev.off()

png(filename=here::here("figs", "supp_mat_figs", "rel_age_iss_bs.png"), 
    width = 7, height = 8.0,
    units = "in", res = 200)

prop_iss_plot(age_iss, type = 'age', reg = 'bs')

dev.off()




# plot relationship between decrease in iss and nss ----

age_iss %>% 
  ggplot(.,aes(x = prop_samp, y = p_base, pch = as.factor(species_name), color = as.factor(comp_type))) +
  geom_point() +
  scale_shape_manual(values=seq(0,14)) +
  facet_grid(surv_labs ~ species_type,
             labeller = label_wrap_gen(10)) +
  geom_abline(slope = 1, intercept = 0, colour = "black", linetype = 2) +
  xlab("\nAge sub-sampling level") +
  ylab("Relative age composition input sample size\n") +
  labs(pch = "Stock") +
  scale_color_scico_d(palette = 'roma',
                      name = "Composition type") + 
  geom_smooth(aes(group = comp_type), method = 'lm', se = F) +
  theme(text = element_text(size = 14),
        strip.text.y.right = element_text(angle = 0)) -> age_ss_iss

ggsave(here::here("figs", "age_ss_iss.png"),
       age_ss_iss,
       device = "png",
       width = 7,
       height = 7)





