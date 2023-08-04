
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
      ylab("Age composition ISS\n") +
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
      ylab("Length composition ISS\n")  +
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
      ylab("Relative age composition ISS\n") +
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
      ylab("Relative length composition ISS\n") +
      scale_fill_scico_d(palette = 'roma',
                         name = "Composition type") +
      geom_abline(slope = 0, intercept = 0.9, colour = "grey")

  }
}


# main manuscript plots ----

# plot length and age iss across length subsampling cases

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
  ylab("Length composition ISS\n") +
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
  ylab("Age composition ISS\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> p2

ggarrange(p1,
          p2,
          ncol= 1) -> plt

ggsave(here::here("figs", "lenage_iss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)


# plot length and age relative iss across length subsampling cases

len_iss %>%  
  tidytable::filter(species_type != 'other') %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = p_base, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(~ species_type) +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  xlab("\nHaul length frequency sample size") +
  ylab("Relative length composition ISS\n") +
  ylim(0.5, 1) +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") -> p1


len_iss %>%
  tidytable::filter(species_type != 'other') %>% 
  tidytable::select(year, species_code, species_type, comp_type, iss_age, sub_samp, region) %>% 
  tidytable::left_join(len_iss %>% 
                         tidytable::mutate(sub_iss_length = base_iss_length) %>% 
                         tidytable::summarise(sub_iss_length = mean(sub_iss_length), 
                                              iss_age = mean(iss_age),
                                              .by = c(year, species_code, comp_type, species_name, surv_labs, species_type, region)) %>% 
                         tidytable::mutate(base_iss_age = iss_age,
                                           sub_samp = 'Full',
                                           p_base = 1,
                                           prop_samp = 1) %>% 
                         tidytable::select(year, species_code, species_type, comp_type, base_iss_age, region)) %>% 
  tidytable::mutate(p_base = iss_age / base_iss_age) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = p_base, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(~ species_type) + 
  theme(text = element_text(size = 14),
        strip.text.x = element_blank(),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Relative age composition ISS\n") +
  ylim(0.5, 1.05) +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 1, colour = "grey") -> p2

ggarrange(p1,
          p2,
          ncol= 1) -> plt

ggsave(here::here("figs", "lenage_reliss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)

# plot bin results of length relative iss across length subsampling cases

len_iss %>% 
  tidytable::filter(region == 'goa') %>% 
  tidytable::mutate(bin = '1cm') %>% 
  tidytable::bind_rows(vroom::vroom(here::here('output', 'binres_iss.csv')) %>% 
                         tidytable::mutate(p_base = sub_iss_length / base_iss_length,
                                           sub_samp = gsub("t", "", sub_samp),
                                           sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250')),
                                           surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                                                 region == 'ai' ~ "Aleutian Islands",
                                                                 region == 'bs' ~ "Eastern Bering Sea Shelf")) %>% 
                         tidytable::left_join(spec)) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = p_base, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(species_type ~ bin) + 
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Relative length composition ISS\n") +
  ylim(0.5, 1.05) +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") -> plt

ggsave(here::here("figs", "lenbin_reliss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)

# compare sub-region length relative iss
len_iss %>% 
  tidytable::filter(region == 'goa') %>% 
  tidytable::select(-surv_labs) %>% 
  tidytable::bind_rows(vroom::vroom(here::here('output', 'subreg_iss.csv')) %>% 
                         tidytable::mutate(p_base = sub_iss_length / base_iss_length,
                                           sub_samp = gsub("t", "", sub_samp),
                                           sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250'))) %>%
                         tidytable::left_join(spec)) %>% 
  tidytable::mutate(surv_labs = case_when(region == 'goa' ~ "GOA",
                                          region == 'egoa' ~ "Eastern GOA",
                                          region == 'cgoa' ~ "Central GOA",
                                          region == 'wgoa' ~ "Western GOA")) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = p_base, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(factor(surv_labs, levels = c('GOA', 'Western GOA', 'Central GOA', 'Eastern GOA')) ~ species_type) + 
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Relative length composition ISS\n") +
  ylim(0.5, 1.05) +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") -> plt

ggsave(here::here("figs", "subreg_len_reliss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)

# plot age iss and relative iss subsample for type

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
  facet_grid( ~ species_type,
        labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Age composition ISS\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> p1

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
                y = p_base, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid( ~ species_type,
              labeller = label_wrap_gen(10)) +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        strip.text.x = element_blank()) +
  xlab("\nAge sub-sampling level (%)") +
  ylab("Relative age composition ISS\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") -> p2

ggarrange(p1,
          p2,
          ncol= 1) -> plt

ggsave(here::here("figs", "age_reliss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)

# plot relationship between decrease in iss and nss

age_iss %>% 
  tidytable::summarise(p_base = mean(p_base), 
                       .by = c(species_code, comp_type, sub_samp, prop_samp, species_name, species_type)) %>% 
  ggplot(.,aes(x = prop_samp, y = p_base, pch = as.factor(species_name), color = as.factor(comp_type))) +
  geom_point() +
  scale_shape_manual(values=seq(0,14)) +
  facet_grid( ~ species_type,
             labeller = label_wrap_gen(10)) +
  geom_abline(slope = 1, intercept = 0, colour = "black", linetype = 2) +
  xlab("\nAge sub-sampling level") +
  ylab("Relative age composition ISS\n") +
  labs(pch = "Stock") +
  scale_color_scico_d(palette = 'roma',
                      name = "Composition type") + 
  geom_smooth(aes(group = comp_type), method = 'lm', se = F) +
  theme(text = element_text(size = 14),
        strip.text.y.right = element_text(angle = 0)) -> plt

ggsave(here::here("figs", "age_reliss_ss.png"),
       plt,
       device = "png",
       width = 7,
       height = 4)



# supp material ----

# plot iss vs iteration

vroom::vroom(here::here('output', 'iss_vs_iter.csv')) %>% 
  tidytable::pivot_longer(cols = c(ess_f, ess_m, ess_t), values_to = 'iss') %>% 
  tidytable::mutate(name = case_when(name == 'ess_f' ~ 'Female',
                                     name == 'ess_m' ~ 'Male',
                                     name == 'ess_t' ~ 'Total')) %>% 
  ggplot(.,aes(x = iteration, y = iss, color = as.factor(name))) +
  geom_point() +
  scale_shape_manual(values=seq(0,14)) +
  geom_vline(xintercept = 500, linetype="solid", color = "grey") +
  xlab("\nIteration") +
  ylab("Length composition ISS\n") +
  scale_color_scico_d(palette = 'roma',
                      name = "Composition type") + 
  theme(text = element_text(size = 14),
        strip.text.y.right = element_text(angle = 0)) -> plt
  
ggsave(here::here("figs", "supp_mat_figs", "iss_vs_iter.png"),
       plt,
       device = "png",
       width = 7,
       height = 4)

# plot regional length iss across subsampling cases

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

# plot length subsample proportion of base iss

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

# plot bin results of length relative iss across length subsampling cases

len_iss %>% 
  tidytable::filter(region == 'goa') %>% 
  tidytable::mutate(bin = '1cm') %>% 
  tidytable::bind_rows(vroom::vroom(here::here('output', 'binres_iss.csv')) %>% 
                         tidytable::mutate(p_base = sub_iss_length / base_iss_length,
                                           sub_samp = gsub("t", "", sub_samp),
                                           sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250')),
                                           surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                                                 region == 'ai' ~ "Aleutian Islands",
                                                                 region == 'bs' ~ "Eastern Bering Sea Shelf")) %>% 
                         tidytable::left_join(spec)) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = sub_iss_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(species_type ~ bin, 
             scales = "free") + 
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Length composition ISS\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> plt

ggsave(here::here("figs", "supp_mat_figs", "bin_len_iss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)

# plot bin results of age relative iss across length subsampling cases

len_iss %>% 
  tidytable::filter(region == 'goa') %>% 
  tidytable::mutate(bin = '1cm') %>% 
  tidytable::bind_rows(vroom::vroom(here::here('output', 'binres_iss.csv')) %>% 
                         tidytable::mutate(p_base = sub_iss_length / base_iss_length,
                                           sub_samp = gsub("t", "", sub_samp),
                                           sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250')),
                                           surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                                                 region == 'ai' ~ "Aleutian Islands",
                                                                 region == 'bs' ~ "Eastern Bering Sea Shelf")) %>% 
                         tidytable::left_join(spec)) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(species_type ~ bin, 
             scales = "free") + 
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Age composition ISS\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> plt

ggsave(here::here("figs", "supp_mat_figs", "bin_age_iss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)

# compare sub-region length iss
len_iss %>% 
  tidytable::filter(region == 'goa') %>% 
  tidytable::select(-surv_labs) %>% 
  tidytable::bind_rows(vroom::vroom(here::here('output', 'subreg_iss.csv')) %>% 
                         tidytable::mutate(p_base = sub_iss_length / base_iss_length,
                                           sub_samp = gsub("t", "", sub_samp),
                                           sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250'))) %>%
                         tidytable::left_join(spec)) %>% 
  tidytable::mutate(surv_labs = case_when(region == 'goa' ~ "GOA",
                                          region == 'egoa' ~ "Eastern GOA",
                                          region == 'cgoa' ~ "Central GOA",
                                          region == 'wgoa' ~ "Western GOA")) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = sub_iss_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(species_type ~ factor(surv_labs, levels = c('GOA', 'Western GOA', 'Central GOA', 'Eastern GOA')), 
             scales = "free") + 
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Length composition ISS\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> plt

ggsave(here::here("figs", "supp_mat_figs", "subreg_len_iss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)

# compare sub-region age iss
len_iss %>% 
  tidytable::filter(region == 'goa') %>% 
  tidytable::select(-surv_labs) %>% 
  tidytable::bind_rows(vroom::vroom(here::here('output', 'subreg_iss.csv')) %>% 
                         tidytable::mutate(p_base = sub_iss_length / base_iss_length,
                                           sub_samp = gsub("t", "", sub_samp),
                                           sub_samp = factor(sub_samp, level = c('50', '100', '150', '200', '250'))) %>%
                         tidytable::left_join(spec)) %>% 
  tidytable::mutate(surv_labs = case_when(region == 'goa' ~ "GOA",
                                          region == 'egoa' ~ "Eastern GOA",
                                          region == 'cgoa' ~ "Central GOA",
                                          region == 'wgoa' ~ "Western GOA")) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(species_type ~ factor(surv_labs, levels = c('GOA', 'Western GOA', 'Central GOA', 'Eastern GOA')), 
             scales = "free") + 
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Age composition ISS\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> plt

ggsave(here::here("figs", "supp_mat_figs", "subreg_age_iss.png"),
       plt,
       device = "png",
       width = 7,
       height = 7)

# plot age iss by subsample rate

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

# plot age relative iss by subsample rate

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


# plot relative bias in mean length and sex ratio

vroom::vroom(here::here('output', 'meanlen_relb.csv')) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', 'sexr_relb.csv'))) %>% 
  tidytable::mutate(sub_samp = gsub("t", "", sub_samp),
                    sub_samp = factor(sub_samp, level = c('100', '150'))) %>% 
  tidytable::left_join(spec) %>% 
  tidytable::pivot_longer(cols = c(relb_males, relb_females, relb_sexr), values_to = 'relb') %>% 
  tidytable::mutate(name = case_when(name == 'relb_males' ~ "Male mean length",
                                     name == 'relb_females' ~ "Female mean length",
                                     name == 'relb_sexr' ~ "Sex Ratio")) %>% 

  ggplot(., aes(x = factor(sub_samp, level = c('100', '150')), 
                y = relb, 
                fill = name)) +
  geom_boxplot2(width.errorbar = 0, 
                width = 0.9,
                position = position_dodge(preserve = "single"),
                alpha = 0.5) +
  facet_grid(~ species_type, 
             scales = "free") + 
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  xlab("\nHaul length frequency sample size") +
  ylab("Relative bias\n") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Metric") +
  geom_abline(slope = 0, intercept = 0, colour = "black") -> plt

ggsave(here::here("figs", "supp_mat_figs", "rel_bias.png"),
       plt,
       device = "png",
       width = 7,
       height = 4)

ggsave(here::here("figs", "rel_bias.png"),
       plt,
       device = "png",
       width = 7,
       height = 4)




