
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

# pull in output ----

spec <- vroom::vroom(here::here('data', 'species_code_name.csv')) # species_code and common names

agesub_iss <- vroom::vroom(here::here('output', 'agesub_iss.csv')) %>% 
  tidytable::filter(!(species_code %in% c(10112, 10115, 10180)))
agesub_ess <- vroom::vroom(here::here('output', 'big_output', 'agesub_ess.csv')) %>% 
  tidytable::filter(!(species_code %in% c(10112, 10115, 10180)))
lensub_iss <- vroom::vroom(here::here('output', 'totlen_iss.csv')) %>% 
  tidytable::filter(!(species_code %in% c(10112, 10115, 10180)))
lensub_ess <- vroom::vroom(here::here('output', 'big_output', 'totlen_ess.csv')) %>% 
  tidytable::filter(!(species_code %in% c(10112, 10115, 10180)))

surv_labs <- c("Aleutian Islands", "Bering Sea Shelf", "Gulf of Alaska")
names(surv_labs) <- c("ai", "bs", "goa")

# plot length ess subsample example ----

# goa
lensub_ess %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'goa' & year == 2019) %>% 
  tidytable::mutate(p_base_len = sub_ess_length / base_ess_length,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250'),
                    sub_samp = factor(sub_samp)) -> plot_dat1_goa

plot_dat1_goa %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = sub_ess_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Length composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> goa_ess_length

ggsave(here::here("figs", "goa_length_ess_examp.png"),
       goa_ess_length,
       device = "png",
       width = 6,
       height = 7)

# bs
lensub_ess %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'bs' & year == 2021) %>% 
  tidytable::mutate(p_base_len = sub_ess_length / base_ess_length,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250'),
                    sub_samp = factor(sub_samp)) -> plot_dat1_bs

plot_dat1_bs %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = sub_ess_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Length composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> bs_ess_length

ggsave(here::here("figs", "bs_length_ess_examp.png"),
       bs_ess_length,
       device = "png",
       width = 6,
       height = 7)

# ai
lensub_ess %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'ai' & year == 2018) %>% 
  tidytable::mutate(p_base_len = sub_ess_length / base_ess_length,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250'),
                    sub_samp = factor(sub_samp)) -> plot_dat1_ai

plot_dat1_ai %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = sub_ess_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Length composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> ai_ess_length

ggsave(here::here("figs", "ai_length_ess_examp.png"),
       ai_ess_length,
       device = "png",
       width = 6,
       height = 7)

# plot age ess for length subsample example ----

# goa
plot_dat1_goa %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = ess_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Age composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> goa_ess_age

ggsave(here::here("figs", "goa_age_length_ess_examp.png"),
       goa_ess_age,
       device = "png",
       width = 6,
       height = 7)

# bs
plot_dat1_bs %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = ess_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Age composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> bs_ess_age

ggsave(here::here("figs", "bs_age_length_ess_examp.png"),
       bs_ess_age,
       device = "png",
       width = 6,
       height = 7)

# ai
plot_dat1_ai %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = ess_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Age composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> ai_ess_age

ggsave(here::here("figs", "ai_age_length_ess_examp.png"),
       ai_ess_age,
       device = "png",
       width = 6,
       height = 7)


# plot length iss subsample example ----

# goa
lensub_iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'goa') %>% 
  tidytable::mutate(p_base_len = sub_iss_length / base_iss_length,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250'),
                    sub_samp = factor(sub_samp)) -> plot_dat1_goa

plot_dat1_goa %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = sub_iss_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Length composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> goa_iss_length

ggsave(here::here("figs", "goa_length_iss_examp.png"),
       goa_iss_length,
       device = "png",
       width = 6,
       height = 7)

# bs
lensub_iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'bs') %>% 
  tidytable::mutate(p_base_len = sub_iss_length / base_iss_length,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250'),
                    sub_samp = factor(sub_samp)) -> plot_dat1_bs

plot_dat1_bs %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = sub_iss_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             # scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Length composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> bs_iss_length

ggsave(here::here("figs", "bs_length_iss_examp.png"),
       bs_iss_length,
       device = "png",
       width = 6,
       height = 7)

# ai
lensub_iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'ai') %>% 
  tidytable::mutate(p_base_len = sub_iss_length / base_iss_length,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250'),
                    sub_samp = factor(sub_samp)) -> plot_dat1_ai

plot_dat1_ai %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = sub_iss_length, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Length composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> ai_iss_length

ggsave(here::here("figs", "ai_length_iss_examp.png"),
       ai_iss_length,
       device = "png",
       width = 6,
       height = 7)

# plot age iss for length subsample example ----

# goa
plot_dat1_goa %>% 
  tidytable::drop_na() %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             # scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Age composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> goa_iss_age

ggsave(here::here("figs", "goa_age_length_iss_examp.png"),
       goa_iss_age,
       device = "png",
       width = 6,
       height = 7)

# bs
plot_dat1_bs %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Age composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> bs_iss_age

ggsave(here::here("figs", "bs_age_length_iss_examp.png"),
       bs_iss_age,
       device = "png",
       width = 6,
       height = 7)

# ai
plot_dat1_ai %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             # scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Age composition inputsample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> ai_iss_age

ggsave(here::here("figs", "ai_age_length_iss_examp.png"),
       ai_iss_age,
       device = "png",
       width = 6,
       height = 7)

# plot length subsample proportion of base for all species ----

lensub_iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::mutate(p_base_len = sub_iss_length / base_iss_length,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250'),
                    sub_samp = factor(sub_samp)) %>% 
  tidytable::drop_na()-> plot_dat2

plot_dat2 %>% 
  tidytable::mutate(surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "Bering Sea Shelf")) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250')), 
                y = p_base_len, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_wrap(species_name ~ surv_labs,
             labeller = function (labels) {
               labels <- lapply(labels, as.character)
               list(do.call(paste, c(labels, list(sep = "\n"))))
             }) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = -45, hjust = 0),
        text = element_text(size = 11)) +
  xlab("Haul length frequency sub-sampling level") +
  ylab("Length composition proportion of full dataset input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") -> prop_iss_length

ggsave(here::here("figs", "prop_iss_length.png"),
       prop_iss_length,
       device = "png",
       width = 6,
       height = 6)

# plot age ess subsample example ----

# goa
agesub_ess %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'goa' & year == 2021) %>% 
  tidytable::mutate(p_base_len = sub_ess_age / base_ess_age,
                    sub_samp = case_when(sub_samp == 'a25' ~ '25%',
                                         sub_samp == 'a50' ~ '50%',
                                         sub_samp == 'a75' ~ '75%',
                                         sub_samp == 'a90' ~ '90%'),
                    sub_samp = factor(sub_samp)) -> plot_dat3_goa


plot_dat3_goa %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25%', '50%', '75%', '90%')), 
                y = sub_ess_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Total age collection sub-sampling level") +
  ylab("Age composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> goa_ess_age

ggsave(here::here("figs", "goa_age_ess_examp.png"),
       goa_ess_age,
       device = "png",
       width = 6,
       height = 7)

# bs
agesub_ess %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'bs' & year == 2021) %>% 
  tidytable::mutate(p_base_len = sub_ess_age / base_ess_age,
                    sub_samp = case_when(sub_samp == 'a25' ~ '25%',
                                         sub_samp == 'a50' ~ '50%',
                                         sub_samp == 'a75' ~ '75%',
                                         sub_samp == 'a90' ~ '90%'),
                    sub_samp = factor(sub_samp)) -> plot_dat3_bs


plot_dat3_bs %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25%', '50%', '75%', '90%')), 
                y = sub_ess_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Total age collection sub-sampling level") +
  ylab("Age composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> bs_ess_age

ggsave(here::here("figs", "bs_age_ess_examp.png"),
       bs_ess_age,
       device = "png",
       width = 6,
       height = 7)

# ai
agesub_ess %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'ai' & year == 2018) %>% 
  tidytable::mutate(p_base_len = sub_ess_age / base_ess_age,
                    sub_samp = case_when(sub_samp == 'a25' ~ '25%',
                                         sub_samp == 'a50' ~ '50%',
                                         sub_samp == 'a75' ~ '75%',
                                         sub_samp == 'a90' ~ '90%'),
                    sub_samp = factor(sub_samp)) -> plot_dat3_ai


plot_dat3_ai %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25%', '50%', '75%', '90%')), 
                y = sub_ess_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Total age collection sub-sampling level") +
  ylab("Age composition iterated effective sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> ai_ess_age

ggsave(here::here("figs", "ai_age_ess_examp.png"),
       ai_ess_age,
       device = "png",
       width = 6,
       height = 7)

# plot age iss subsample example ----

# goa
agesub_iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::drop_na() %>% 
  tidytable::filter(region == 'goa') %>% 
  tidytable::mutate(p_base_len = sub_iss_age / base_iss_age,
                    sub_samp = case_when(sub_samp == 'a25' ~ '25%',
                                         sub_samp == 'a50' ~ '50%',
                                         sub_samp == 'a75' ~ '75%',
                                         sub_samp == 'a90' ~ '90%'),
                    sub_samp = factor(sub_samp)) -> plot_dat3_goa


plot_dat3_goa %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25%', '50%', '75%', '90%')), 
                y = sub_iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             # scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Total age collection sub-sampling level") +
  ylab("Age composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> goa_iss_age

ggsave(here::here("figs", "goa_age_iss_examp.png"),
       goa_iss_age,
       device = "png",
       width = 6,
       height = 7)

# bs
agesub_iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'bs') %>% 
  tidytable::mutate(p_base_len = sub_iss_age / base_iss_age,
                    sub_samp = case_when(sub_samp == 'a25' ~ '25%',
                                         sub_samp == 'a50' ~ '50%',
                                         sub_samp == 'a75' ~ '75%',
                                         sub_samp == 'a90' ~ '90%'),
                    sub_samp = factor(sub_samp)) -> plot_dat3_bs


plot_dat3_bs %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25%', '50%', '75%', '90%')), 
                y = sub_iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Total age collection sub-sampling level") +
  ylab("Age composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> bs_iss_age

ggsave(here::here("figs", "bs_age_iss_examp.png"),
       bs_iss_age,
       device = "png",
       width = 6,
       height = 7)

# ai
agesub_iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::filter(region == 'ai') %>% 
  tidytable::mutate(p_base_len = sub_iss_age / base_iss_age,
                    sub_samp = case_when(sub_samp == 'a25' ~ '25%',
                                         sub_samp == 'a50' ~ '50%',
                                         sub_samp == 'a75' ~ '75%',
                                         sub_samp == 'a90' ~ '90%'),
                    sub_samp = factor(sub_samp)) -> plot_dat3_ai


plot_dat3_ai %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25%', '50%', '75%', '90%')), 
                y = sub_iss_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_grid(species_name ~ comp_type, 
             # scales = "free",
             labeller = label_wrap_gen(10)) +
  theme(legend.position = "none",
        text = element_text(size = 13)) +
  xlab("Total age collection sub-sampling level") +
  ylab("Age composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> ai_iss_age

ggsave(here::here("figs", "ai_age_iss_examp.png"),
       ai_iss_age,
       device = "png",
       width = 6,
       height = 7)




# plot age subsample proportion of base for all species ----

agesub_iss %>% 
  tidytable::left_join(spec) %>% 
  tidytable::mutate(p_base_age = sub_iss_age / base_iss_age,
                    sub_samp = case_when(sub_samp == 'a25' ~ '25%',
                                         sub_samp == 'a50' ~ '50%',
                                         sub_samp == 'a75' ~ '75%',
                                         sub_samp == 'a90' ~ '90%'),
                    sub_samp = factor(sub_samp),
                    prop_samp = case_when(sub_samp == '25%' ~ 0.25,
                                          sub_samp == '50%' ~ 0.5,
                                          sub_samp == '75%' ~ 0.75,
                                          sub_samp == '90%' ~ 0.9)) %>% 
  tidytable::drop_na()-> plot_dat4

plot_dat4 %>% 
  tidytable::mutate(surv_labs = case_when(region == 'goa' ~ "Gulf of Alaska",
                                          region == 'ai' ~ "Aleutian Islands",
                                          region == 'bs' ~ "Bering Sea Shelf")) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25%', '50%', '75%', '90%')), 
                y = p_base_age, 
                fill = comp_type)) +
  geom_boxplot2(width.errorbar = 0) +
  facet_wrap(species_name ~ surv_labs,
             # labeller = labeller(region = surv_labs)) +
             labeller = function (labels) {
               labels <- lapply(labels, as.character)
               list(do.call(paste, c(labels, list(sep = "\n"))))
             }) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = -45, hjust = 0),
        text = element_text(size = 11),
        panel.spacing.y = unit(0, "cm")) +
  xlab("Total age collection sub-sampling level") +
  ylab("Age composition proportion of full dataset input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") -> prop_iss_age

ggsave(here::here("figs", "prop_iss_age.png"),
       prop_iss_age,
       device = "png",
       width = 6,
       height = 6)

# plot relationship between decrease in iss and nss ----

plot_dat4 %>% 
  tidytable::summarise(p_base_age = mean(p_base_age),
                       prop_samp = mean(prop_samp),
                       .by = c(species_code, comp_type, region, species_name, species_type, sub_samp)) %>% 
  ggplot(.,aes(x = prop_samp, y = p_base_age, pch = as.factor(species_name), color = as.factor(comp_type))) +
  geom_point() +
  scale_shape_manual(values=seq(0,14)) +
  facet_grid(region ~ species_type,
             labeller = labeller(region = surv_labs)) +
  geom_abline(slope = 1, intercept = 0, colour = "black") +
  xlab("Proportion of total age samples") +
  ylab("Proportion of age composition input sample size") +
  labs(pch = "Stock") +
  scale_color_scico_d(palette = 'roma',
                      name = "Composition type") + 
  theme(text = element_text(size = 14)) -> p_iss_nss

ggsave(here::here("figs", "p_age_iss_nss.png"),
       p_iss_nss,
       device = "png",
       width = 7,
       height = 6)





