
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

agesub <- vroom::vroom(here::here('output', 'agesub_iss.csv')) # age subsample cases

lensub <- vroom::vroom(here::here('output', 'totlen_iss.csv')) # age subsample cases

surv_labs <- c("Aleutian Islands", "Bering Sea Shelf", "Gulf of Alaska")
names(surv_labs) <- c("ai", "bs", "goa")

# plot length subsample example ----

lensub %>% 
  tidytable::filter(sub_samp == 'base') %>% 
  tidytable::rename(iss_age_base = 'iss_age',
                    iss_len_base = 'iss_length') %>% 
  tidytable::select(-sub_samp) -> lenbase

lensub %>% 
  tidytable::left_join.(lenbase) %>% 
  tidytable::mutate(p_base_age = iss_age / iss_age_base,
                    p_base_len = iss_length / iss_len_base,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250',
                                         sub_samp == 'base' ~ sub_samp),
                    sub_samp = factor(sub_samp)) %>% 
  tidytable::select(-iss_age_base, -iss_len_base) -> plot_dat
  
plot_dat %>% 
  tidytable::filter(species_code == 21740) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'base')), 
                y = iss_length, 
                fill = comp_type)) +
  geom_boxplot() +
  facet_grid(region ~ ., 
             scales = "free",
             labeller = labeller(region = surv_labs)) +
  theme(strip.text.y = element_blank(),
        legend.position = "none") +
  xlab("Sub-sampling level") +
  ylab("Length composition input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> p1

plot_dat %>% 
  tidytable::filter(species_code == 21740) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'base')), 
                y = p_base_len, 
                fill = comp_type)) +
  geom_boxplot() +
  facet_grid(region ~ ., 
             scales = "free",
             labeller = labeller(region = surv_labs)) +
  xlab("Sub-sampling level") +
  ylab("Proportion of base input sample size") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type") -> p2

ggarrange(p1,
          p2,
          nrow = 1)

# plot length subsample for all species ----

# plot length iss
lensub %>% 
  tidytable::filter(sub_samp == 'base') %>% 
  tidytable::rename(iss_age_base = 'iss_age',
                    iss_len_base = 'iss_length') %>% 
  tidytable::select(-sub_samp) -> lenbase

lensub %>% 
  tidytable::left_join.(lenbase) %>% 
  tidytable::left_join.(spec) %>% 
  tidytable::mutate(p_base_age = iss_age / iss_age_base,
                    p_base_len = iss_length / iss_len_base,
                    sub_samp = case_when(sub_samp == 't50' ~ '50',
                                         sub_samp == 't100' ~ '100',
                                         sub_samp == 't150' ~ '150',
                                         sub_samp == 't200' ~ '200',
                                         sub_samp == 't250' ~ '250',
                                         sub_samp == 'base' ~ sub_samp),
                    sub_samp = factor(sub_samp)) %>% 
  tidytable::select(-iss_age_base, -iss_len_base) -> plot_dat

# goa
plot_dat %>%  
  tidytable::filter(region == 'goa' & species_code == 30060) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'base')), 
                y = p_base_len, 
                fill = comp_type)) +
  geom_boxplot() +
  facet_wrap( ~ species_name, 
              # scales = "free",
              # labeller = labeller(region = surv_labs),
  ) +
  xlab("Sub-sampling level") +
  ylab("Proportion of base input sample size") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type")

# bs
plot_dat %>%  
  tidytable::filter(region == 'bs' & species_code == 30060) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'base')), 
                y = p_base_len, 
                fill = comp_type)) +
  geom_boxplot() +
  facet_wrap( ~ species_name, 
             # scales = "free",
             # labeller = labeller(region = surv_labs),
             ) +
  xlab("Sub-sampling level") +
  ylab("Proportion of base input sample size") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type")

# ai
plot_dat %>%  
  tidytable::filter(region == 'ai' & species_code == 30060) %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'base')), 
                y = p_base_len, 
                fill = comp_type)) +
  geom_boxplot() +
  facet_wrap( ~ species_name, 
              # scales = "free",
              # labeller = labeller(region = surv_labs),
  ) +
  xlab("Sub-sampling level") +
  ylab("Proportion of base input sample size") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type")

# plot age iss for length subsampling
plot_dat %>%  
  tidytable::filter(region == 'goa') %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('50', '100', '150', '200', '250', 'base')), 
                y = p_base_age, 
                fill = comp_type)) +
  geom_boxplot() +
  facet_wrap( ~ species_name, 
              # scales = "free",
              # labeller = labeller(region = surv_labs),
  ) +
  xlab("Sub-sampling level") +
  ylab("Proportion of base input sample size") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type")


# plot age subsample for all species ----

# plot age iss
agesub %>% 
  tidytable::filter(sub_samp == 'base') %>% 
  tidytable::rename(iss_age_base = 'iss_age') %>% 
  tidytable::select(-sub_samp) -> agebase

agesub %>% 
  tidytable::left_join.(agebase) %>% 
  tidytable::left_join.(spec) %>% 
  tidytable::mutate(p_base_age = iss_age / iss_age_base,
                    sub_samp = case_when(sub_samp == 'a25' ~ '25%',
                                         sub_samp == 'a50' ~ '50%',
                                         sub_samp == 'a75' ~ '75%',
                                         sub_samp == 'a90' ~ '90%',
                                         sub_samp == 'base' ~ '100%'),
                    sub_samp = factor(sub_samp)) %>% 
  tidytable::select(-iss_age_base) -> plot_dat

plot_dat %>%  
  tidytable::filter(region == 'bs') %>% 
  ggplot(., aes(x = factor(sub_samp, level = c('25%', '50%', '75%', '90%', '100%')), 
                y = p_base_age, 
                fill = comp_type)) +
  geom_boxplot() +
  facet_wrap( ~ species_name, 
              # scales = "free",
              # labeller = labeller(region = surv_labs),
  ) +
  xlab("Sub-sampling level") +
  ylab("Proportion of base input sample size") +
  geom_abline(slope = 0, intercept = 0.9, colour = "grey") +
  scale_fill_scico_d(palette = 'roma',
                     name = "Composition type")




