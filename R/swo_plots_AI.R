# swo basic figures 
# ben.williams@noaa.gov
# 2022-05

# load ----
library(tidyverse)
library(tidytable)
library(scico)
library(extrafont)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()
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


# globals ----
region = 'ai'

# data ----
spec <- vroom::vroom(here::here('data', 'species_ai.csv'))
s50 <- vroom::vroom(here::here('output', region, 's50_ess_sz.csv')) %>% 
  mutate.(id = 50) 
s75 <- vroom::vroom(here::here('output', region, 's75_ess_sz.csv')) %>% 
  mutate.(id = 75)
s100 <- vroom::vroom(here::here('output', region, 's100_ess_sz.csv')) %>% 
  mutate.(id = 100) 
s125 <- vroom::vroom(here::here('output', region, 's125_ess_sz.csv')) %>% 
  mutate.(id = 125) 
s150 <- vroom::vroom(here::here('output', region, 's150_ess_sz.csv')) %>% 
  mutate.(id = 150)
s175 <- vroom::vroom(here::here('output', region, 's175_ess_sz.csv')) %>% 
  mutate.(id = 175)
base <- vroom::vroom(here::here('output', region, 'base_ess_sz.csv')) %>% 
  mutate.(id = 'base')


# length data 
bind_rows.(s50, s75, s100, s125, s150, s175, base) %>% 
  left_join.(spec) %>% 
    mutate.(id = factor(id, c('50', '75', '100', '125', '150', '175', 'base'))) -> df


# plots ----


# ess boxplot ----
df %>% 
  tidytable::mutate.(ess = replace(ess, ess == "ess_f", "Female"),
                     ess = replace(ess, ess == "ess_m", "Male"),
                     ess = replace(ess, ess == "ess_t", "Total")) %>% 
  ggplot(aes(id, value, fill = factor(ess))) + 
    geom_boxplot(outlier.size = 0, alpha = 0.7, outlier.shape = NA) +
    coord_cartesian(ylim = c(0, 3000)) +
    facet_wrap(~species, scales = 'free') + 
    scico::scale_fill_scico_d("Sex", palette = 'roma') +
    ylab('Length composition effective sample size\n') +
    xlab("\nSubsample size case")

# example to do it by sex for boxplots 
# df %>% 
#   tidytable::filter.(ess == 'ess_m') %>% 
#   ggplot(aes(id, value, fill = factor(year))) + 
#   geom_boxplot(outlier.size = 0, alpha = 0.7) +
#   coord_cartesian(ylim = c(0, 3000)) +
#   facet_wrap(~species, scales = 'free') + 
#   scico::scale_fill_scico_d("Year", palette = 'roma') +
#   ylab('Effective sample size (male length)\n') +
#   xlab("\nSexed sample size")

# harmonic mean barplot ----
df %>%
  tidytable::mutate.(HM = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
                     .by = c(year, species_code, ess, id, species)) %>%
  tidytable::mutate.(ess = replace(ess, ess == "ess_f", "Female"),
          ess = replace(ess, ess == "ess_m", "Male"),
          ess = replace(ess, ess == "ess_t", "Total")) %>% 
  group_by(year, species_code, ess, id, species, HM) %>%
  dplyr::distinct(HM) %>% 
  ggplot(aes(id, HM, fill = factor(ess))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(year~species, scales = "free_y", labeller = label_wrap_gen(width = 2, multi_line = TRUE))  +
  scico::scale_fill_scico_d("Year", palette = 'roma') +
  ylab("\nLength composition input sample size") +
  xlab('Subsample size case\n') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
# example to do it by sex for harmonic mean
# df %>%
#   tidytable::mutate.(HM = length(unique(sim))/sum(value^(-1),na.rm=TRUE), .by = c(year, species_code, ess, id, species)) %>%
#   group_by(year, species_code, ess, id, species, HM) %>%
#   dplyr::distinct(HM) %>% 
#   tidytable::filter.(ess == 'ess_m') %>%
#   ggplot(aes(id, HM, fill = factor(year))) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   facet_grid(year~species, scales = "free_y", labeller = label_wrap_gen(width = 2, multi_line = TRUE))  +
#   scico::scale_fill_scico_d("Year", palette = 'roma') +
#   ylab("\nInput sample size (male length)") +
#   xlab('Sexed sample size\n') +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# harmonic mean barplot point-lineplots ----
df %>%
  tidytable::mutate.(ess = replace(ess, ess == "ess_f", "Female"),
                     ess = replace(ess, ess == "ess_m", "Male"),
                     ess = replace(ess, ess == "ess_t", "Total")) %>% 
  dplyr::rename('Sex' = ess) %>% 
  tidytable::mutate.(HM = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
                     .by = c(year, species_code, Sex, id, species)) %>%
  group_by(year, species_code, Sex, id, species, HM) %>%
  dplyr::distinct(HM) -> .df

.df %>% 
  tidytable::filter.(id == 'base') %>% 
  tidytable::mutate.(HM_base = HM) %>% 
  tidytable::select.(-id, -HM)-> .df_base

.df %>% 
  tidytable::filter.(id != 'base') %>% 
  tidytable::left_join.(.df_base) %>% 
  tidytable::mutate.(p_redux = 1-(HM_base - HM)/HM_base, .by = c(year, species_code)) %>% 
  tidytable::mutate.(p_redux = ifelse(p_redux > 1, 1, p_redux), .by = c(year, species_code)) %>% 
  ggplot(aes(id, p_redux, group = Sex)) +
  geom_point(aes(color = Sex, shape = Sex), size = 3) +
  geom_line(aes(color = Sex), linetype = "dotted", size = 0.777) +
  ylim(0, 1) +
  facet_grid(year~species, scales = "free_y", labeller = label_wrap_gen(width = 2, multi_line = TRUE)) +
  scico::scale_color_scico_d(palette = 'roma') +
  ylab("Proportion of base length composition\n input sample size for length") +
  xlab("\nSubsample size case") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


