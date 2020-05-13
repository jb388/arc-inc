# Script for EGU presentation plots
# requires arc-inc-data-wrangling .Rmd script

setwd("/Users/jeff/Documents")

# Fig. 1
png(filename = "f1.concept.model.png", 
    width = 6,
    height = 4,
    type = "cairo",
    units = "in",
    res = 200)
ggplot(f.2p.C14.df, aes(years, d14C, color = pool)) +
  geom_path() +
  scale_color_manual(
    name = "Model pool",
    values = c("atm" = 8,
               "fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5"),
    labels = c("atm" = "atmosphere",
               "fast" = "fast",
               "slow" = "slow")) +
  scale_x_continuous(limits = c(1950, 2022)) +
  xlab("Year") +
  ylab(expression(''*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=16))
dev.off()

# Fig 3
png(filename = "f2.concept.model.png", 
    width = 6,
    height = 4,
    type = "cairo",
    units = "in",
    res = 200)
f.2p.C14.df %>% 
  filter(years > 1987) %>%
  filter(pool != "atm") %>%
  ggplot(., aes(years, d14C)) +
  geom_path(aes(color = pool)) +
  geom_point(data = obs.pts, aes(years, d14c, shape = Observation), color = "#FFC107", stroke = 1, size = 3) +
  scale_color_manual(
    name = "Model pool",
    values = c("atm" = 8,
               "fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5"),
    labels = c("atm" = "atmosphere",
               "fast" = "fast",
               "slow" = "slow")) +
  scale_shape_manual(
    name = "Observed respiration",
    values = c("control" = 16,
               "increased slow pool contribution" = 0,
               "increased fast pool contribution" = 1),
    labels = c("control" = "control",
               "increased slow pool contribution" = "\nincreased slow pool\ncontribution\n",
               "increased fast pool contribution" = "\nincreased fast pool\ncontribution\n")) +
  scale_y_continuous(limits = c(0, 265)) +
  xlab("Year") +
  ylab(expression(''*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 14))
dev.off()

# Fig. 4
png(filename = "f4.resp.rates.png", 
    width = 6,
    height = 4,
    type = "cairo",
    units = "in",
    res = 200)
ts.avg2 %>% 
  filter(Experiment != "tme") %>%
  ggplot(., aes(time_d_cmtv_mean, mgCO2.C_gC_d_mean)) +
  geom_vline(xintercept = 4, color="gray") +
  geom_ribbon(aes(ymin = se_slope_l, ymax = se_slope_u, fill = Type, linetype = Treatment, alpha = Treatment)) +
  geom_line(aes(color = Type, linetype = Treatment)) +
  facet_grid(rows = vars(Experiment),
             labeller = labeller(Experiment = c("arc" = "2011", "rewet" = "2019"))) +
  scale_x_continuous(limits = c(0,18)) +
  scale_color_manual(name = 'Land use',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_fill_manual(name = 'Standard error',
                    values =c('F'='#a35513','G'='#1361a3'),
                    labels = c('Forest','Grassland')) +
  scale_alpha_manual(name = 'Treatment',
                     values = c("air-dry + storage" = .2, 
                                "control" = .4,
                                "air-dry" = .2)) +
  scale_linetype_manual(name = 'Treatment',
                        values = c("air-dry + storage" = 'dashed', 
                                   "control" ='solid',
                                   "air-dry" = "dashed")) +
  ylab(expression('Respiration Rate (mgCO'[2]*'-C gC'^-1*'d'^-1*')')) +
  xlab("Time (days)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16))
dev.off()

# Fig. 5 pre vs. equil. 14C
png(filename = "f5.pre.equil.png", 
    width = 6,
    height = 4,
    type = "cairo",
    units = "in",
    res = 200)
all.14c.pre.inc %>%
  filter(Experiment != "tme") %>%
  ggplot(., aes(d14c_corr_mean_pre, d14c_corr_mean_inc, color = Type, shape = Treatment)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = d14c_corr_min_inc, 
        ymax = d14c_corr_max_inc, 
        color = Type), 
    width = .25) +
  geom_errorbarh(
    aes(xmin = d14c_corr_min_pre, 
        xmax = d14c_corr_max_pre, 
        color = Type), 
    height = .9) +
  scale_color_manual(name = 'Land use',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Treatment',
                     values = c("control" = 16,
                                "air-dry" = 1,
                                "air-dry + storage" = 0)) +
  scale_x_continuous(limits = c(-60, 115)) +
  scale_y_continuous(limits = c(-60, 115)) +
  xlab(expression('Pre-incubation '*Delta*''^14*'C (‰)')) +
  ylab(expression('Equilibrium respiration '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.height=unit(.8, "cm"),
        aspect.ratio = 1,
        text = element_text(size = 16))
dev.off()

# Fig. 6 Treatment effects over time
png(filename = "f6.treat.time.png", 
    width = 6,
    height = 4,
    type = "cairo",
    units = "in",
    res = 200)
all.14c.sum2 %>%
  filter(Period == "inc") %>%
  mutate(year.Type = paste(YearSampled, Type)) %>%
  mutate(type.long = ifelse(Type == "F", "Forest", "Grassland")) %>%
  ggplot(., aes(YearSampled, d14c_corr_mean, color = type.long)) +
  geom_point(aes(shape = Treatment), size = 3, stroke = 1.5) +
  geom_path(aes(group = year.Type), 
            arrow = arrow(type = "open", length = unit(.1, "inches"), ends = "first"),
            show.legend = FALSE,
            position = position_dodge(width = 2, preserve = "single"),
            size = 1.2) +
  geom_path(data = Datm, aes(Date, NHc14), color = "darkgray") +
  geom_errorbar(
    aes(ymin = d14c_corr_l, 
        ymax = d14c_corr_u, 
        color = type.long), 
    width = .5,
    alpha = .3) +
  scale_color_manual(name = 'Land use',
                     values =c('Forest'='#a35513','Grassland'='#1361a3')) +
  scale_shape_manual(name = 'Treatment',
                     values = c("air-dry" = 1, 
                                "air-dry + storage" = 0,
                                "control" = 16)) +
  scale_x_continuous(limits = c(2010, 2020), breaks = c(2013, 2016, 2019)) +
  scale_y_continuous(limits = c(-15, 100)) +
  xlab('Year sampled') +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  facet_grid(cols = vars(type.long)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 14))
dev.off()

# Fig. 7 all samples
png(filename = "f7.ctl.trt.all.png", 
    width = 6,
    height = 4,
    type = "cairo",
    units = "in",
    res = 200)
all.14c.ctl.trt %>%
  filter(Horizon == "A") %>%
  mutate(Location = recode(Site, !!!loc)) %>%
  ggplot(., aes(d14c_corr_mean_ctl, d14c_corr_mean_trt, color = Location, shape = Type)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = 1, intercept = -20, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 20, linetype = "dashed") +
  geom_abline(slope = 1, intercept = -40, linetype = "dotted") +
  geom_abline(slope = 1, intercept = 40, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = d14c_corr_min_trt, 
        ymax = d14c_corr_max_trt, 
        color = Location), 
    width = .25) +
  geom_errorbarh(
    aes(xmin = d14c_corr_min_ctl, 
        xmax = d14c_corr_max_ctl, 
        color = Location), 
    height = .9) +
  scale_color_manual(values = c('Duke, NC' = '#01BB97',
                                'Sierra Nevada, CA' = '#FFC107',
                                'Central Germany' = 'black',
                                'Harvard, MA' = '#1E88E5',
                                'Oak Ridge, TN' = '#D81B60')) +
  scale_shape_manual(name = 'Land use',
                     values = c('F'= 17,'G' = 16),
                     labels = c('Forest','Grassland')) +
  xlab(expression('Control '*Delta*''^14*'C (‰)')) +
  ylab(expression('Treatment '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.height=unit(.8, "cm"),
        aspect.ratio = 1,
        text = element_text(size = 16))
dev.off()

# Fig. 8 duration of storage
png(filename = "f8.dur.store.png", 
    width = 6,
    height = 4,
    type = "cairo",
    units = "in",
    res = 200)
all.14c.ctl.trt %>%
  filter(Horizon == "A") %>%
  filter(Experiment != "rewet") %>%
  select(Type, tme.f, Site, ctl.trt.d14, tme) %>%
  group_by(Type, tme.f, Site) %>%
  add_tally() %>%
  summarise_all(list(mean = mean, sd = sd), na.rm = TRUE) %>%
  mutate(dif_se = ctl.trt.d14_sd/n_mean,
         dif_u = ctl.trt.d14_mean + dif_se*2,
         dif_l = ctl.trt.d14_mean - dif_se*2) %>%
  mutate(Location = factor(recode(Site, !!!loc), exclude = TRUE)) %>%
  ggplot(., aes(tme_mean, ctl.trt.d14_mean, color = Location, shape = Type)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -20, linetype = "dashed") +
  geom_hline(yintercept = 20, linetype = "dashed") +
  geom_hline(yintercept = 40, linetype = "dotted") +
  geom_hline(yintercept = -40, linetype = "dotted") +
  geom_point(size = 3, position = position_dodge2(width = .7, preserve = "single")) +
  geom_errorbar(
    aes(ymin = dif_l,
        ymax = dif_u, 
        color = Location),
    width = .7,
    position = position_dodge2(width = .7, preserve = "single")) +
  scale_color_manual(values = c('Duke, NC' = '#01BB97',
                                'Sierra Nevada, CA' = '#FFC107',
                                'Central Germany' = 'black',
                                'Harvard, MA' = '#1E88E5',
                                'Oak Ridge, TN' = '#D81B60')) +
  scale_shape_manual(name = 'Land use',
                     values = c('F'= 17,'G' = 16),
                     labels = c('Forest','Grassland')) +
  ylab(expression('Control - Treatment '*Delta*''^14*'C (‰)')) +
  xlab("Storage duration (years)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16))
dev.off()

# calculate mean differences
all.14c.ctl.trt %>% 
  group_by(Experiment, Type) %>% 
  select(Experiment, Type, ctl.trt.d14) %>% 
  add_tally() %>%
  summarize_all(list(mean = mean, sd = sd), na.rm = TRUE) %>%
  mutate(dif_se = ctl.trt.d14_sd/n_mean,
         dif_u = ctl.trt.d14_mean + dif_se*2,
         dif_l = ctl.trt.d14_mean - dif_se*2)
