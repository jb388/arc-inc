c13 <- iso.12.avg2 %>%
  filter(Period == "inc") %>%
  mutate(YearSampled = ifelse(Experiment == "arc", "2011", "2019")) %>%
  mutate(Eco = ifelse(Type == "F", "Forest", "Grassland")) %>%
  ggplot(., aes(YearSampled, d13c_mean, color = Type)) +
  geom_point(aes(shape = Treatment), size = 4) +
  geom_errorbar(
    aes(ymin = d13c_u, 
        ymax = d13c_l, 
        color = Type), 
    width = .15) +
  scale_color_manual(name = 'Ecosystem',
                     values = c('F'='#a35513',
                                'G'='#1361a3'),
                     labels = c('Forest',
                                'Grassland')) +
  scale_shape_manual(name = 'Treatment',
                     values = c("air-dry" = 1, 
                                "air-dry + storage" = 0,
                                "control" = 16),
                     labels = c("air-dry" = "air-dry/rewet", 
                                "air-dry + storage" = "air-dry/rewet + storage",
                                "control" = "control-1 (2011)\ncontrol-2 (2019)")) +
  ylab(expression('mean '*delta*''^13*'C (‰)')) +
  facet_grid(cols = vars(Eco)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14))

ggsave(filename = "/Users/jeff/arc-inc/doc/manuscript/figs_pdf/ctl.trt.13c.pdf",
       plot = c13,
       device = cairo_pdf,
       width = 6,
       height = 4,
       units = "in")

all.14c.p <- all.14c.ctl.trt %>%
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
  scale_color_manual(values = c('Duke FACE control' = '#01BB97',
                                'Sierra Nevada, CA' = '#FFC107',
                                'Central Germany' = 'black',
                                'Harvard Forest' = '#1E88E5',
                                'Oak Ridge' = '#D81B60')) +
  scale_shape_manual(name = 'Ecosystem',
                     values = c('F'= 17,'G' = 16),
                     labels = c('Forest','Grassland')) +
  xlab(expression('Control '*Delta*''^14*'C (‰)')) +
  ylab(expression('Treatment '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.height=unit(.8, "cm"),
        aspect.ratio = 1)

ggsave(filename = "/Users/jeff/arc-inc/doc/manuscript/figs_pdf/all.14c.pdf",
       plot = all.14c.p,
       device = cairo_pdf,
       width = 6.5,
       height = 5,
       units = "in")


stor.dur.p <- suppressWarnings(
  all.14c.ctl.trt %>%
    filter(Horizon == "A") %>%
    filter(Experiment != "rewet") %>%
    select(Type, tme.f, Site, trt.ctl.d14, tme) %>%
    group_by(Type, tme.f, Site) %>%
    add_tally() %>%
    summarise_all(list(mean = mean, sd = sd), na.rm = TRUE) %>%
    mutate(dif_se = trt.ctl.d14_sd/n_mean,
           dif_u = trt.ctl.d14_mean + dif_se*2,
           dif_l = trt.ctl.d14_mean - dif_se*2) %>%
    mutate(Location = factor(recode(Site, !!!loc), exclude = TRUE)) %>%
    ggplot(., aes(tme_mean, trt.ctl.d14_mean, color = Location, shape = Type)) +
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
    scale_color_manual(values = c('Duke FACE control' = '#01BB97',
                                  'Sierra Nevada, CA' = '#FFC107',
                                  'Central Germany' = 'black',
                                  'Harvard Forest' = '#1E88E5',
                                  'Oak Ridge' = '#D81B60')) +
    scale_shape_manual(name = 'Ecosystem',
                       values = c('F'= 17,'G' = 16),
                       labels = c('Forest','Grassland')) +
    scale_x_continuous(breaks = seq(5,13,2)) +
    ylab(expression('Treatment - Control '*Delta*''^14*'C (‰)')) +
    xlab("Storage duration (years)") +
    theme_bw() +
    theme(panel.grid = element_blank())
)

ggsave(filename = "/Users/jeff/arc-inc/doc/manuscript/figs_pdf/stor.dur.pdf",
       plot = stor.dur.p,
       device = cairo_pdf,
       width = 6.5,
       height = 4,
       units = "in")


ctl.trt.time.p <- all.14c.sum2 %>%
  filter(Period == "inc") %>%
  mutate(year.Type = paste(YearSampled, Type),
         Eco = ifelse(Type == "F", "Forest", "Grassland")) %>%
  ggplot(., aes(YearSampled, d14c_corr_mean, color = Type)) +
  geom_point(aes(shape = Treatment), size = 3, stroke = 1.5) +
  geom_path(aes(group = year.Type), 
            arrow = arrow(type = "open", length = unit(.1, "inches"), ends = "first"),
            show.legend = FALSE,
            position = position_dodge(width = 2, preserve = "single"),
            size = 1.2) +
  geom_path(data = Datm, aes(Date, NHc14, linetype = Atmosphere), color = "darkgray") +
  geom_errorbar(
    aes(ymin = d14c_corr_l, 
        ymax = d14c_corr_u, 
        color = Type), 
    width = .5,
    alpha = .3) +
  scale_color_manual(name = 'Ecosystem',
                     values = c('F'='#a35513',
                                'G'='#1361a3'),
                     labels = c('Forest',
                                'Grassland')) +
  scale_shape_manual(name = 'Treatment',
                     values = c("air-dry" = 1, 
                                "air-dry + storage" = 0,
                                "control" = 16),
                     labels = c("air-dry" = "air-dry/rewet", 
                                "air-dry + storage" = "air-dry/rewet + storage",
                                "control" = "control-1 (2011)\ncontrol-2 (2019)")) +
  guides(linetype = guide_legend(title = NULL)) +
  scale_x_continuous(limits = c(2010, 2020), breaks = c(2013, 2016, 2019)) +
  scale_y_continuous(limits = c(-15, 100)) +
  xlab('Year sampled') +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  facet_grid(cols = vars(Eco)) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave(filename = "/Users/jeff/arc-inc/doc/manuscript/figs_pdf/ctl.trt.time.pdf",
       plot = ctl.trt.time.p,
       device = cairo_pdf,
       width = 6.5,
       height = 4,
       units = "in")
