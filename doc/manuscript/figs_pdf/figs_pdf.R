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
                     labels = c('forest',
                                'grassland')) +
  scale_shape_manual(name = 'Treatment',
                     values = c("air-dry" = 1, 
                                "air-dry + storage" = 0,
                                "control" = 16),
                     labels = c("air-dry" = "air-dry", 
                                "air-dry + storage" = "air-dry + storage",
                                "control" = "control-1 (2011)\n control-2 (2019)")) +
  ylab(expression('mean '*delta*''^13*'C (‰)')) +
  facet_grid(cols = vars(Eco)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14))

ggsave(filename = "/Users/jeff/arc-inc/doc/egu-2020/ctl.trt.13c.pdf",
       plot = c13,
       width = 6,
       height = 4,
       units = "in")

all.14c.p <- all.14c.ctl.trt %>%
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

ggsave(filename = "/Users/jeff/arc-inc/doc/egu-2020/all.14c.pdf",
       plot = all.14c.p,
       width = 6.5,
       height = 5,
       units = "in")