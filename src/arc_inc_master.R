# Archive incubations, polished script
# Update 6 April 2020: cleaning up code and adding to new git repo
# Updated 11 March 2020
# Notes: 1) d14C corrections were wrong, data corrected since 6 Feb 2020
#        2) simplified CO2 curve plotting (some code now broken that used the data in old format)

library(ggplot2)
library(dplyr)
library(corrplot)

# Read in and prepare t2 respiration data (Xpl samples)
#####
# all of archive data in one table
archive.ts <- read.csv("/Users/jeff/R/14Constraint/archive_ts.csv")
archive.ts <- archive.ts[-which(archive.ts$SampleName == "HEW22-2-2"),] # remove HEW22-2-2 (lost CO2 between t5 and t6)
# archive.ts <- archive.ts[-which(grepl("S",archive.ts$SampleName)), ] # only Hainich samples...

# read in rewet data
rewet.ts <- read.csv("/Users/jeff/R/14Constraint/rewet_ts.csv")
str(rewet.ts)

# summarize rewet by treatment and landcover at each timepoint
rewet.ts.avg <- na.omit(rewet.ts) %>%
  filter(timepoint_cmtv != 5) %>%
  select(timepoint_cmtv, mgCO2_gC_day, Type, Treatment, time_days_t2_cmtv)  %>%
  group_by(Treatment, Type, timepoint_cmtv) %>%
  summarise_all(funs(mean, sd), na.rm = TRUE)
rewet.ts.avg$se_slope <- rewet.ts.avg$mgCO2_gC_day_sd/sqrt(2)
rewet.ts.avg$se_slope_l <- rewet.ts.avg$mgCO2_gC_day_mean - rewet.ts.avg$se_slope
rewet.ts.avg$se_slope_u <- rewet.ts.avg$mgCO2_gC_day_mean + rewet.ts.avg$se_slope

# summarize air-dry + storage data by treatment and landcover at each timepoint
archive.ts.avg <- na.omit(archive.ts) %>%
  filter(timepoint_cmtv != 5) %>%
  select(timepoint_cmtv, mgCO2_gC_day, Type, Treatment, time_days_t2_cmtv)  %>%
  group_by(Treatment, Type, timepoint_cmtv) %>%
  summarise_all(funs(mean, sd), na.rm = TRUE)
archive.ts.avg$se_slope <- archive.ts.avg$mgCO2_gC_day_sd/sqrt(2)
archive.ts.avg$se_slope_l <- archive.ts.avg$mgCO2_gC_day_mean - archive.ts.avg$se_slope
archive.ts.avg$se_slope_u <- archive.ts.avg$mgCO2_gC_day_mean + archive.ts.avg$se_slope

# Plot respiration rates over time
#####
# air-dry plot (2019 samples)
ggplot(rewet.ts.avg, aes(time_days_t2_cmtv_mean, mgCO2_gC_day_mean)) +
  geom_vline(xintercept = 4, color="gray") +
  geom_ribbon(aes(ymin = se_slope_l, ymax = se_slope_u, fill = Type, linetype = Treatment, alpha = Treatment)) +
  geom_line(aes(color = Type, linetype = Treatment)) +
  scale_x_continuous(limits = c(0,18)) +
  scale_y_continuous(limits = c(0,30)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_fill_manual(name = 'Std. Err.',
                    values =c('F'='#a35513','G'='#1361a3'),
                    labels = c('Forest','Grassland')) +
  scale_alpha_manual(name = 'Treatment',
                       values = c(dry = .2, wet = .4),
                       labels = c(dry = 'air-dried (2019)', wet = 'field-moist (2019)')) +
  scale_linetype_manual(name = 'Treatment',
                        values = c(dry = 'dashed', wet ='solid'),
                        labels = c(dry = 'air-dried (2019)', wet = 'field-moist (2019)')) +
  ylab(expression('Respiration Rate (mgCO'[2]*' gC'^-1*'d'^-1*')')) +
  xlab("Time (days)") +
  theme_bw() +
  theme(panel.grid = element_blank())

# air-dry + storage CO2 plot (2011 samples)
ggplot(archive.ts.avg, aes(time_days_t2_cmtv_mean, mgCO2_gC_day_mean)) +
  geom_vline(xintercept = 4, color="gray") +
  geom_ribbon(aes(ymin = se_slope_l, ymax = se_slope_u, fill = Type, linetype = Treatment, alpha = Treatment)) +
  geom_line(aes(color = Type, linetype = Treatment)) +
  scale_x_continuous(limits = c(0,18)) +
  scale_y_continuous(limits = c(0,30)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_fill_manual(name = 'Std. Err.',
                    values =c('F'='#a35513','G'='#1361a3'),
                    labels = c('Forest','Grassland')) +
  scale_alpha_manual(name = 'Treatment',
                     values = c(archive = .2, control = .4),
                     labels = c(archive = 'air-dry + storage (2011)', control = 'field-moist (2011)')) +
  scale_linetype_manual(name = 'Treatment',
                        values = c(archive = 'dashed', control ='solid'),
                        labels = c(archive = 'air-dry + storage (2011)', control = 'field-moist (2011)')) +
  ylab(expression('Respiration Rate (mgCO'[2]*' gC'^-1*'d'^-1*')')) +
  xlab("Time (days)") +
  theme_bw() +
  theme(panel.grid = element_blank())

# all in one
co2.ts <- data.frame(rbind(rewet.ts.avg, archive.ts.avg))
co2.ts$experiment <- ifelse(co2.ts$Treatment == "dry" | co2.ts$Treatment == "wet", "air-dry", "air-dry + storage")

ggplot(co2.ts, aes(time_days_t2_cmtv_mean, mgCO2_gC_day_mean)) +
  geom_vline(xintercept = 4, color="gray") +
  geom_ribbon(aes(ymin = se_slope_l, ymax = se_slope_u, fill = Type, linetype = Treatment, alpha = Treatment)) +
  geom_line(aes(color = Type, linetype = Treatment)) +
  facet_grid(rows = vars(experiment)) +
  scale_x_continuous(limits = c(0,18)) +
  scale_y_continuous(limits = c(0,30)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_fill_manual(name = 'Std. Err.',
                    values =c('F'='#a35513','G'='#1361a3'),
                    labels = c('Forest','Grassland')) +
  scale_alpha_manual(name = 'Treatment',
                     values = c(archive = .2, 
                                dry = .2, 
                                control = .4, 
                                wet = .4),
                     labels = c(archive = 'air-dry + storage (2011)', 
                                control = 'control (2011)',
                                dry = "air-dry (2019)",
                                wet = "control (2019)")) +
  scale_linetype_manual(name = 'Treatment',
                        values = c(archive = 'dashed', 
                                   control ='solid',
                                   dry = "dashed",
                                   wet = "solid"),
                        labels = c(archive = 'air-dry + storage (2011)',
                                   control = 'control (2011)',
                                   dry = "air-dry (2019)",
                                   wet = "control (2019)")) +
  ylab(expression('Respiration Rate (mgCO'[2]*' gC'^-1*'d'^-1*')')) +
  xlab("Time (days)") +
  theme_bw() +
  theme(panel.grid = element_blank())


#####

# Plot CO2 accumulation over time
#####
# need to fix this for new data format
r1.se$Period <- factor(ifelse(r1.se$time_days_cmtv < 4.1, "pre","inc"),levels=c("pre","inc"))
r3.se2 <- r3.se[-which(r3.se$time_days_cmtv > 15),] # get rid of final points

co2.p1 <- ggplot(r3.se2, aes(time_days_cmtv, mean.co2.cm)) +
  geom_vline(xintercept=4, color="gray") +
  geom_ribbon(aes(ymin = se_co2cm_l, ymax = se_co2cm_u, fill=Type), alpha=0.4) +
  geom_ribbon(data=r1.se, aes(ymin = se_co2cm_l, ymax = se_co2cm_u, fill=Type), alpha=0.2) +
  geom_line(aes(color=Type, linetype='solid')) +
  geom_line(data=r1.se, aes(time_days_cmtv, mean.co2.cm, color=Type, linetype="dashed")) +
  geom_point(aes(color=Type, shape='19'), size=3) +
  geom_point(data=r1.se, aes(time_days_cmtv, mean.co2.cm, color=Type, shape='5'), size=3) +
  scale_x_continuous(limits=c(0,18)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(values =c('19'=19,'5'=5),
                     guide = FALSE) +
  scale_fill_manual(name = 'Std. Err.',
                    values =c('F'='#a35513','G'='#1361a3'),
                    labels = c('Forest','Grassland')) +
  scale_linetype_manual(name = 'Time',
                        values = c('dashed'='dashed', 'solid'='solid'),
                        labels = c('dashed'='t1','solid'='t2')) +
  ylab(expression('Cumulative mgCO'[2]*' gC'^-1*'')) +
  xlab("Time (days)") +
  theme_bw() +
  theme(panel.grid = element_blank())
co2.p1
#####

# Read in isotope data and prepare data frame (Xpl samples)
#####
# Compare 13C/14C data
r3.iso <- read.csv("/Users/jeff/R/14Constraint/arc_inc_iso_06-02-2020.csv")
str(r3.iso)
r3.iso$Type <- ifelse(grepl("G", r3.iso$ID),"G","F")
r3.iso$Exploratory <- ifelse(grepl("H", r3.iso$ID),"Hainich","Schorfheide")

# avg reps from t2
r3.iso.t2 <- r3.iso[r3.iso$Time=="t2",] 
r3.iso.t2.pre <- r3.iso.t2[r3.iso.t2$Period=="pre",]
r3.iso.t2.inc <- r3.iso.t2[r3.iso.t2$Period=="inc",]
r3.iso.t2.pre.avg <- aggregate(.~ ID, data=r3.iso.t2.pre[,c(2,5:10)], FUN="mean", drop=F) # avg numeric cols
r3.iso.t2.pre.avg$Period <- "pre" # add back factor
r3.iso.t2.inc.avg <- aggregate(.~ ID, data=r3.iso.t2.inc[,c(2,5:10)], FUN="mean", drop=F, na.action = na.pass)
r3.iso.t2.inc.avg$Period <- "inc"
r3.iso.t2.avg <- rbind(r3.iso.t2.pre.avg, r3.iso.t2.inc.avg) # combine
r3.iso.t2.avg$Time <- "t2"
r3.iso.avg <- rbind(r3.iso.t2.avg, r3.iso[r3.iso$Time=="t1",2:10]) # combine avg t2 df w/ t1
r3.iso.avg$Type <- ifelse(grepl("G", r3.iso.avg$ID),"G","F")
#####

# Compare 13C from t1 against pre and post at t2 (Xpl samples)
#####
r3.iso.t2.13C <- data.frame(pre.13C = r3.iso.t2[r3.iso.t2$Period=="pre","d13C_CO2"],
                            inc.13C = r3.iso.t2[r3.iso.t2$Period=="inc","d13C_CO2"],
                            Type = r3.iso.t2[r3.iso.t2$Period=="pre","Type"],
                            Xpl = r3.iso.t2[r3.iso.t2$Period=="pre","Exploratory"])
ggplot(r3.iso.t2.13C, aes(pre.13C, inc.13C, color=Type, shape=Xpl)) +
  geom_abline(slope=1,linetype="dashed", color="gray") +
  geom_point(size=2) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_x_continuous(limits=c(-28.4,-24.2)) +
  scale_y_continuous(limits=c(-28.4,-24.2)) +
  coord_fixed() + # default is 1:1 aspect ratio
  theme_bw() +
  theme(panel.grid = element_blank())

# compare 13C from t1 against t2
r3.iso.13C <- data.frame(ID = r3.iso.avg[which(r3.iso.avg$Time == "t1" & r3.iso.avg$Period == "inc"),"ID"],
                         Type = r3.iso.avg[which(r3.iso.avg$Time == "t1" & r3.iso.avg$Period == "inc"),"Type"],
                         d13C_CO2_t1 = r3.iso.avg[which(r3.iso.avg$Time == "t1" & r3.iso.avg$Period == "inc"),"d13C_CO2"],
                         d13C_CO2_t2_pre = r3.iso.avg[which(r3.iso.avg$Time == "t2" & r3.iso.avg$Period == "pre"),"d13C_CO2"],
                         d13C_CO2_t2_inc = r3.iso.avg[which(r3.iso.avg$Time == "t2" & r3.iso.avg$Period == "inc"),"d13C_CO2"])
r3.iso.13C$Exploratory <- ifelse(grepl("H", r3.iso.13C$ID),"Hainich","Schorfheide")
ggplot(r3.iso.13C, aes(d13C_CO2_t1, d13C_CO2_t2_inc, color=Type, shape=Exploratory)) +
  geom_abline(slope=1,linetype="dashed", color="gray") +
  geom_point(size=2.5) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_x_continuous(limits=c(-28.4,-22.7)) +
  scale_y_continuous(limits=c(-28.4,-22.7)) +
  coord_fixed() + # default is 1:1 aspect ratio
  theme_bw() +
  theme(panel.grid = element_blank())
#####

# Prepare 14C data and initial plotting (Xpl samples)
#####
r3.iso.14C.2 <- data.frame(ID = rep(r3.iso[which(r3.iso$Time == "t1" & r3.iso$Period == "inc"),"ID"],each=2),
                           Type = rep(r3.iso[which(r3.iso$Time == "t1" & r3.iso$Period == "inc"),"Type"],each=2),
                           d14C_CO2_t1 = rep(r3.iso[which(r3.iso$Time == "t1" & r3.iso$Period == "inc"),"d14C_CO2_corr"],each=2),
                           d14C_CO2_t2_pre = r3.iso[which(r3.iso$Time == "t2" & r3.iso$Period == "pre"),"d14C_CO2_corr"],
                           d14C_CO2_t2_inc = r3.iso[which(r3.iso$Time == "t2" & r3.iso$Period == "inc"),"d14C_CO2_corr"],
                           CO2_cmtv_t1_pre = r3.iso[which(r3.iso$Time == "t1" & r3.iso$Period == "pre"),"CO2_mg_gC_cmtv"],
                           CO2_cmtv_t1_inc = r3.iso[which(r3.iso$Time == "t1" & r3.iso$Period == "inc"),"CO2_mg_gC_cmtv"],
                           CO2_cmtv_t2_pre = r3.iso[which(r3.iso$Time == "t2" & r3.iso$Period == "pre"),"CO2_mg_gC_cmtv"],
                           CO2_cmtv_t2_inc = r3.iso[which(r3.iso$Time == "t2" & r3.iso$Period == "inc"),"CO2_mg_gC_cmtv"],
                           d14c_err_t1_inc = rep(r3.iso[which(r3.iso$Time == "t1" & r3.iso$Period == "inc"),"d14_err"],each=2),
                           d14c_err_t2_inc = r3.iso[which(r3.iso$Time == "t2" & r3.iso$Period == "inc"),"d14_err"])

# difference in main inc period CO2 against difference in 14C (t2-t1)
r3.iso.14C.2$dif.co2.inc <- r3.iso.14C.2$CO2_cmtv_t1_inc-r3.iso.14C.2$CO2_cmtv_t2_inc
r3.iso.14C.2$dif.co2.inc.norm <- r3.iso.14C.2$dif.co2.inc/r3.iso.14C.2$CO2_cmtv_t1_inc
r3.iso.14C.2$dif.co2.pre <- r3.iso.14C.2$CO2_cmtv_t1_pre-r3.iso.14C.2$CO2_cmtv_t2_pre
r3.iso.14C.2$dif.14c <- r3.iso.14C.2$d14C_CO2_t2_inc-r3.iso.14C.2$d14C_CO2_t1
r3.iso.14C.2$Exploratory <- ifelse(grepl("H", r3.iso.14C.2$ID),"Hainich","Schorfheide")

# look at correlations for forests and grasslands separately
# need to add C, N, etc. data...
r3.iso.G <- r3.iso.14C.2[which(r3.iso.14C.2$Type == "G"),]
r3.iso.F <- r3.iso.14C.2[which(r3.iso.14C.2$Type == "F"),]
par(mar=c(2,2,2,2))
corrplot(cor(r3.iso.G[,c(3:9,12:15)], use="pairwise.complete.obs"), method="number")
corrplot(cor(r3.iso.F[,c(3:9,12:15)], use="pairwise.complete.obs"), method="number")
# strong correlations between CO2 and 14C in grasslands, but none in forests
dev.off()

# boxplots of differences, Xpl samples only
ggplot(r3.iso.14C.2, aes(Type, dif.14c, color=Type)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_boxplot(size=.8) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=18))
## stats:
# Forests
t.test(r3.iso.F$dif.14c, conv.level=0.95)
# t = -5.1639, df = 11, p-value = 0.0003114
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -16.494170  -6.635703
# sample estimates:
#   mean of x 
# -11.56494 

# weighted
summary(lm(d14C_CO2_t2_inc ~ d14C_CO2_t1, r3.iso.F, weights=1/r3.iso.F$d14c_err_t2_inc)) # not sure this does what I think...

# Grasslands
t.test(r3.iso.G$dif.14c, conv.level=0.95)
# t = 6.8988, df = 11, p-value = 2.592e-05
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   15.84126 30.68479
# sample estimates:
#   mean of x 
# 23.26302

# Plot dif in CO2, normalized by CO2 released
ggplot(r3.iso.14C.2, aes(dif.co2.inc.norm, dif.14c, color=Type)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(size=2) +
  geom_smooth(method="lm") +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid = element_blank())
r3.iso.14C.2 %>% filter(Type == "G") %>% lm(dif.14c~dif.co2.inc.norm,.) %>% summary # no effect
r3.iso.14C.2 %>% filter(Type == "F") %>% lm(dif.14c~dif.co2.inc.norm,.) %>% summary # no effect

# Plot dif in cumulative CO2 t1-t2 against dif in 14C t1-t2
ggplot(r3.iso.14C.2, aes(dif.co2.inc, dif.14c, color=Type)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(size=2) +
  geom_smooth(method="lm") +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Plot max resp rate difference against dif in 14C t1-t2
max.slope.dif <- data.frame(ID=rownames(tapply(r1.ts$slope_mgCO2gC_d_t1, r1.ts$ID, max, na.rm=T)-tapply(r3.ts$slope_mgCO2gC_day, r3.ts$ID, max, na.rm=T)),
                            peak.resp.rate.dif=tapply(r1.ts$slope_mgCO2gC_d_t1, r1.ts$ID, max, na.rm=T)-tapply(r3.ts$slope_mgCO2gC_day, r3.ts$ID, max, na.rm=T))
left_join(r3.iso.14C.2,max.slope.dif) %>% 
  ggplot(., aes(peak.resp.rate.dif, dif.14c, color=Type)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(size=2) +
  geom_smooth(method="lm") +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Look at effect of main vs. pre incubation period [no real trend]
# NB: "pre" points are the t2 preincubation minus the t1 main incubation
ggplot(r3.iso.14C.2, aes(dif.co2.inc, dif.14c, shape=Type)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(aes(color="coral"), size=2) +
  geom_point(aes(dif.co2.pre, dif.14c, color="steelblue4"), size=2) +
  scale_color_manual(name = 'Period',
                     values = c('coral'='coral','steelblue4'='steelblue4'),
                     labels = c('inc','pre')) +
  facet_grid(.~Type) +
  theme_bw() +
  theme(panel.grid = element_blank())
# possibly a forest trend?
summary(lm(d14C_CO2_t2_inc ~ d14C_CO2_t2_pre + Type, r3.iso.14C.2)) # no effect of Type
#####

## Plot t1 14C against t2 14C w/ error bars (Xpl samples)
## Plot t2 14C pre vs. inc w/ error bars (Xpl samples)
#####
r3.iso.inc.t1 <- r3.iso[r3.iso$Time == "t1" & r3.iso$Period == "inc",c("ID","Type","d14C_CO2_corr")]
r3.iso.inc.t1 <- r3.iso.inc.t1[order(r3.iso.inc.t1$ID),]
r3.iso.pre.t2 <- r3.iso[r3.iso$Time == "t2" & r3.iso$Period == "pre",c("ID","d14C_CO2_corr")]
r3.iso.pre.t2 <- r3.iso.pre.t2[order(r3.iso.pre.t2$ID),]
r3.iso.inc.t2 <- r3.iso[r3.iso$Time == "t2" & r3.iso$Period == "inc",c("ID","d14C_CO2_corr")]
r3.iso.inc.t2 <- r3.iso.inc.t2[order(r3.iso.inc.t2$ID),]
r3.iso.inc.t2.avg <- data.frame(ID = unique(r3.iso.inc.t2$ID),
                                d14C_CO2_t2_inc = tapply(r3.iso.inc.t2$d14C_CO2_corr, r3.iso.inc.t2$ID, mean, na.rm=T),
                                d14C_CO2_t2_pre = tapply(r3.iso.pre.t2$d14C_CO2_corr, r3.iso.pre.t2$ID, mean, na.rm=T))
r3.iso.inc <- dplyr::left_join(r3.iso.inc.t1, r3.iso.inc.t2.avg, by="ID")
r3.iso.inc$t2_inc_top <- unlist(tapply(r3.iso.inc.t2$d14C_CO2_corr, r3.iso.inc.t2$ID, max))       
r3.iso.inc$t2_inc_bot <- unlist(tapply(r3.iso.inc.t2$d14C_CO2_corr, r3.iso.inc.t2$ID, min))
r3.iso.inc$t2_pre_top <- unlist(tapply(r3.iso.pre.t2$d14C_CO2_corr, r3.iso.pre.t2$ID, max))       
r3.iso.inc$t2_pre_bot <- unlist(tapply(r3.iso.pre.t2$d14C_CO2_corr, r3.iso.pre.t2$ID, min))

# Xpl only: t1 vs. t2 14C (r3.iso.inc)
range(r3.iso.inc[,3:4], na.rm=T)
ggplot(r3.iso.inc, aes(d14C_CO2_corr, d14C_CO2_t2_inc, color=Type)) +
  geom_hline(yintercept = 45.8, linetype="dotted") +
  geom_vline(xintercept = 45.8, linetype="dotted") +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method="lm", se=T) +
  geom_point(aes(color = Type),size=3) +
  geom_errorbar(aes(ymin=t2_inc_bot, ymax=t2_inc_top, color=Type), width=3.5) +
  # geom_text(aes(label=ID), angle=45, size=3) +
  coord_fixed(xlim = c(30,105), ylim = c(30,105)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression(Delta*''^14*'C control (2011)')) +
  ylab(expression(Delta*''^14*'C air-dry + storage (2011)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=16))

# Add margin of error lines
library(ISRaD)
graven
dateRange <- list(2000.5, 2015.5)
range <- seq(dateRange[[1]], dateRange[[2]], by = 1)
rate <- list() 
for(i in seq_along(range)[-length(range)]) {
  rate[[i]] <- (graven[graven$Date == range[i]+1, "NHc14"]-graven[graven$Date == range[i], "NHc14"])/
    (graven[graven$Date == range[i]+1, "Date"]-graven[graven$Date == range[i], "Date"])
}
mean(unlist(rate))*5

ggplot(r3.iso.inc, aes(d14C_CO2_corr, d14C_CO2_t2_inc, color=Type)) +
  geom_hline(yintercept = 45.8, linetype="dotted") +
  geom_vline(xintercept = 45.8, linetype="dotted") +
  geom_abline(slope=1, intercept=25, linetype = "dashed") + # 5 yrs
  geom_abline(slope=1, intercept=-25, linetype = "dashed") + # 5 yrs
  # geom_abline(slope=1, intercept=-6, linetype = "dotdash") +
  # geom_abline(slope=1, intercept=6, linetype = "dotdash") +
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(color = Type),size=3) +
  geom_errorbar(aes(ymin=t2_inc_bot, ymax=t2_inc_top, color=Type), width=3.5) +
  coord_fixed(xlim = c(30,105), ylim = c(30,105)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression(Delta*''^14*'C of initial incubation')) +
  ylab(expression(Delta*''^14*'C of final incubation')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=18))

# look at trend in 14C, by type
# grasslands:
r3.iso.14C.2 %>% filter(Type=="G") %>% lm(d14C_CO2_t2_inc ~ d14C_CO2_t1, .) %>% summary()
# slope = 0.49, intercept = 59.95, r2 = 0.44, p = 0.0116
r3.iso.14C.2 %>% filter(Type=="G") %>% filter(ID != "HEG10") %>% lm(d14C_CO2_t2_inc ~ d14C_CO2_t1, .) %>% summary()
# excluding HEG10 slope = 0.86, intercept = 26, r2 = 0.83, p = 0.0002
# forests:
r3.iso.14C.2 %>% filter(Type=="F") %>% lm(d14C_CO2_t2_inc ~ d14C_CO2_t1, .) %>% summary()
# slope = 0.70, intercept = 16, r2 = 0.20, p = 0.08

# pre v. main inc
range(r3.iso.inc[,6:9], na.rm=T)
ggplot(r3.iso.inc, aes(d14C_CO2_t2_pre, d14C_CO2_t2_inc)) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_point(aes(color = Type), size=2) +
  geom_errorbar(aes(ymin=t2_inc_bot, ymax=t2_inc_top, color=Type), width=3.5) +
  geom_errorbarh(aes(xmin=t2_pre_bot, xmax=t2_pre_top, color=Type), width=3.5) +
  coord_fixed(xlim = c(50,125), ylim = c(50,125)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  facet_grid(.~Type) +
  theme_bw() +
  theme(panel.grid = element_blank())
# all points same plot
range(c(r3.iso.inc$d14C_CO2_t2_pre, r3.iso.inc$d14C_CO2_t2_inc), na.rm=T)
ggplot(r3.iso.inc, aes(d14C_CO2_t2_pre, d14C_CO2_t2_inc)) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  # geom_smooth(method="lm", color="black") +
  geom_point(aes(color = Type), size=3) +
  geom_errorbar(aes(ymin=t2_inc_bot, ymax=t2_inc_top, color=Type), width=3.5) +
  geom_errorbarh(aes(xmin=t2_pre_bot, xmax=t2_pre_top, color=Type), width=3.5) +
  coord_fixed(xlim = c(50,115), ylim = c(50,115)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression(Delta*''^14*'C of pre-inc respiration')) +
  ylab(expression(Delta*''^14*'C of main inc respiration')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=18))
summary(lm(d14C_CO2_t2_inc~d14C_CO2_t2_pre,r3.iso.inc))
t.test(r3.iso.inc$d14C_CO2_t2_pre, r3.iso.inc$d14C_CO2_t2_inc, paired=T, conf.level = 0.95)
#####

## Plot 14C t1 vs. 14C t2 w/ error bars, all samples
#####
# read in data
r1 <- read.csv("/Users/jeff/R/14Constraint/arc_inc.csv")
r1$CN <- r1$C_mg_g/r1$N_mg_g
# remove outliers
r1 <- r1[-which(r1$SampleName == "HEW26-1" | r1$SampleName == "HEW49-1"),]
r1$d14_dif <- r1$d14_2_corr-r1$d14_1 # t2-t1
r1$d14_dif_rel <- r1$d14_dif/r1$d14_1
r1$d14_dif_norm <- (r1$d14_dif-mean(r1$d14_dif, na.rm=T))/sd(r1$d14_dif, na.rm=T)
# restrict to only A horizon samples
r1.a <- r1[which(r1$Horizon == "A"),]
r1.a$OG.SN <- factor(r1.a$OG.SN, exclude=NULL)

# make simple plotting df
r1.se <- data.frame(OG.SN = sort(unique(r1.a$OG.SN)))
r1.se$t1_inc_top <- unlist(tapply(r1.a$d14_1, r1.a$OG.SN, max, na.rm=T))       
r1.se$t1_inc_bot <- unlist(tapply(r1.a$d14_1, r1.a$OG.SN, min, na.rm=T))
r1.se$t2_inc_top <- unlist(tapply(r1.a$d14_2_corr, r1.a$OG.SN, max, na.rm=T))       
r1.se$t2_inc_bot <- unlist(tapply(r1.a$d14_2_corr, r1.a$OG.SN, min, na.rm=T))
r1.se$d14_1 <- unlist(tapply(r1.a$d14_1, r1.a$OG.SN, mean, na.rm=T))
r1.se$d14_2_corr <- unlist(tapply(r1.a$d14_2_corr, r1.a$OG.SN, mean, na.rm=T))
r1.se$land_cover <- r1.a[match(r1.se$OG.SN, r1.a$OG.SN),"land_cover"]
r1.se$Site <- r1.a[match(r1.se$OG.SN, r1.a$OG.SN),"Site"]
r1.se$Round <- factor(r1.a[match(r1.se$OG.SN, r1.a$OG.SN),"Round"])

range(r1.se[,2:5], na.rm=T)
ggplot(r1.se, aes(d14_1, d14_2_corr)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_vline(xintercept = 0, color="gray") +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method="lm", color="black") +
  geom_point(aes(color = land_cover, shape = Site), size=3) +
  geom_errorbar(aes(ymin=t2_inc_bot, ymax=t2_inc_top, color=land_cover), width=3.5) +
  geom_errorbarh(aes(xmin=t1_inc_bot, xmax=t1_inc_top, color=land_cover), width=3.5) +
  coord_fixed(xlim = c(-12,385), ylim = c(-12,385)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression(Delta*''^14*'C of HR '*italic(t[initial]))) +
  ylab(expression(Delta*''^14*'C of HR '*italic(t[final]))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size=18))
summary(lm(d14_2_corr ~ d14_1, r1.se))

# TN sites excluded:
max(r1.se[which(r1.se$Site != "Tennessee"),c("d14_1","d14_2_corr")])
filter(r1.se, Site != "Tennessee") %>% 
  # filter(., Round == 2 | Site != "Germany") %>%
  ggplot(., aes(d14_1, d14_2_corr)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_vline(xintercept = 0, color="gray") +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method="lm", color="black") +
  geom_point(aes(color = land_cover, shape = Site), size=2.5) +
  geom_errorbar(aes(ymin=t2_inc_bot, ymax=t2_inc_top, color=land_cover), width=3.5) +
  geom_errorbarh(aes(xmin=t1_inc_bot, xmax=t1_inc_top, color=land_cover), width=3.5) +
  geom_text(aes(label=OG.SN), angle=45, size=3) +
  coord_fixed(xlim = c(-5,160), ylim = c(-5,160)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression(Delta*''^14*'C of HR '*italic(t[1]))) +
  ylab(expression(Delta*''^14*'C of HR '*italic(t[2]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
filter(r1.se, Site != "Tennessee") %>% lm(d14_2_corr ~ d14_1, .) %>% summary
filter(r1.se, Site != "Tennessee") %>% filter(., Round == 2 | Site != "Germany") %>% lm(d14_2_corr ~ d14_1, .) %>% summary

# Xpl sites only:
max(r1.se[which(r1.se$Site == "Germany"),c("d14_1","d14_2_corr")])
filter(r1.se, Site == "Germany") %>% 
  ggplot(., aes(d14_1, d14_2_corr)) +
  geom_hline(yintercept = 0, color="gray") +
  geom_vline(xintercept = 0, color="gray") +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method="lm", color="black") +
  geom_point(aes(color = land_cover, shape = Round), size=2.5) +
  geom_errorbar(aes(ymin=t2_inc_bot, ymax=t2_inc_top, color=land_cover), width=3.5) +
  geom_errorbarh(aes(xmin=t1_inc_bot, xmax=t1_inc_top, color=land_cover), width=3.5) +
  coord_fixed(xlim = c(-5,105), ylim = c(-12,105)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression(Delta*''^14*'C of HR '*italic(t[1]))) +
  ylab(expression(Delta*''^14*'C of HR '*italic(t[2]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
filter(r1.se, Site == "Germany") %>% lm(d14_2_corr ~ d14_1, .) %>% summary
xpl.r2 <- filter(r1.se, Site == "Germany") %>% filter(., Round == 2) 
t.test(xpl.r2$d14_1, xpl.r2$d14_2_corr, paired=T, conf.level = 0.95)
t.test(xpl.r2[xpl.r2$land_cover=="forest","d14_2_corr"], xpl.r2[xpl.r2$land_cover=="forest","d14_1"], paired=T, conf.level = 0.95)
t.test(xpl.r2[xpl.r2$land_cover=="grass","d14_2_corr"], xpl.r2[xpl.r2$land_cover=="grass","d14_1"], paired=T, conf.level = 0.95)
##### 

## Paired t-test of t1 vs. t2 14C for data from all sites (with A hz only and outliers excluded)
#####
# assess normality
r1.a[,c("OG.SN","d14_1","d14_2_corr","d14_dif","d14_dif_norm")]
plot(lm(d14_1 ~ d14_2_corr, r1.a)) # not normal
hist(r1.a$d14_dif) # not too bad
shapiro.test(r1.a$d14_dif) # normal enough (p > 0.05)
qqnorm(r1.a$d14_dif); qqline(r1.a$d14_dif)

# grasslands
hist(r1.a[r1.a$land_cover == "grass", "d14_dif"]) # bimodal
shapiro.test(r1.a.g$d14_dif) # not normal
# split into 2 unimodal groups:
r1.a$land_cover2 <- ifelse(r1.a$land_cover == "grass" & r1.a$d14_dif > 0, "grass +",
                           ifelse(r1.a$land_cover == "grass" & r1.a$d14_dif < 0, "grass -", "forest"))
hist(r1.a[r1.a$land_cover2 == "grass +", "d14_dif"])
shapiro.test(r1.a[r1.a$land_cover2 == "grass +", "d14_dif"]) # normal enough
hist(r1.a[r1.a$land_cover2 == "grass -", "d14_dif"])
shapiro.test(r1.a[r1.a$land_cover2 == "grass -", "d14_dif"]) # normal

# forests
r1.a.f <- filter(r1.a, land_cover=="forest") 
hist(r1.a.f$d14_dif) # left skew
shapiro.test(r1.a.f$d14_dif) # normal enough (p = 0.077)


# t-tests
# raw data, all
t.test(r1.a$d14_2_corr, r1.a$d14_1, paired=T, conv.level=0.95)
# t = -4.719, df = 59, p-value = 1.499e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -26.99682 -10.91931
# sample estimates:
#   mean of the differences 
# -18.95807

# landuse effect
summary(lm(d14_dif~land_cover2,r1.a)) # grass + highly signifcantly different
summary(lm(d14_dif_norm~land_cover,r1.a)) # p < 0.001

# grasslands, +
t.test(r1.a[r1.a$land_cover2 == "grass +", "d14_2_corr"],
       r1.a[r1.a$land_cover2 == "grass +", "d14_1"], paired=T, conv.level=0.95)
# t = 7.3921, df = 12, p-value = 8.364e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   17.19900 31.57521
# sample estimates:
#   mean of the differences 
# 24.3871 

# grasslands, -
t.test(r1.a[r1.a$land_cover2 == "grass -", "d14_2_corr"],
       r1.a[r1.a$land_cover2 == "grass -", "d14_1"], paired=T, conv.level=0.95)
# t = -7.4123, df = 7, p-value = 0.0001479
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -49.72166 -25.67054
# sample estimates:
#   mean of the differences 
# -37.6961

# forests
t.test(r1.a.f$d14_2_corr, r1.a.f$d14_1, paired=T, conv.level=0.95)
# t = -7.3165, df = 39, p-value = 7.909e-09
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -36.41374 -20.64076
# sample estimates:
#   mean of the differences 
# -28.52725
r1.a.f2 <- filter(r1.a.f, Site != "Tennessee")
t.test(r1.a.f2$d14_2_corr, r1.a.f2$d14_1, paired=T, conv.level=0.95) # much improved...
#####

## Plot 95% CIs of differences
#####
r1.ci <- data.frame(land_cover2 = c("forest", "grass -", "grass +"),
                    d14C_dif = unlist(tapply(r1.a$d14_dif, r1.a$land_cover2, mean, na.rm=T)))
r1.ci[r1.ci$land_cover2 == "forest","u95"] <- t.test(r1.a[r1.a$land_cover2=="forest","d14_dif"], conv.level=0.95)$conf.int[2]
r1.ci[r1.ci$land_cover2 == "forest","l95"] <- t.test(r1.a[r1.a$land_cover2=="forest","d14_dif"], conv.level=0.95)$conf.int[1]
r1.ci[r1.ci$land_cover2 == "grass +","u95"] <- t.test(r1.a[r1.a$land_cover2=="grass +","d14_dif"], conv.level=0.95)$conf.int[2]
r1.ci[r1.ci$land_cover2 == "grass +","l95"] <- t.test(r1.a[r1.a$land_cover2=="grass +","d14_dif"], conv.level=0.95)$conf.int[1]
r1.ci[r1.ci$land_cover2 == "grass -","u95"] <- t.test(r1.a[r1.a$land_cover2=="grass -","d14_dif"], conv.level=0.95)$conf.int[2]
r1.ci[r1.ci$land_cover2 == "grass -","l95"] <- t.test(r1.a[r1.a$land_cover2=="grass -","d14_dif"], conv.level=0.95)$conf.int[1]

ggplot(r1.ci, aes(land_cover2, d14C_dif, color = land_cover2)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(aes(color = land_cover2), size=3) +
  geom_point(data=r1.ci, aes(color = land_cover2), size=2) +
  geom_errorbar(aes(ymin=l95, ymax=u95, color=land_cover2), width=.2) +
  scale_color_manual(name = 'Landuse',
                     values =c('forest'='chocolate4','grass -'='olivedrab4', 'grass +'="olivedrab3"),
                     labels = c('Forest','Grassland -','Grassland +')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(face="bold", color="black"),
        text = element_text(size=18))

#####

## Back to back histograms
#####
hb2b <- Hmisc::histbackback(r1.a[r1.a$land_cover=="forest","d14_dif_norm"],
                            r1.a[r1.a$land_cover=="grass","d14_dif_norm"],
                            brks=seq(-2,1.5,.5),
                            xlab=c("Forest","Grassland"),
                            probability=TRUE,
                            axes=F, 
                            xlim=c(-1,1))
# axTicks(2) # tells you scale of axis
axis(2, at=seq(0,7,1), labels=seq(-2,1.5,.5), cex.axis = .7)
axis(1)
abline(a=4, b=0, lty="dashed")
barplot(-hb2b$left, col="#8b4513", add=T, space=0, horiz=T, axes=F) # forest
barplot(hb2b$right, col="#698b22", add=T, space=0, horiz=T, axes=F) # grass

range(r1.a$d14_dif, na.rm=T)
par(mar=c(5.1,4.5,2.1,2.1))
hb2b.raw <- Hmisc::histbackback(r1.a[r1.a$land_cover=="forest","d14_dif"],
                            r1.a[r1.a$land_cover=="grass","d14_dif"],
                            brks=seq(-80,60,20),
                            xlab=c("Forest","Grassland"),
                            probability=TRUE,
                            axes=F,
                            xlim=c(-0.022,0.022))
axis(2, at=seq(0,7,1), labels=seq(-80,60,20), cex.axis = 1.5)
axis(1, cex.axis = 1.5)
title(xlab="Density", ylab=expression('Difference in '*Delta*''^14*'C of HR'), cex.lab = 1.5)
abline(a=4, b=0, lty="dashed")
barplot(-hb2b.raw$left, col="#8b4513", add=T, space=0, horiz=T, axes=F) # forest
barplot(hb2b.raw$right, col="#698b22", add=T, space=0, horiz=T, axes=F) # grass
dev.off()
#####

## Statistics
#####
r1.a$delta_hr <- r1.a$mgCO2_C_gsoilC_day_t2-r1.a$mgCO2_C_gsoilC_day_t1
summary(lm(d14_dif~delta_hr, r1.a)) # sig
# Grasslands
r1.a.g <- r1.a[r1.a$land_cover=="grass",]
cor.g <- cor(r1.a.g[,c(14:15,32:34,36,42:43,46)], use="pairwise.complete.obs")
dimnames(cor.g)[1] <- list(abbreviate(unlist(dimnames(cor.g)[1]), minlength=6))
dimnames(cor.g)[2] <- list(abbreviate(unlist(dimnames(cor.g)[2]), minlength=6))
corrplot(cor.g, method="number", tl.cex=.8)

ggplot(r1.a.g, aes(C_mg_g, d14_dif)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank())
summary(lm(d14_dif~C_mg_g,r1.a.g)) # ns

# Forest
r1.a.f <- r1.a[r1.a$land_cover=="forest",]
cor.f <- cor(r1.a.f[,c(12,14:15,32:34,36,42:43,46)], use="pairwise.complete.obs")
dimnames(cor.f)[1] <- list(abbreviate(unlist(dimnames(cor.f)[1]), minlength=6))
dimnames(cor.f)[2] <- list(abbreviate(unlist(dimnames(cor.f)[2]), minlength=6))
corrplot(cor.f, method="number", tl.cex=.8)

ggplot(r1.a, aes(C_mg_g, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, color="gray") +
  geom_smooth(method="lm") +
  geom_point(size=2) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid = element_blank())

summary(lm(d14_dif ~ CN * land_cover, r1.a)) # ns
summary(lm(d14_dif ~ CN, r1.a.f)) # ns
summary(lm(d14_dif ~ CN, r1.a.g)) # ns

# Plot dif in CO2 against 14C dif [no trend when normalized by total CO2 respired]
r1.a$c_dif <- (r1.a$mgCO2_C_gsoil_C_total_t2-r1.a$mgCO2_C_gsoil_C_total_t1)
r1.a$c_dif_norm <- r1.a$c_dif/r1.a$mgCO2_C_gsoil_C_total_t1
r1.a$Round <- factor(r1.a$Round)

ggplot(r1.a, aes(mgCO2_C_gsoil_C_total_t2, d14_2, color=land_cover)) +
  geom_hline(yintercept=0, color="gray") +
  geom_vline(xintercept=0, color="gray") +
  geom_point(size=4) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression("Total C respired t2 (mgC gC"*''^-1*") ")) +
  ylab(expression(Delta*''^14*'C')) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12, face="bold"),
        axis.text = element_text(size=10, face="bold", color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))

ggplot(r1.a, aes(c_dif_norm, d14_dif, color=land_cover, shape=Round)) +
  geom_hline(yintercept=0, linetype="dashed", size=1) +
  geom_vline(xintercept=0, linetype="dashed", size=1) +
  geom_point(size=2) +
  scale_y_continuous(limits=c(-65,65)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression("Relative dif. in C respired (mgC gC"*''^-1*") ")) +
  ylab(expression(Delta*''^14*'C dif. ')) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12, face="bold"),
        axis.text = element_text(size=10, face="bold", color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))
filter(r1.a, Round !=2) %>%
  ggplot(., aes(c_dif_norm, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, linetype="dashed", size=1) +
  geom_vline(xintercept=0, linetype="dashed", size=1) +
  geom_point(size=2) +
  scale_y_continuous(limits=c(-65,65)) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression("Relative dif. in C respired (mgC gC"*''^-1*") ")) +
  ylab(expression(Delta*''^14*'C dif. ')) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12, face="bold"),
        axis.text = element_text(size=10, face="bold", color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))

summary(lm(d14_dif ~ c_dif_norm + land_cover, r1.a)) # grass sig
summary(lm(d14_dif ~ c_dif_norm * land_cover, r1.a)) # grass sig
summary(lm(d14_dif ~ land_cover, r1.a)) # highly sig

ggplot(r1.a, aes(c_dif, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, linetype="dashed", size=1) +
  geom_vline(xintercept=0, linetype="dashed", size=1) +
  geom_smooth(method="lm") +
  geom_point(size=2) +
  geom_text(aes(label=OG.SN)) +
  scale_y_continuous(limits=c(-65,65)) +
  scale_color_manual(name = 'Landuse',
                     values =c('forest'='#a35513','grass'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab(expression("Dif. in C respired (mgC gC"*''^-1*") ")) +
  ylab(expression(Delta*''^14*'C dif. ')) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12, face="bold"),
        axis.text = element_text(size=10, face="bold", color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))
summary(lm(d14_dif ~ c_dif * land_cover, r1.a))
filter(r1.a, land_cover=="grass") %>% lm(d14_dif ~ c_dif, .) %>% summary
filter(r1.a, land_cover=="forest") %>% lm(d14_dif ~ c_dif, .) %>% summary

# plot residuals of 14C regression against years archived
r1.a.avg <- group_by(r1.a, OG.SN) %>% summarise_all(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
r1.a.avg$fm_dif <- r1.a.avg$fm_2-r1.a.avg$fm_1
r1.a.avg.filtered <- filter(r1.a.avg, !is.na(d14_1)) %>% filter(., !is.na(d14_2_corr))
r1.a.avg.filtered$d14_res <- summary(lm(d14_2_corr~d14_1,r1.a.avg.filtered))$residuals
summary(lm(d14_res~years_arc, r1.a.avg.filtered))
ggplot(r1.a.avg.filtered, aes(years_arc, d14_res, color = land_cover)) +
  geom_hline(yintercept=0, size=1.2) +
  geom_point(size=2) +
  scale_color_manual(name = 'land_cover',
                     values =c('forest'='#a35513','grass'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  xlab("Years archived") +
  ylab("Residuals") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10, face="bold", color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))

# new groupings
r1.a.avg2 <- r1.a.avg.filtered
r1.a.avg2$site2 <- ifelse(r1.a.avg$Site=="Germany" & r1.a.avg$land_cover=="forest", "Germany (forest)",
                          ifelse(r1.a.avg$Site=="Germany" & r1.a.avg$land_cover=="grass", "Germany (grass)",
                                 as.character(r1.a.avg$Site)))
r1.a.mean <- group_by(r1.a.avg2, site2) %>% summarise_all(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
r1.a.sd <- group_by(r1.a.avg2, site2) %>% summarise_all(funs(if(is.numeric(.)) sd(., na.rm = TRUE) else first(.)))
r1.a.mean$upper <- r1.a.mean$d14_dif+(r1.a.sd$d14_dif/sqrt(table(r1.a.avg2$site2)))
r1.a.mean$lower <- r1.a.mean$d14_dif-(r1.a.sd$d14_dif/sqrt(table(r1.a.avg2$site2)))
ggplot(r1.a.mean, aes(years_arc, d14_dif)) +
  geom_hline(yintercept=0, size=1.2) +
  geom_point(aes(color=land_cover), size=3) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=land_cover)) +
  scale_color_manual(name = "Landuse",
                     values =c('forest'='chocolate4','grass'='olivedrab4'),
                     labels = c('Forest','Grassland')) +
  xlab("Years archived") +
  ylab("d14C dif") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=18),
        axis.text = element_text(size=18, face="bold", color="black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18))

r1.a.avg.filtered$delta_hr <- r1.a.avg.filtered$mgCO2_C_gsoilC_day_t1-r1.a.avg.filtered$mgCO2_C_gsoilC_day_t2
ggplot(r1.a.avg.filtered, aes(delta_hr, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, color="gray", size=1) +
  geom_point(size=2) +
  scale_color_manual(name = 'Landuse',
                     values =c('forest'='chocolate4','grass'='olivedrab4'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid=element_blank())
r1.a.avg.filtered$delta_hr <- r1.a.avg.filtered$mgCO2_C_gsoilC_day_t1-r1.a.avg.filtered$mgCO2_C_gsoilC_day_t2
ggplot(r1.a.avg.filtered, aes(delta_hr, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, color="gray", size=1) +
  geom_point(size=2) +
  scale_color_manual(name = 'Landuse',
                     values =c('forest'='chocolate4','grass'='olivedrab4'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid=element_blank())

r1.a.avg.filtered$mean_hr_t1_t2 <- rowMeans(r1.a.avg.filtered[,c("mgCO2_C_gsoilC_day_t1","mgCO2_C_gsoilC_day_t2")], na.rm=T)
ggplot(r1.a.avg.filtered, aes(mean_hr_t1_t2, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, color="gray", size=1) +
  geom_point(size=2) +
  scale_color_manual(name = 'Landuse',
                     values =c('forest'='chocolate4','grass'='olivedrab4'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid=element_blank())

ggplot(r1.a, aes(mgCO2_C_gsoilC_day_t2, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, color="gray", size=1) +
  geom_vline(xintercept=0, color="gray", size=1) +
  geom_point(size=2) +
  scale_color_manual(name = 'Landuse',
                     values =c('forest'='chocolate4','grass'='olivedrab4'),
                     labels = c('Forest','Grassland')) +
  xlab(expression("HR rate (mgC gC"*''^-1*" day"*''^-1*")")) +
  ylab(expression(Delta*''^14*'C dif. ')) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12, face="bold"),
        axis.text = element_text(size=10, face="bold", color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))

r1.a$hr.rate.dif <- r1.a$mgCO2_C_gsoilC_day_t1-r1.a$mgCO2_C_gsoilC_day_t2
ggplot(r1.a, aes(hr.rate.dif, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, color="gray", size=1) +
  geom_vline(xintercept=0, color="gray", size=1) +
  geom_point(size=2) +
  scale_color_manual(name = 'Landuse',
                     values =c('forest'='chocolate4','grass'='olivedrab4'),
                     labels = c('Forest','Grassland')) +
  xlab(expression("HR rate dif (mgC gC"*''^-1*" day"*''^-1*")")) +
  ylab(expression(Delta*''^14*'C dif. ')) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12, face="bold"),
        axis.text = element_text(size=10, face="bold", color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))

# plot w/ error bars
# Calulate SE
r1.a_p1 <- group_by(r1.a, OG.SN) %>% summarise_all(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
r1.a_p1 <- r1.a_p1[,c("SampleName","Round","land_cover","d14_dif","delta_hr")]
r1.a_p1$d14_dif_top <- tapply(r1.a$d14_dif, r1.a$OG.SN, max)
r1.a_p1$d14_dif_bot <- tapply(r1.a$d14_dif, r1.a$OG.SN, min)
r1.a_p1$delta_hr_top <- tapply(r1.a$delta_hr, r1.a$OG.SN, max)
r1.a_p1$delta_hr_bot <- tapply(r1.a$delta_hr, r1.a$OG.SN, min)

r1.a_p1 %>% filter(Round == 2) %>%
  ggplot(., aes(delta_hr, d14_dif, color=land_cover)) +
  geom_hline(yintercept=0, color="gray", size=1) +
  geom_vline(xintercept=0, color="gray", size=1) +
  geom_smooth(method="lm") +
  # geom_smooth(method="lm", mapping = aes(weight = wts), color="black") +  # switch to wtd regression
  geom_point(size=3) +
  geom_errorbar(aes(ymin=d14_dif_bot, ymax=d14_dif_top, color=land_cover), width=.4) +
  geom_errorbarh(aes(xmin=delta_hr_bot, xmax=delta_hr_top, color=land_cover), height=5) +
  scale_color_manual(name = 'Landuse',
                     values =c('forest'='chocolate4','grass'='olivedrab4'),
                     labels = c('Forest','Grassland')) +
  xlab(expression("Delta HR rate (mgC gC"*''^-1*" day"*''^-1*")")) +
  ylab(expression(Delta*''^14*'C dif. ')) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=18, face="bold", color="black"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=18))
summary(lm(d14_dif~delta_hr,r1.a_p1[r1.a_p1$Round==2,]))
summary(lm(d14_dif~delta_hr+land_cover,r1.a_p1[r1.a_p1$Round==2,])) # delta hr effect driven by landuse
# experimental weighted regression
r1.a_p2 <- group_by(r1.a, OG.SN) %>% summarise_all(funs(if(is.numeric(.)) var(., na.rm = TRUE) else first(.)))
r1.a_p1$d14_dif_var <- r1.a_p2$d14_dif
r1.a_p1$wts <- 1/r1.a_p1$d14_dif_var
summary(lm(d14_dif~delta_hr, r1.a_p1, weights = 1/r1.a_p1$d14_dif_var)) # pretty similar fit

#####


# Calculate % of initial C respired
#####
r3.iso$pct_C_HR <- r3.iso$CO2_mg_gC_cmtv
max(r3.iso$CO2_mg_gC_cmtv, na.rm=T)
r3.iso[,c("SampleName","Period","Time","CO2_mg_gC_cmtv")]

# airdry + storage
#####
# load file
arcInc <- read.csv("/Users/jeff/R/14Constraint/arc_inc_iso_06-02-2020.csv")
str(arcInc)

# change "d14C_CO2_corr" to "d14C"
colnames(arcInc)[which(names(arcInc) == "d14C_CO2_corr")] <- "d14C"

# subset pre/inc data
arcInc.pre <- arcInc[arcInc$Period == "pre", ]
arcInc.inc <- arcInc[arcInc$Period == "inc", ]
# subset t1/t2 data
arcInc.t1 <- arcInc[arcInc$Time == "t1", ]
arcInc.t2 <- arcInc[arcInc$Time == "t2", ]

# change inc C-respired data to total C respired
arcInc.t2[arcInc.t2$Period == "inc", "mgCO2.C_permilleSoilC"] <- 
  arcInc.t2[arcInc.t2$Period == "inc", "mgCO2.C_permilleSoilC"] +
  arcInc.t2[arcInc.t2$Period == "pre", "mgCO2.C_permilleSoilC"]
# subset t2 data and summarize
# add sum C resp and land_cover
arcInc.t2.sum <- select(arcInc.t2, d14C, mgCO2.C_permilleSoilC, ID, Period)  %>%
  group_by(Period, ID) %>%
  summarise_all(funs(mean, min, max), na.rm = TRUE)
arcInc.t2.sum <- arcInc.t2.sum[complete.cases(arcInc.t2.sum), ]
arcInc.t2.sum$treat.Period <- paste("t2", arcInc.t2.sum$Period)

# repeat w/ t1 data to rbind
arcInc.t1[arcInc.t1$Period == "inc", "mgCO2.C_permilleSoilC"] <- 
  arcInc.t1[arcInc.t1$Period == "inc", "mgCO2.C_permilleSoilC"] +
  arcInc.t1[arcInc.t1$Period == "pre", "mgCO2.C_permilleSoilC"]
arcInc.t1.sum <- select(arcInc.t1, d14C, mgCO2.C_permilleSoilC, ID, Period)  %>%
  group_by(Period, ID) %>%
  summarise_all(funs(mean, min, max), na.rm = TRUE)
arcInc.t1.sum <- arcInc.t1.sum[complete.cases(arcInc.t1.sum), ]
arcInc.t1.sum$treat.Period <- paste("t1", arcInc.t1.sum$Period)
arcInc.sum2 <- as.data.frame(rbind(arcInc.t1.sum, arcInc.t2.sum))
arcInc.sum2$land_cover <- ifelse(grepl("G", arcInc.sum2$ID), "G", "F")
arcInc.sum2$treat.Period <- factor(arcInc.sum2$treat.Period, levels = c("t1 inc", "t2 pre", "t2 inc"))
# sort
arcInc.sum2 <- arcInc.sum2[order(arcInc.sum2$treat.Period), ]


# calculate differences (14C, CO2-C)
# Notes: t1 data (n=12) recycle to length of t2 data
arcInc.inc.d <- data.frame(
  ID = unique(arcInc.inc$ID), 
  d14C_inc_dif = arcInc.inc[arcInc.inc$Time == "t2", "d14C"] - 
    arcInc.inc[arcInc.inc$Time == "t1", "d14C"],
  d14C_pre_dif = arcInc.pre[arcInc.pre$Time == "t2", "d14C"] - 
    arcInc.pre[arcInc.pre$Time == "t1", "d14C"],
  C_inc_dif = arcInc.inc[arcInc.inc$Time == "t2", "mgCO2.C"] - 
    arcInc.inc[arcInc.inc$Time == "t1", "mgCO2.C"],
  C_inc_pml_dif = arcInc.inc[arcInc.inc$Time == "t2", "mgCO2.C_permilleSoilC"] - 
    arcInc.inc[arcInc.inc$Time == "t1", "mgCO2.C_permilleSoilC"],
  C_pre_dif = arcInc.pre[arcInc.pre$Time == "t2", "mgCO2.C"] - 
    arcInc.pre[arcInc.pre$Time == "t1", "mgCO2.C"],
  C_pre_pml_dif = arcInc.pre[arcInc.pre$Time == "t2", "mgCO2.C_permilleSoilC"] - 
    arcInc.pre[arcInc.pre$Time == "t1", "mgCO2.C_permilleSoilC"]
  )
arcInc.inc.d$C_sum_dif <- arcInc.inc.d$C_inc_dif + arcInc.inc.d$C_pre_dif
arcInc.inc.d$C_pml_dif <- arcInc.inc.d$C_inc_pml_dif + arcInc.inc.d$C_pre_pml_dif

# mean, min, max
arcInc.sum <- group_by(arcInc.inc.d, ID) %>%
  summarise_all(funs(mean, min, max), na.rm = TRUE)
arcInc.sum$land_cover <- ifelse(grepl("G", arcInc.sum$ID), "G", "F")
#####

# airdry
#####
rewet <- read.csv("/Users/jeff/R/14Constraint/rewet_14c_13-02-2020.csv")
str(rewet)

# subset pre/inc data
rewet.pre <- rewet[rewet$Period == "pre", ]
rewet.inc <- rewet[rewet$Period == "inc", ]
# subset treat data (t1 = moist, t2 = dry)
rewet.t1 <- rewet[rewet$treat == "moist", ]
rewet.t2 <- rewet[rewet$treat == "dry", ]

# change inc C-respired data to total C respired
rewet.t2[rewet.t2$Period == "inc", "mgCO2.C_permilleSoilC"] <- 
  rewet.t2[rewet.t2$Period == "inc", "mgCO2.C_permilleSoilC"] +
  rewet.t2[rewet.t2$Period == "pre", "mgCO2.C_permilleSoilC"]
# subset t2 data and summarize
# add sum C resp and land_cover
rewet.t2.sum <- select(rewet.t2, d14C, mgCO2.C_permilleSoilC, OG.SN, Period)  %>%
  group_by(Period, OG.SN) %>%
  summarise_all(funs(mean, min, max), na.rm = TRUE)
rewet.t2.sum$treat.Period <- paste("t2", rewet.t2.sum$Period)

# repeat w/ t1 data to rbind
rewet.t1[rewet.t1$Period == "inc", "mgCO2.C_permilleSoilC"] <- 
  rewet.t1[rewet.t1$Period == "inc", "mgCO2.C_permilleSoilC"] +
  rewet.t1[rewet.t1$Period == "pre", "mgCO2.C_permilleSoilC"]
rewet.t1.sum <- select(rewet.t1, d14C, mgCO2.C_permilleSoilC, OG.SN, Period)  %>%
  group_by(Period, OG.SN) %>%
  summarise_all(funs(mean, min, max), na.rm = TRUE)
rewet.t1.sum$treat.Period <- paste("t1", rewet.t1.sum$Period)
rewet.sum3 <- as.data.frame(rbind(rewet.t1.sum, rewet.t2.sum))
rewet.sum3$land_cover <- ifelse(grepl("G", rewet.sum3$OG.SN), "G", "F")
rewet.sum3$treat.Period <- factor(rewet.sum3$treat.Period, levels = c("t1 pre", "t1 inc", "t2 pre", "t2 inc"))
# sort
rewet.sum3 <- rewet.sum3[order(rewet.sum3$treat.Period), ]
# rename "OG.SN" to "ID"
colnames(rewet.sum3)[which(names(rewet.sum3) == "OG.SN")] <- "ID"

# calculate differences (14C, CO2-C)
# Notes: t1 data (n=12) recycle to length of t2 data
rewet.inc.d <- data.frame(
  ID = unique(rewet.inc$OG.SN), 
  d14C_inc_dif = rewet.inc[rewet.inc$treat == "dry", "d14C"] - 
    rewet.inc[rewet.inc$treat == "moist", "d14C"],
  d14C_pre_dif = rewet.pre[rewet.pre$treat == "dry", "d14C"] - 
    rewet.pre[rewet.pre$treat == "moist", "d14C"],
  C_inc_dif = rewet.inc[rewet.inc$treat == "dry", "mgCO2.C"] - 
    rewet.inc[rewet.inc$treat == "moist", "mgCO2.C"],
  C_inc_pml_dif = rewet.inc[rewet.inc$treat == "dry", "mgCO2.C_permilleSoilC"] - 
    rewet.inc[rewet.inc$treat == "moist", "mgCO2.C_permilleSoilC"],
  C_pre_dif = rewet.pre[rewet.pre$treat == "dry", "mgCO2.C"] - 
    rewet.pre[rewet.pre$treat == "moist", "mgCO2.C"],
  C_pre_pml_dif = rewet.pre[rewet.pre$treat == "dry", "mgCO2.C_permilleSoilC"] - 
    rewet.pre[rewet.pre$treat == "moist", "mgCO2.C_permilleSoilC"]
)
rewet.inc.d$C_sum_dif <- rewet.inc.d$C_inc_dif + rewet.inc.d$C_pre_dif
rewet.inc.d$C_pml_dif <- rewet.inc.d$C_inc_pml_dif + rewet.inc.d$C_pre_pml_dif

# mean, min, max
rewet.sum <- group_by(rewet.inc.d, ID) %>%
  summarise_all(funs(mean, min, max), na.rm = TRUE)
# add landcover factor
rewet.sum$land_cover <- ifelse(grepl("G", rewet.sum$ID), "G", "F")
#####

# combine data
#####
all.sum <- rbind(arcInc.sum, rewet.sum)
all.sum$exp <- c(rep("arcInc", nrow(arcInc.sum)),
                 rep("rewet", nrow(rewet.sum)))

# Plots
#####
# total C respired difference
ggplot(arcInc.sum, aes(C_sum_dif_mean, d14C_inc_dif_mean, color = land_cover)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# incubation C respired difference
ggplot(arcInc.sum, aes(C_inc_dif_mean, d14C_inc_dif_mean, color = land_cover)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Fraction C respired difference, arcInc
ggplot(arcInc.sum, aes(C_pml_dif_mean, d14C_inc_dif_mean, color = land_cover)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = d14C_inc_dif_min, 
        ymax = d14C_inc_dif_max, 
        color = land_cover), 
    width = .6) +
  geom_errorbarh(
    aes(xmin = C_pml_dif_min, 
        xmax = C_pml_dif_max, 
        color = land_cover), 
    height = 3) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_x_continuous(limits = c(-8,15)) +
  scale_y_continuous(limits = c(-25,60)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# absolute 14C over time
arcInc.sum2 %>%
  filter(treat.Period != "t2 pre") %>%
  ggplot(., aes(mgCO2.C_permilleSoilC_mean, d14C_mean, color = land_cover, shape = treat.Period)) +
  geom_point(size = 3) +
  geom_path(aes(group = ID)) +
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .25) +
  geom_errorbarh(
    aes(xmin = mgCO2.C_permilleSoilC_min, 
        xmax = mgCO2.C_permilleSoilC_max, 
        color = land_cover), 
    height = .9) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Treatment (year incubated)',
                     values =c(16, 1),
                     labels = c('control (2011)', 'air-dry + storage (2018)')) +
  # geom_text(aes(label=ID), angle=45, size=3) +
  facet_grid(cols = vars(land_cover)) +
  xlab(expression('Respired C (mgCO'[2]*'-C'*' g soil C'^-1*')')) +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# pre vs. main inc
arcInc.sum2$treat.Period
arcInc.sum2 %>%
  filter(treat.Period != "t1 inc") %>%
  ggplot(., aes(mgCO2.C_permilleSoilC_mean, d14C_mean, color = land_cover, shape = treat.Period)) +
  geom_point(size = 3) +
  geom_path(aes(group = ID)) +
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .25) +
  geom_errorbarh(
    aes(xmin = mgCO2.C_permilleSoilC_min, 
        xmax = mgCO2.C_permilleSoilC_max, 
        color = land_cover), 
    height = .9) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Sampling period',
                     values =c("t2 pre" = 2, "t2 inc" = 1),
                     labels = c('equilibrium respiration (air-dry + storage)'), 'pre-incubation (air-dry + storage)' ) +
  facet_grid(cols = vars(land_cover)) +
  xlab(expression('Respired C (mgCO'[2]*'-C'*' g soil C'^-1*')')) +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# absolute 14C over time, rewet 
# NB: I don't think this should be shown, as mixes treatment effect w/ C respired effect
rewet.sum3 %>%
  filter(treat.Period != "t2 pre") %>%
  filter(treat.Period != "t2 inc") %>%
  ggplot(., aes(mgCO2.C_permilleSoilC_mean, d14C_mean, color = land_cover, shape = treat.Period)) +
  geom_point(size = 3) +
  geom_path(aes(group = ID), linetype = "dashed", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .25) +
  geom_errorbarh(
    aes(xmin = mgCO2.C_permilleSoilC_min, 
        xmax = mgCO2.C_permilleSoilC_max, 
        color = land_cover), 
    height = .9) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Treatment (year incubated)',
                     values =c(16, 1),
                     labels = c('control (2019)', 'air-dry (2019)')) +
  facet_grid(cols = vars(land_cover)) +
  xlab(expression('Respired C (mgCO'[2]*'-C'*' g soil C'^-1*')')) +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# pre vs. main inc
rewet.sum3 %>%
  mutate(ID.treat.Period = paste(ID, substr(treat.Period, 1, 2))) %>%
  ggplot(., aes(mgCO2.C_permilleSoilC_mean, d14C_mean, color = land_cover, shape = treat.Period)) +
  geom_point(size = 3) +
  geom_path(aes(group = ID.treat.Period)) +
  # geom_text(aes(label = ID)) + # shows that the divergent grassland sample is HEG10
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .25) +
  geom_errorbarh(
    aes(xmin = mgCO2.C_permilleSoilC_min, 
        xmax = mgCO2.C_permilleSoilC_max, 
        color = land_cover), 
    height = .9) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Sampling period (treatment)',
                     values =c(16, 17, 1, 2),
                     labels = c('pre-incubation\n(control)', 
                                'equilibrium respiration\n(control)',
                                'pre-incubation\n(air-dry/air-dry + storage)',
                                'equilibrium respiration\n(air-dry/air-dry + storage)')) +
  facet_grid(cols = vars(land_cover)) +
  xlab(expression('Respired C (mgCO'[2]*'-C'*' g soil C'^-1*')')) +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.height=unit(.8, "cm"))

# combine rewet and arc inc data to show pre-inc/equil data together
rewet.sum3$year <- 2019
arcInc.sum2$year <- 2011
all.rewet.arcinc <- rbind(rewet.sum3, arcInc.sum2)
all.rewet.arcinc <- all.rewet.arcinc[-which(all.rewet.arcinc$year == 2011 & all.rewet.arcinc$treat.Period == "t1 inc"), ]
all.rewet.arcinc <- all.rewet.arcinc[-which(all.rewet.arcinc$year == 2019 & all.rewet.arcinc$ID == "HEW22"), ]

all.rewet.arcinc %>%
  mutate(ID.treat.Period = paste(ID, substr(treat.Period, 1, 2))) %>%
  ggplot(., aes(mgCO2.C_permilleSoilC_mean, d14C_mean, color = land_cover, shape = treat.Period)) +
  geom_point(size = 3) +
  geom_path(aes(group = ID.treat.Period)) +
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .25) +
  geom_errorbarh(
    aes(xmin = mgCO2.C_permilleSoilC_min, 
        xmax = mgCO2.C_permilleSoilC_max, 
        color = land_cover), 
    height = .9) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Sampling period (treatment)',
                     values =c("t1 pre" = 17, 
                               "t1 inc" = 16, 
                               "t2 pre" = 2, 
                               "t2 inc" = 1),
                     labels = c("t1 pre" = 'pre-incubation\n(control)', 
                                "t1 inc" = 'equilibrium respiration\n(control)',
                                "t2 pre" = 'pre-incubation\n(air-dry/air-dry + storage)',
                                "t2 inc" = 'equilibrium respiration\n(air-dry/air-dry + storage)')) +
  facet_grid(cols = vars(land_cover), rows = vars(year)) +
  xlab(expression('Respired C (mgCO'[2]*'-C'*' g soil C'^-1*')')) +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.height=unit(.8, "cm"))

# Fraction C respired difference, rewet
# reshape rewet to add period as column
rewet.sum2 <- data.frame(ID = rep(rewet.sum$ID, 2),
                         Period = rep(c("pre-incubation", "equilibrium"), each = nrow(rewet.sum)),
                         land_cover = rep(rewet.sum$land_cover, 2),
                         d14C_dif_mean = c(rewet.sum$d14C_pre_dif_mean, rewet.sum$d14C_inc_dif_mean),
                         C_dif_mean = c(rewet.sum$C_pre_dif_mean, rewet.sum$C_sum_dif_mean),
                         C_pml_dif_mean = c(rewet.sum$C_pre_pml_dif_mean, rewet.sum$C_pml_dif_mean),
                         d14C_dif_min = c(rewet.sum$d14C_pre_dif_min, rewet.sum$d14C_inc_dif_min),
                         C_pml_dif_min = c(rewet.sum$C_pre_pml_dif_min, rewet.sum$C_pml_dif_min),
                         d14C_dif_max = c(rewet.sum$d14C_pre_dif_max, rewet.sum$d14C_inc_dif_max),
                         C_pml_dif_max = c(rewet.sum$C_pre_pml_dif_max, rewet.sum$C_pml_dif_max)
                         )
ggplot(rewet.sum2, aes(C_pml_dif_mean, d14C_dif_mean, color = land_cover, shape = Period)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = d14C_dif_min, 
        ymax = d14C_dif_max, 
        color = land_cover), 
    width = .6) +
  geom_errorbarh(
    aes(xmin = C_pml_dif_min, 
        xmax = C_pml_dif_max, 
        color = land_cover), 
    height = 3) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Period',
                     values = c(16,1),
                     labels = c('equilibrium','pre-incubation')) +
  scale_x_continuous(limits = c(-8,15)) +
  scale_y_continuous(limits = c(-25,60)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# inc only
ggplot(rewet.sum, aes(C_pml_dif_mean, d14C_inc_dif_mean, color = land_cover)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = d14C_inc_dif_min, 
        ymax = d14C_inc_dif_max, 
        color = land_cover), 
    width = .6) +
  geom_errorbarh(
    aes(xmin = C_pml_dif_min, 
        xmax = C_pml_dif_max, 
        color = land_cover), 
    height = 3) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_x_continuous(limits = c(-8,15)) +
  scale_y_continuous(limits = c(-25,60)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Fraction C respired difference, all (I don't think it makes sense to lump across treatment)
ggplot(all.sum, aes(C_pml_dif_mean, d14C_dif_mean, color = land_cover)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(aes(shape = exp), size = 3) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_x_continuous(limits = c(-8,15)) +
  scale_y_continuous(limits = c(-25,55)) +
  theme_bw() +
  theme(panel.grid = element_blank())


#####

# Statistics
#####
# arcInc
arcInc.inc.d.g <- arcInc.inc.d[grep("G", arcInc.inc.d$ID), ] # grasslands
arcInc.inc.d.f <- arcInc.inc.d[grep("W", arcInc.inc.d$ID), ] # forests
summary(lm(d14C_dif ~ C_pml_dif, arcInc.inc.d.g))
summary(lm(d14C_dif ~ C_pml_dif, arcInc.inc.d.f))

# rewet
rewet.inc.d.g <- rewet.inc.d[grep("G", rewet.inc.d$ID), ] # grasslands
rewet.inc.d.f <- rewet.inc.d[grep("W", rewet.inc.d$ID), ] # forests
summary(lm(d14C_dif ~ C_pml_dif, rewet.inc.d.g))
summary(lm(d14C_dif ~ C_pml_dif, rewet.inc.d.f))

summary(lm(d14C_dif ~ C_pml_dif, rewet.inc.d)) # R2 = 0.44; C_pml_dif p = 0.011

# all
all.inc.d <- rbind(arcInc.inc.d, rewet.inc.d)
all.inc.d$exp <- c(rep("arcInc", nrow(arcInc.inc.d)),
                 rep("rewet", nrow(rewet.inc.d)))

all.inc.d.g <- all.inc.d[grep("G", all.inc.d$ID), ] # grasslands
all.inc.d.f <- all.inc.d[grep("W", all.inc.d$ID), ] # forests
summary(lm(d14C_dif ~ C_pml_dif, all.inc.d.g))
summary(lm(d14C_dif ~ C_pml_dif, all.inc.d.f)) # R2 = 0.43; C_pml_dif p = 0.002

summary(lm(d14C_dif ~ C_pml_dif, all.inc.d))

#####

# Model
#####
# testing forest vs grassland turnover
library(SoilR)
library(ISRaD)

# look up data in ISRaD from Germany

# initial parameter set
Datm <- rbind(graven, future14C)
Datm <- Datm[Datm$Date > 1900, c("Date", "NHc14")]
yrs <- Datm$Date

# helper fxs
lambda <- (1/8267)

k <- function(Fm) {
  (Fm*lambda)/(1-Fm)
}
fm <- function(k){
  k/(k+lambda)
}
fm_14c <- function(fm, date) {
  (fm*exp(lambda*(1950 - date)) - 1)*1000
}

# forests
f.c <- 38.96104 # forest C pool, total (mean Hainich forest C  = 24 g kg^-1); nb @ ss = sum(-1*solve(A)%*%c(f.in*f.gam, f.in*(1-f.gam)))
f.in <- 1 # inputs (12.5=mean equilibrium C flux, annual extrapolation, gC yr-1, 2011 incubation data); try much less...
f.frc <- .1 # C-stock partitioning coefficient (from Schrumpf 2013, Hainich 0-5, free light)
f.Cfast <- f.frc*f.c
f.Cslow <- f.c-f.Cfast
f.kfast <- 1/6 # unknown
f.kslow <- 1/100 # unknown
f.fast.fm <- fm(f.kfast)
f.slow.fm <- fm(f.kslow)
f.F0_Delta14C <- fm_14c(c(f.fast.fm, f.slow.fm), date = c(1900, 1900))
f.gam <- (f.c*f.Cfast*f.kfast)/(f.c*f.Cfast*f.kfast+f.c*f.Cslow*f.kslow) # input partitioning coefficient (proportion to fast pool, function of ks)
  
# grasslands
g.c <- 42 # C pool, total (mean Hainich grassland C g kg^-1)
g.in <- 63.4 # inputs (mean equilibrium C flux, annual extrapolation, 2011 incubation data)
g.frc <- .3 # C-stock partitioning coefficient (from Schrumpf 2013, average of Lacqueville, Easter Bush, Bugac, 0-5, free light)
g.Cfast <- g.frc*g.c
g.Cslow <- g.c-g.Cfast
g.kfast <- 1/10 # unknown
g.kslow <- 1/100 # unknown
g.fast.fm <- fm(g.kfast)
g.slow.fm <- fm(g.kslow)
g.F0_Delta14C <- fm_14c(c(g.fast.fm, g.slow.fm), date = c(1900, 1900))
g.gam <- (g.c*g.Cfast*g.kfast)/(g.c*g.Cfast*g.kfast+g.c*g.Cslow*g.kslow) # input partitioning coefficient (proportion to fast pool, function of ks)

# 2pool parallel model
f.2p <- TwopParallelModel14(t = yrs,
                            ks = c(f.kfast, f.kslow),
                            C0 = c(f.Cfast, f.Cslow),
                            F0_Delta14C = f.F0_Delta14C,
                            In = f.in,
                            gam = f.gam,
                            inputFc = Datm)

f.2p.C14 <- getF14(f.2p)
f.2p.HR <- getF14R(f.2p)
f.2p.Ctot <- getC(f.2p)
data.frame(f.2p.HR, f.2p.Ctot)

# solve for steady-state C stocks
A <- -1*diag(c(f.kfast, f.kslow))
sum(-1*solve(A)%*%c(f.in*f.gam, f.in*(1-f.gam)))

f.2p.C14.df <- data.frame(
  years = rep(Datm$Date, 4),
  d14C = c(f.2p.C14[,1], f.2p.C14[,2], f.2p.HR, Datm$NHc14),
  pool = rep(c("fast", "slow", "respiration", "atm"), each = nrow(f.2p.C14))
  )

plot(yrs, xlim = c(1900,2022), ylim = c(0,60))
lines(yrs, f.2p.Ctot[, 1], col = 2)
lines(yrs, f.2p.Ctot[, 2], col = 4)

ggplot(f.2p.C14.df, aes(years, d14C, color = pool)) +
  geom_path() +
  geom_point(aes(x = 1991, y = 218.3264), color = "#FFC107", size = 2, shape = 16) +
  geom_point(aes(x = 2019, y = 75.65284), color = "#FFC107", size = 2, shape = 16) +
  scale_color_manual(
    values = c("atm" = 8,
               "fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5")) +
  # scale_y_continuous(limits = c(0, 220)) +
  scale_x_continuous(limits = c(1950, 2022)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# zoom (w/ atm, to 2022)
par(mar = c(5.1,4.1,4.1,3)) # bltr
mean(f.2p.C14.df[f.2p.C14.df$years > 1990 & f.2p.C14.df$years < 1992 & f.2p.C14.df$pool == "respiration", "d14C"])
mean(f.2p.C14.df[f.2p.C14.df$years > 2018 & f.2p.C14.df$years < 2020 & f.2p.C14.df$pool == "respiration", "d14C"])
f.2p.C14.df %>% 
  filter(years > 1987) %>%
  filter(pool != "atm") %>%
  ggplot(., aes(years, d14C, color = pool)) +
  geom_path() +
  geom_point(aes(x = 1991, y = 218.3264), color = "#FFC107", size = 2, shape = 16) +
  geom_point(aes(x = 2019, y = 75.65284), color = "#FFC107", size = 2, shape = 16) +
  scale_color_manual(
    values = c("fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5")) +
  scale_y_continuous(limits = c(0, 265)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(b = 2, l = 2, t = 1, r = 10))
#####

# Fit second model through same respiration curve
#####
modFun_2pp <- function(pars){
  mod <- TwopParallelModel14(
    t = yrs,
    ks = pars[1:2],
    C0 = c(f.Cfast, f.Cslow),
    F0_Delta14C = f.F0_Delta14C,
    In = f.in,
    gam = pars[3],
    inputFc = Datm)
  C14r = getF14R(mod)
  Ctot = getC(mod)
  return(
    data.frame(time = 2019.5, 
               hrt = C14r[120], 
               Ctot.1 = Ctot[120, 1], 
               Ctot.2 = Ctot[120, 2]))
}

# cost fx (evaluates error as model vs. obsv, per FME req)
f.2p.C14.df[f.2p.C14.df$years == 2019.5 & f.2p.C14.df$pool == "respiration", "d14C"]
which(f.2p.C14.df$d14C == f.2p.C14.df[f.2p.C14.df$years == 2019.5 & f.2p.C14.df$pool == "respiration", "d14C"])
obs <- data.frame(time = 2019.5,
                  hrt = f.2p.C14.df[364, "d14C"],
                  Ctot.1 = f.2p.Ctot[120, 1],
                  Ctot.2 = f.2p.Ctot[120, 2])
modCost_2pp <- function(pars){
  modCost(model = modFun_2pp(pars), obs = obs) # 2019 resp "obs"
}

# set initial parameters
inipars <- c(
  kfast = f.kfast,
  kslow = f.kslow,
  gam = f.gam
)

# optimize model pars
fit_2pp <- modFit(f = modCost_2pp, 
                  p = inipars, 
                  method = "Nelder-Mead",
                  upper = c(1,.1,1),
                  lower = c(.1,0,0)) 
fit_2pp$par
inipars # kfast much faster, kslow slower, gam much lower
fit_2pp$var_ms_unweighted

#####
# Parameter optimization: FB
#####
# Model iterations and how many to exclude (burn-in)
niter <- 2000 # start low (time consuming)
burnin <- 200

start <- Sys.time()
bayes_fit_2pp <- modMCMC(f = modCost_2pp, 
                         p = fit_2pp$par, 
                         var0 = fit_2pp$var_ms,
                         upper = c(1,.1,1),
                         lower = c(.1,0,0),
                         niter=niter, 
                         burninlength=burnin)
(time <- Sys.time()-start)
bayes_fit_2pp$bestpar 
bayes_fit_2pp$pars
# choose a random parameter set
sample(seq(1,1800),10)
bayes_fit_2pp$pars[sample(seq(1,1800),10),]
bayesFacts <- data.frame(niter, burnin, bayes_fit_2pp$naccepted)
colnames(bayesFacts) <- c('# Iterations', "# Burn-In", "# Accepted")
# textplot(bayesFacts)
bayesFacts

#Check that parameters have stabilized after burnin (nope)
plot(bayes_fit_2pp)
pairs(bayes_fit_2pp)

# summary
round(summary(bayes_fit_2pp)[,c(1:5)],4)

# still figuring out how to interpret this; since I lack priors maybe not so useful?
# Estimate parameter sensitivity and return timeseries distribution envelope
pred_uncert_2pp <- sensRange(modFun_2pp, parInput = bayes_fit_2pp$pars)
tail(summary(pred_uncert_2pp))

plot(summary(pred_uncert_2pp), which = 1, main = 'HR 14C', xlab = 'Year', ylab = expression(paste(Delta^"14","C")),
     xlim = c(1950, 2021.5), ylim = c(-10, 200))
points(f.2p.C14.df[364, "years"],  f.2p.C14.df[364, "d14C"], pch = 15, col=6)


# grasslands
g.2p <- TwopParallelModel14(t = yrs,
                            ks = c(g.kfast, g.kslow),
                            C0 = c(g.Cfast, g.Cslow),
                            F0_Delta14C = g.F0_Delta14C,
                            In = g.in,
                            gam = g.gam,
                            inputFc = Datm)

g.2p.C14 <- getF14(g.2p)
g.2p.HR <- getF14R(g.2p)

g.2p.C14.df <- data.frame(
  years = rep(Datm$Date, 4),
  d14C = c(g.2p.C14[,1], g.2p.C14[,2], g.2p.HR, Datm$NHc14),
  pool = rep(c("fast", "slow", "respiration", "atm"), each = nrow(g.2p.C14))
)

mean(g.2p.C14.df[g.2p.C14.df$years > 2010 & g.2p.C14.df$years < 2012 & g.2p.C14.df$pool == "respiration", "d14C"])
ggplot(g.2p.C14.df, aes(years, d14C, color = pool, linetype = pool)) +
  geom_path() +
  geom_point(aes(x = 2011, y = 99.7), color = 6, size = 3, shape = 8) +
  scale_linetype_manual(
    values = c("atm" = "solid",
               "fast" = "solid", 
               "respiration" = "dashed", 
               "slow" = "solid")) +
  scale_color_manual(values = c(8, 2, 6, 4)) +
  scale_y_continuous(limits = c(0, 220)) +
  scale_x_continuous(limits = c(1995, 2016)) +
  theme_bw() +
  theme(panel.grid = element_blank())


#####

# Time series analysis
#####
# inc data, only resampled plots, moist vs. moist
rewet.IDs <- unique(rewet.sum3$ID)
ts.arcInc <- arcInc.sum2[arcInc.sum2$treat.Period == "t1 inc", ]
ts.arcInc <- ts.arcInc[match(rewet.IDs, ts.arcInc$ID), ] # t1.inc = ctl
ts.rewet <- rewet.sum3[rewet.sum3$treat.Period == "t1 inc", ] # t1.inc = ctl
time.series <- rbind(ts.arcInc, ts.rewet)
time.series$years <- c(rep(2011, nrow(ts.arcInc)), rep(2019, nrow(ts.rewet)))

# inc data, only resampled plots, air-dry vs air-dry
ts.arcInc.ad <- arcInc.sum2[arcInc.sum2$treat.Period == "t2 inc", ] # t2.inc = treat
ts.arcInc.ad <- ts.arcInc.ad[match(rewet.IDs, ts.arcInc.ad$ID), ]
ts.rewet.ad <- rewet.sum3[rewet.sum3$treat.Period == "t2 inc", ]
time.series.ad <- rbind(ts.arcInc.ad, ts.rewet.ad)
time.series.ad$years <- c(rep(2011, nrow(ts.arcInc.ad)), rep(2019, nrow(ts.rewet.ad)))

# combine
ts <- rbind(time.series, time.series.ad)
ts$treat <- ifelse(ts$treat.Period == "t1 inc", "control", "air-dry")
ts$treat <- factor(ts$treat, levels = c("control", "air-dry"))

# atm
atm <- Datm
atm$pool <- "atmosphere"

ggplot(atm, aes(Date, NHc14, color = pool)) +
  geom_path() +
  scale_color_manual(name = "",
                     values = c("atmosphere" = 8)) +
  scale_x_continuous(limits = c(1950, 2022)) +
  xlab("Year") +
  ylab(expression(Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Plot w/ atm
ggplot(time.series, aes(years, d14C_mean, color = land_cover)) +
  geom_point(size = 3) +
  geom_path(aes(group = ID)) +
  geom_path(data = Datm, aes(Date, NHc14), color = "darkgray") +
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .1) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_x_continuous(limits = c(1950, 2022)) +
  xlab("Year") +
  ylab(expression(Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# zoom
ggplot(time.series, aes(years, d14C_mean, color = land_cover)) +
  geom_point(size = 2.5) +
  geom_path(aes(group = ID)) +
  geom_path(data = Datm, aes(Date, NHc14), color = "darkgray") +
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .1) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_x_continuous(limits = c(2010, 2020), breaks = c(2011, 2013, 2015, 2017, 2019)) +
  scale_y_continuous(limits = c(-15, 100)) +
  xlab("Year") +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")

# air-dry soils vs. controls
ggplot(ts, aes(years, d14C_mean, color = land_cover)) +
  geom_point(size = 2.5) +
  geom_path(aes(group = ID)) +
  geom_path(data = Datm, aes(Date, NHc14), color = "darkgray") +
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .1) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_x_continuous(limits = c(2010, 2020), breaks = c(2011, 2013, 2015, 2017, 2019)) +
  scale_y_continuous(limits = c(-15, 100)) +
  xlab("Year") +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  facet_grid(cols = vars(treat)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Show treatment effects for one forest and one grassland sample
# NB: somehow the treatment IDs got switched...need to trace this (best not to use "t1"/"t2", but actual treatment labels)
ts$year.ID <- paste(ts$years, ts$ID)
ts %>% 
  filter(ID == "HEG32" | ID == "HEW41") %>%
  ggplot(., aes(years, d14C_mean, color = land_cover)) +
  geom_point(aes(shape = treat), size = 3) +
  geom_path(aes(group = year.ID), linetype = "dashed") +
  geom_path(data = Datm, aes(Date, NHc14), color = "darkgray") +
  geom_errorbar(
    aes(ymin = d14C_min, 
        ymax = d14C_max, 
        color = land_cover), 
    width = .2) +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Treatment',
                     values = c(1,16),
                     labels = c('air-dry (+ storage for 2011 points)', 'control')) +
  scale_x_continuous(limits = c(2010, 2020), breaks = c(2011, 2013, 2015, 2017, 2019)) +
  scale_y_continuous(limits = c(-15, 100)) +
  facet_grid(cols = vars(land_cover)) +
  xlab("Year") +
  ylab(expression('mean '*Delta*''^14*'C (‰)')) +
  facet_grid(cols = vars(land_cover)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# plot all samples, facet by ID/land_cover
ggplot(ts, aes(years, d14C_mean, color = land_cover)) +
  geom_point(aes(shape = treat), size = 3) +
  # geom_errorbar(
  #   aes(ymin = d14C_min, 
  #       ymax = d14C_max, 
  #       color = land_cover), 
  #   width = .2) +
  geom_path(aes(group = year.ID), 
            arrow = arrow(type = "open", length = unit(.1, "inches"), ends = "last"),
            show.legend = FALSE) +
  geom_path(data = Datm, aes(Date, NHc14), color = "darkgray") +
  scale_color_manual(name = 'Landuse',
                     values =c('F'='#a35513','G'='#1361a3'),
                     labels = c('Forest','Grassland')) +
  scale_shape_manual(name = 'Treatment',
                     values = c("air-dry" = 1, "control" = 16),
                     labels = c("air-dry" = 'air-dry (+ storage for 2011 points)', 
                                "control" = 'control')) +
  scale_x_continuous(limits = c(2010, 2020), breaks = c(2013, 2016, 2019)) +
  scale_y_continuous(limits = c(-15, 100)) +
  facet_wrap(~ ID, ncol = 3) +
  theme_bw() +
  theme(panel.grid = element_blank())
  

#####

# Vaughn data for comparison
#####
vgn <- read.csv("/Users/jeff/R/14Constraint/vgn_d14C_dC.csv")
str(vgn)

summary(lm(abs(delta_d14C) ~ pct_c_respired, vgn[vgn$inc_dur_d == 50,]))

summary(lm(delta_d14C ~ d14C, vgn[vgn$inc_dur_d == 50,]))
plot(lm(delta_d14C ~ d14C, vgn[vgn$inc_dur_d == 50 & vgn$delta_d14C > -80,]))
summary(lm(delta_d14C ~ d14C, vgn[vgn$inc_dur_d == 50 & vgn$delta_d14C > -80 & vgn$delta_d14C < 20,]))

vgn$d14C_int <- ifelse(vgn$inc_dur_d == 13, vgn$d14C, 
                       vgn[match(vgn[vgn$inc_dur_d == 13, "ID"], vgn[vgn$inc_dur_d == 50, "ID"]), "d14C"])

vgn %>%
  filter(inc_dur_d == 50) %>%
  ggplot(., aes(pct_c_respired, abs(delta_d14C))) +
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank())

vgn %>%
  filter(delta_d14C > -80) %>%
  filter(delta_d14C < 20) %>%  
  ggplot(., aes(delta_d14C, d14C)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank())

# calculate pct C respired @day 13 using total, assuming linear relationship (obv not true)
dC <- data.frame(pct_c_respired = vgn[vgn$inc_dur_d == 50, "delta_mgC_day"]*13,
                 ID = vgn[vgn$inc_dur_d == 50, "ID"])
vgn[vgn$inc_dur_d == 13, "pct_c_respired"] <- dC[match(vgn[vgn$inc_dur_d == 13, "ID"], dC$ID), "pct_c_respired"]

vgn %>%
  ggplot(., aes(pct_c_respired, d14C)) +
  geom_point() +
  geom_path(aes(group = ID)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# compare my data
rewet

#####

col2hex <- function(cname)
{
  colMat <- col2rgb(cname)
  rgb(
    red=colMat[1,]/255,
    green=colMat[2,]/255,
    blue=colMat[3,]/255
  )
}
