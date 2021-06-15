# atm 14C
Datm <- rbind(graven, future14C)
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

# calculate resp flux points at 1991 and 2019 and put in data frame
arc.rewet.rsp <- arc.rewet.14c[arc.rewet.14c$Site == "Hainich" & 
                           arc.rewet.14c$Type == "F" & 
                           arc.rewet.14c$treat.bi == "control", 
                         "d14c_corr_inc"]
yrs.smp <- arc.rewet.14c[arc.rewet.14c$Site == "Hainich" & 
                                 arc.rewet.14c$Type == "F" & 
                                 arc.rewet.14c$treat.bi == "control", 
                               "YearSampled"] + .5
obs.rsp <- data.frame(time = yrs.smp,
                      resp = arc.rewet.rsp)

arc.rewet.rsp.t <- arc.rewet.14c[arc.rewet.14c$Site == "Hainich" & 
                                   arc.rewet.14c$Type == "F" & 
                                   arc.rewet.14c$treat.bi == "treatment", 
                                 "d14c_corr_inc"]
yrs.smp.t <- arc.rewet.14c[arc.rewet.14c$Site == "Hainich" & 
                           arc.rewet.14c$Type == "F" & 
                           arc.rewet.14c$treat.bi == "treatment", 
                         "YearSampled"] + .5
obs.rsp.t <- data.frame(time = yrs.smp.t,
                        resp = arc.rewet.rsp.t)
#####
# mod fit fxns
#####
cgam <- function(pars, In = 1, C = 38.96104, frc = .1) {
  
  # cstock
  Cfast <- frc * C
  Cslow <- C - Cfast

  # define pars
  fast.fm <- fm(pars[1])
  slow.fm <- fm(pars[2])
  F0_Delta14C <- fm_14c(c(fast.fm, slow.fm), date = c(1900, 1900))
  gam <- (C * Cfast * pars[1])/(C * Cfast * pars[1] + C * Cslow * pars[2])
  
  # model matrix
  A <- -diag(pars[1:2])
  
  # steady-state C stocks
  ss.cstock <- (-1 * solve(A) %*% c(In * gam, In * (1 - gam)))
  
  # return
  return(list(gam = gam, ss.cstock = ss.cstock))
}

modFun_2pp <- function(pars, In = 1, lag = 0, pass = TRUE, out = "modFit", 
                       gam = FALSE, yrs = Datm$Date) {
  # constants
  cstock <- 38.96104 # forest C pool, total (mean Hainich forest C  = 24 g kg^-1); nb @ ss = sum(-1*solve(A)%*%c(f.in*f.gam, f.in*(1-f.gam)))
  f.in <- In # inputs (12.5=mean equilibrium C flux, annual extrapolation, gC yr-1, 2011 incubation data); try much less...
  f.frc <- .1 # C-stock partitioning coefficient (from Schrumpf 2013, Hainich 0-5, free light)
  f.Cfast <- f.frc * cstock
  f.Cslow <- cstock - f.Cfast
  
  # define pars
  f.kfast <- pars[1] # unknown
  f.kslow <- pars[2] # unknown
  f.fast.fm <- fm(f.kfast)
  f.slow.fm <- fm(f.kslow)
  f.F0_Delta14C <- fm_14c(c(f.fast.fm, f.slow.fm), date = c(1900, 1900))
  f.gam <- (cstock*f.Cfast*f.kfast)/(cstock*f.Cfast*f.kfast+cstock*f.Cslow*f.kslow) # input partitioning coefficient (proportion to fast pool, function of ks)
  if (gam) {
    f.gam <- pars[3]
  }
  
  # model matrix
  A <- -diag(pars[1:2])
  
  # steady-state C stocks
  ss.cstock <- (-1 * solve(A) %*% c(In * f.gam, In * (1 - f.gam)))
  
  # time index
  ix.t <- c((lag + 1):length(yrs))
  
  # model
  mod <- TwopParallelModel14(t = yrs[ix.t],
                             ks = c(f.kfast, f.kslow),
                             C0 = c(f.Cfast, f.Cslow),
                             F0_Delta14C = f.F0_Delta14C,
                             In = f.in,
                             gam = f.gam,
                             inputFc = Datm,
                             lag = lag,
                             pass = pass)

  # get mod values
  C14m <- getF14C(mod)
  C14p <- getF14(mod)
  C14r <- getF14R(mod)
  Ctot <- getC(mod)
    
  if(out == "modFit") {
    # dataframe for modFit fx
    return(data.frame(
      time = Datm$Date[ix.t], 
      resp = C14r))
  } else {
    # data frame for plotting
    return(data.frame(
      years = rep(Datm$Date[ix.t], 5),
      d14C = c(C14p[,1], 
               C14p[,2], 
               C14m,
               C14r,
               Datm$NHc14[ix.t]),
      pool = rep(c("fast", "slow", "bulk C", "respiration", "atm"), each = nrow(C14p))))
  }
  }

# Use median values for ks and lag value from Schrumpf 2015
schrumpf.2015 <- modFun_2pp(par = c(1/4, 1/115), 
                            In = 1,
                            lag = 8,
                            out = "plot",
                            gam = FALSE)
cgam(c(1/4, 1/115))

# also for medians
par.25.25 <- c(1, 1/80)
par.25.75 <- c(1, 1/200)
par.75.25 <- c(7, 1/80)
par.75.75 <- c(7, 1/200)
L8.25.25 <- modFun_2pp(par.25.25,
                       In = 1,
                       lag = 8,
                       out = "plot",
                       gam = FALSE)
L8.25.75 <- modFun_2pp(par.25.75,
                       In = 1,
                       lag = 8,
                       out = "plot",
                       gam = FALSE)
L8.75.25 <- modFun_2pp(par.75.25,
                       In = 1,
                       lag = 8,
                       out = "plot",
                       gam = FALSE)
L8.75.75 <- modFun_2pp(par.75.75,
                       In = 1,
                       lag = 8,
                       out = "plot",
                       gam = FALSE)
schrumpf.2015$q25.25 <- L8.25.25$d14C - schrumpf.2015$d14C
schrumpf.2015$q25.75 <- L8.25.75$d14C - schrumpf.2015$d14C
schrumpf.2015$q75.25 <- L8.75.25$d14C - schrumpf.2015$d14C
schrumpf.2015$q75.75 <- L8.75.75$d14C - schrumpf.2015$d14C
save(schrumpf.2015, file = "/Users/jeff/arc-inc/src/schrumpf.2015.mod.RData")

# Observed respiration
# ctl
obs.rsp.m <- obs.rsp %>%
  group_by(time) %>%
  summarize(across(resp, .fns = c(mean = mean, sd = sd))) %>%
  rename(d14C = resp_mean) %>%
  mutate(years = time - .25,
         Treatment = "control")
save(obs.rsp.m, file = "/Users/jeff/arc-inc/src/obs.rsp.m.RData")
# trt
obs.rsp.t.m <- obs.rsp.t %>%
  group_by(time) %>%
  summarize(across(resp, .fns = c(mean = mean, sd = sd))) %>%
  rename(d14C = resp_mean) %>%
  mutate(years = time + .25,
         Treatment = ifelse(time == "2011.5", "air-dry/rewet + storage", "air-dry/rewet"))
save(obs.rsp.t.m, file = "/Users/jeff/arc-inc/src/obs.rsp.m.t.RData")

# plot medians w/ potential error ranges
schrumpf.2015 %>%
  filter(pool != "bulk C") %>%
  ggplot(., aes(years, d14C)) +
  geom_path(aes(years, d14C - q25.25, color = pool), show.legend = FALSE, linetype = "dotted") +
  geom_path(aes(years, d14C + q25.25, color = pool), show.legend = FALSE, linetype = "dotted") +
  geom_path(aes(years, d14C - q25.75, color = pool), show.legend = FALSE, linetype = "dashed") +
  geom_path(aes(years, d14C + q25.75, color = pool), show.legend = FALSE, linetype = "dashed") +
  geom_path(aes(years, d14C - q75.25, color = pool), show.legend = FALSE, linetype = "dotdash") +
  geom_path(aes(years, d14C + q75.25, color = pool), show.legend = FALSE, linetype = "dotdash") +
  geom_path(aes(years, d14C - q75.75, color = pool), show.legend = FALSE, linetype = "longdash") +
  geom_path(aes(years, d14C + q75.75, color = pool), show.legend = FALSE, linetype = "longdash") +
  geom_path(aes(color = pool)) +
  geom_point(data = obs.rsp.m, aes(years, d14C, shape = Treatment), 
             color = "#FFC107", size = 3) +
  geom_point(data = obs.rsp.t.m, aes(years, d14C, shape = Treatment), 
             color = "#FFC107", size = 3) +
  geom_errorbar(data = obs.rsp.m, 
                aes(ymin = d14C - resp_sd, ymax = d14C + resp_sd),
                color = "#FFC107", width = .2) +
  geom_errorbar(data = obs.rsp.t.m, 
                aes(ymin = d14C - resp_sd, ymax = d14C + resp_sd),
                color = "#FFC107", width = .2) +
  scale_color_manual(
    name = "Model pool",
    values = c("atm" = "black",
               "fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5"),
    labels = c("atm" = "atmosphere\n(measured)",
               "fast" = "fast",
               "slow" = "slow")) +
  scale_fill_manual(
    values = c("atm" = 8,
               "fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5")) +
  scale_shape_manual(
    values = c("control" = 16,
               "air-dry/rewet" = 1, 
               "air-dry/rewet + storage" = 0),
    labels = c("control" = "control-1 (2011)\ncontrol-2 (2019)",
               "air-dry/rewet" = "air-dry/rewet", 
               "air-dry/rewet + storage" = "air-dry/rewet + storage")) +
  scale_x_continuous(limits = c(2000, 2022)) +
  scale_y_continuous(limits = c(-100, 180)) +
  xlab("Year") +
  ylab(expression(''*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

# plot final fig w/ 75th quartile error on resp
schrumpf.2015$q75.75 <- ifelse(schrumpf.2015$pool == "respiration", schrumpf.2015$q75.75, NA)
schrumpf.2015 %>%
  filter(pool != "bulk C") %>%
  ggplot(., aes(years, d14C)) +
  geom_ribbon(aes(ymin = d14C - q75.75, ymax = d14C + q75.75, fill = pool),
              alpha = .1, show.legend = FALSE) +
  geom_path(aes(color = pool)) +
  geom_point(data = obs.rsp.m, aes(years, d14C, shape = Treatment), 
             color = "#FFC107", size = 3) +
  geom_point(data = obs.rsp.t.m, aes(years, d14C, shape = Treatment), 
             color = "#FFC107", size = 3) +
  geom_errorbar(data = obs.rsp.m, 
                aes(ymin = d14C - resp_sd, ymax = d14C + resp_sd),
                color = "#FFC107", width = .2) +
  geom_errorbar(data = obs.rsp.t.m, 
                aes(ymin = d14C - resp_sd, ymax = d14C + resp_sd),
                color = "#FFC107", width = .2) +
  scale_color_manual(
    name = "Model pool",
    values = c("atm" = "black",
               "fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5"),
    labels = c("atm" = "atmosphere\n(measured)",
               "fast" = "fast",
               "slow" = "slow")) +
  scale_fill_manual(
    values = c("atm" = 8,
               "fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5")) +
  scale_shape_manual(
    values = c("control" = 16,
               "air-dry/rewet" = 1, 
               "air-dry/rewet + storage" = 0),
    labels = c("control" = "control-1 (2011)\ncontrol-2 (2019)",
               "air-dry/rewet" = "air-dry/rewet", 
               "air-dry/rewet + storage" = "air-dry/rewet + storage")) +
  scale_x_continuous(limits = c(2000, 2022)) +
  scale_y_continuous(limits = c(-25, 180)) +
  xlab("Year") +
  ylab(expression(''*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

modFun_2pp(par = c(1/7, 1/80), 
           In = 1,
           lag = 8,
           out = "plot",
           gam = FALSE) %>%
  filter(pool != "bulk C") %>%
  ggplot(., aes(years, d14C)) +
  geom_path(aes(color = pool)) +
  geom_point(data = obs.rsp.m, aes(years, d14C, shape = Treatment), 
             color = "#FFC107", size = 3) +
  geom_point(data = obs.rsp.t.m, aes(years, d14C, shape = Treatment), 
             color = "#FFC107", size = 3) +
  geom_errorbar(data = obs.rsp.m, 
                aes(ymin = d14C - resp_sd, ymax = d14C + resp_sd),
                color = "#FFC107", width = .2) +
  geom_errorbar(data = obs.rsp.t.m, 
                aes(ymin = d14C - resp_sd, ymax = d14C + resp_sd),
                color = "#FFC107", width = .2) +
  scale_color_manual(
    name = "Model pool",
    values = c("atm" = "black",
               "fast" = "#D81B60", 
               "respiration" = "#FFC107", 
               "slow" = "#1E88E5"),
    labels = c("atm" = "atmosphere\n(measured)",
               "fast" = "fast",
               "slow" = "slow")) +
  scale_shape_manual(
    values = c("control" = 16,
               "air-dry/rewet" = 1, 
               "air-dry/rewet + storage" = 0),
    labels = c("control" = "control-1 (2011)\ncontrol-2 (2019)",
               "air-dry/rewet" = "air-dry/rewet", 
               "air-dry/rewet + storage" = "air-dry/rewet + storage")) +
  scale_x_continuous(limits = c(2000, 2022)) +
  scale_y_continuous(limits = c(-25, 180)) +
  xlab("Year") +
  ylab(expression(''*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())
