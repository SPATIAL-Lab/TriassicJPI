library(R2jags)
source("code/prepData.R")

# Wrap up all the data
d = list(lats_mar.an = lats_mar.an, lats_cont.an = lats_cont.an,
         mp_sites.ind.an = mp_sites.ind.an, mc_sites.ind.an = mc_sites.ind.an,
         clump_sites.ind.an = clump_sites.ind.an, pc_sites.ind.an = pc_sites.ind.an,
         cia_sites.ind.an = cia_sites.ind.an, alsi_sites.ind.an = alsi_sites.ind.an,
         pwi_sites.ind.an = pwi_sites.ind.an, mp_data.an = mp_samples.an$d18O, 
         mc_data.an = mc_samples.an$d18O, clump_data.an = clump_data.an, 
         pc_data.an = pc_samples.an$d18O, cia_data.an = cia_samples.an$CIA, 
         alsi_data.an = alsi_samples.an$AlSi, pwi_data.an = pwi_samples.an$PWI,
         lats_mar.car = lats_mar.car, lats_cont.car = lats_cont.car,
         mp_sites.ind.car = mp_sites.ind.car, md_sites.ind.car = md_sites.ind.car, 
         mc_sites.ind.car = mc_sites.ind.car,
         pc_sites.ind.car = pc_sites.ind.car, cia_sites.ind.car = cia_sites.ind.car,
         alsi_sites.ind.car = alsi_sites.ind.car, pwi_sites.ind.car = pwi_sites.ind.car,
         mp_data.car = mp_samples.car$d18O, md_data.car = md_samples.car$d18O, 
         mc_data.car = mc_samples.car$d18O,
         pc_data.car = pc_samples.car$d18O, cia_data.car = cia_samples.car$CIA,
         alsi_data.car = alsi_samples.car$AlSi, pwi_data.car = pwi_samples.car$PWI,
         alsi_cal = alsi_cal, cia_cal = cia_cal, pwi_cal = pwi_cal)
parms = c("a.an", "b.an", "c.an", "d.an", "e.an", "t_seas.an", "dO_cont_off.an", "dO_mar.mu",
          "t_mar.an", "dO_mar.an", "t_cont.an", "dO_cont.an", "lat_mar.an", "lat_cont.an",
          "a.car", "b.car", "c.car", "d.car", "e.car", "t_seas.car", "dO_cont_off.car", 
          "t_mar.car", "dO_mar.car", "t_cont.car", "dO_cont.car", "lat_mar.car", "lat_cont.car",
          "alsi_var", "alsi_slope", "alsi_int", "cia_var", "cia_slope", "cia_int",
          "pwi_var", "pwi_a", "pwi_b")

# Some parameters for the sampler
post = jags.parallel(model.file = "code/modelAll.R", parameters.to.save = parms, data = d,
                 inits = NULL, n.chains = 3, n.iter = 2e5, n.burnin = 2e4,
                 n.thin = 20)
save(post, file = "bigout/post.rda")

# Diagnostics
plot(post)
traceplot(post$BUGSoutput)
View(post$BUGSoutput$summary)

# Summary
s = post$BUGSoutput$summary
anout.c = data.frame("Site" = cont_sites.an$Site)
anout.c = cbind(anout.c, "Lat_m" = 
                  s[grep("^lat_cont.an", row.names(s)), 1])
anout.c = cbind(anout.c, "T_m" = s[grep("^t_cont.an", row.names(s)), 1])
anout.c = cbind(anout.c, "T_sd" = s[grep("^t_cont.an", row.names(s)), 2])

anout.m = data.frame("Site" = mar_sites.an$Site)
anout.m = cbind(anout.m, "Lat_m" = 
                  s[grep("^lat_mar.an", row.names(s)), 1])
anout.m = cbind(anout.m, "T_m" = s[grep("^t_mar.an", row.names(s)), 1])
anout.m = cbind(anout.m, "T_sd" = s[grep("^t_mar.an", row.names(s)), 2])

anout = rbind(anout.c, anout.m)

write.csv(anout, "out/Anisian.csv", row.names = FALSE)

carout.c = data.frame("Site" = cont_sites.car$Site)
carout.c = cbind(carout.c, "Lat_m" =
                   s[grep("^lat_cont.car", row.names(s)), 1])
carout.c = cbind(carout.c, "T_m" = s[grep("^t_cont.car", row.names(s)), 1])
carout.c = cbind(carout.c, "T_sd" = s[grep("^t_cont.car", row.names(s)), 2])

carout.m = data.frame("Site" = mar_sites.car$Site)
carout.m = cbind(carout.m, "Lat_m" = 
                   s[grep("^lat_mar.car", row.names(s)), 1])
carout.m = cbind(carout.m, "T_m" = s[grep("^t_mar.car", row.names(s)), 1])
carout.m = cbind(carout.m, "T_sd" = s[grep("^t_mar.car", row.names(s)), 2])

carout = rbind(carout.c, carout.m)

write.csv(carout, "out/Carnian.csv", row.names = FALSE)

