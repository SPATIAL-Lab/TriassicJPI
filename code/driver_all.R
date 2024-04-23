library(openxlsx)
library(R2jags)

#Read in calibration data
alsi_cal = as.matrix(read.xlsx("data/alsi_cal.xlsx"))
cia_cal = as.matrix(read.csv("data/Sheldon_climofunc.txt"))
pwi_cal = as.matrix(read.csv("data/pwi.txt"))

#Read in Anisian data ----
dfile = "data/dataAnisian.xlsx"
pc_samples.an = read.xlsx(dfile, sheet = "Cont_d18O")
cia_samples.an = read.xlsx(dfile, sheet = "Cont_CIA")
alsi_samples.an = read.xlsx(dfile, sheet = "Cont_AlSi")
cont_sites.an = read.xlsx(dfile, sheet = "Cont_Sites")
clump_samples.an = read.xlsx(dfile, sheet = "Cont_Clump")
mSamples.an = read.xlsx(dfile, sheet = "Mar_d18O")
mar_sites.an = read.xlsx(dfile, sheet = "Mar_Sites")

#Split out marine phosphate and carbonate d18O
mp_samples.an = mSamples.an[mSamples.an$Material == "conP",]
mc_samples.an = mSamples.an[mSamples.an$Material != "conP",]

#Generate site index vector for each dataset
mp_sites.ind.an = match(mp_samples.an$Site, mar_sites.an$Site)
mc_sites.ind.an = match(mc_samples.an$Site, mar_sites.an$Site)
clump_sites.ind.an = match(clump_samples.an$Site, cont_sites.an$Site)
pc_sites.ind.an = match(pc_samples.an$Site, cont_sites.an$Site)
cia_sites.ind.an = match(cia_samples.an$Site, cont_sites.an$Site)
alsi_sites.ind.an = match(alsi_samples.an$Site, cont_sites.an$Site)

#clump data as matrix: Cap47, d18O
clump_data.an = as.matrix(clump_samples.an[,c("cap_47", "d18O")])

#lats as matricies: min, max
lats_cont.an = as.matrix(cont_sites.an[,c("lat_min", "lat_max")])
lats_mar.an = as.matrix(mar_sites.an[,c("lat_min", "lat_max")])

#Read in Ladinian data ----
dfile = "data/dataLadinian.xlsx"
cia_samples.lad = read.xlsx(dfile, sheet = "Cont_CIA")
cont_sites.lad = read.xlsx(dfile, sheet = "Cont_Sites")
mSamples.lad = read.xlsx(dfile, sheet = "Mar_d18O")
mar_sites.lad = read.xlsx(dfile, sheet = "Mar_Sites")

#Split out marine phosphate and carbonate d18O
mp_samples.lad = mSamples.lad[mSamples.lad$Material == "conP",]
mc_samples.lad = mSamples.lad[mSamples.lad$Material != "conP",]

#Generate site index vectore for each dataset
mp_sites.ind.lad = match(mp_samples.lad$Site, mar_sites.lad$Site)
mc_sites.ind.lad = match(mc_samples.lad$Site, mar_sites.lad$Site)
cia_sites.ind.lad = match(cia_samples.lad$Site, cont_sites.lad$Site)

#lats as matricies: min, max
lats_cont.lad = as.matrix(cont_sites.lad[,c("lat_min", "lat_max")])
lats_mar.lad = as.matrix(mar_sites.lad[,c("lat_min", "lat_max")])

#Read in Carnian data ----
dfile = "data/dataCarnian.xlsx"
pc_samples.car = read.xlsx(dfile, sheet = "Cont_d18O")
cia_samples.car = read.xlsx(dfile, sheet = "Cont_CIA")
alsi_samples.car = read.xlsx(dfile, sheet = "Cont_AlSi")
pwi_samples.car = read.xlsx(dfile, sheet = "Cont_PWI")
cont_sites.car = read.xlsx(dfile, sheet = "Cont_Sites")
mSamples.car = read.xlsx(dfile, sheet = "Mar_d18O")
mar_sites.car = read.xlsx(dfile, sheet = "Mar_Sites")

#Split out marine phosphate and carbonate d18O
mp_samples.car = mSamples.car[mSamples.car$Material == "conP",]
mc_samples.car = mSamples.car[mSamples.car$Material != "conP",]

#Generate site index vectore for each dataset
mp_sites.ind.car = match(mp_samples.car$Site, mar_sites.car$Site)
mc_sites.ind.car = match(mc_samples.car$Site, mar_sites.car$Site)
pc_sites.ind.car = match(pc_samples.car$Site, cont_sites.car$Site)
cia_sites.ind.car = match(cia_samples.car$Site, cont_sites.car$Site)
alsi_sites.ind.car = match(alsi_samples.car$Site, cont_sites.car$Site)
pwi_sites.ind.car = match(pwi_samples.car$Site, cont_sites.car$Site)

#lats as matricies: min, max
lats_cont.car = as.matrix(cont_sites.car[,c("lat_min", "lat_max")])
lats_mar.car = as.matrix(mar_sites.car[,c("lat_min", "lat_max")])

#Wrap up all the data
d = list(lats_mar.an = lats_mar.an, lats_cont.an = lats_cont.an,
         mp_sites.ind.an = mp_sites.ind.an, mc_sites.ind.an = mc_sites.ind.an,
         clump_sites.ind.an = clump_sites.ind.an, pc_sites.ind.an = pc_sites.ind.an,
         cia_sites.ind.an = cia_sites.ind.an, alsi_sites.ind.an = alsi_sites.ind.an,
         mp_data.an = mp_samples.an$d18O, mc_data.an = mc_samples.an$d18O,
         clump_data.an = clump_data.an, pc_data.an = pc_samples.an$d18O,
         cia_data.an = cia_samples.an$CIA, alsi_data.an = alsi_samples.an$AlSi,
         lats_mar.lad = lats_mar.lad, lats_cont.lad = lats_cont.lad,
         mp_sites.ind.lad = mp_sites.ind.lad, mc_sites.ind.lad = mc_sites.ind.lad,
         cia_sites.ind.lad = cia_sites.ind.lad,
         mp_data.lad = mp_samples.lad$d18O, mc_data.lad = mc_samples.lad$d18O,
         cia_data.lad = cia_samples.lad$CIA,
         lats_mar.car = lats_mar.car, lats_cont.car = lats_cont.car,
         mp_sites.ind.car = mp_sites.ind.car, mc_sites.ind.car = mc_sites.ind.car,
         pc_sites.ind.car = pc_sites.ind.car, cia_sites.ind.car = cia_sites.ind.car,
         alsi_sites.ind.car = alsi_sites.ind.car, pwi_sites.ind.car = pwi_sites.ind.car,
         mp_data.car = mp_samples.car$d18O, mc_data.car = mc_samples.car$d18O,
         pc_data.car = pc_samples.car$d18O, cia_data.car = cia_samples.car$CIA,
         alsi_data.car = alsi_samples.car$AlSi, pwi_data.car = pwi_samples.car$PWI,
         alsi_cal = alsi_cal, cia_cal = cia_cal, pwi_cal = pwi_cal)
parms = c("a.an", "b.an", "c.an", "d.an", "e.an", "t_cont_off.an", "dO_cont_off.an", 
          "t_mar.an", "dO_mar.an", "t_cont.an", "dO_cont.an", "lat_mar.an", "lat_cont.an",
          "a.lad", "b.lad", "c.lad", "t_cont_off.lad", 
          "t_mar.lad", "dO_mar.lad", "t_cont.lad", "dO_cont.lad", "lat_mar.lad", "lat_cont.lad",
          "a.car", "b.car", "c.car", "d.car", "e.car", "t_cont_off.car", "dO_cont_off.car", 
          "t_mar.car", "dO_mar.car", "t_cont.car", "dO_cont.car", "lat_mar.car", "lat_cont.car",
          "alsi_var", "alsi_slope", "alsi_int", "cia_var", "cia_slope", "cia_int",
          "pwi_var", "pwi_a", "pwi_b")

#Some parameters for the sampler
n.iter = 50000
n.burnin = 1000
n.thin = 10

post = jags.parallel(model.file = "code/model_all.R", parameters.to.save = parms, data = d,
                 inits = NULL, n.chains = 3, n.iter = 50000, n.burnin = 1000,
                 n.thin = 10)

#diagnostics
plot(post)
traceplot(post$BUGSoutput)
View(post$BUGSoutput$summary)
save(post, file = "bigout/post.rda")
sl = post$BUGSoutput$sims.list

#summary
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

write.csv(anout, "out/Anasinian.csv", row.names = FALSE)

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

#Plots ----
###Without Ladinian
l = seq(1:70)
pdf("out/T_grad_noLad.pdf", width = 6, height = 5)
plot(l, 23 + l * 0.22 + l^2 * -0.008, type = "l", lwd = 2, ylim = c(0,43),
     xlab = "Latitude", ylab = "Mean temperature")
for(i in seq(1, nrow(post$BUGSoutput$sims.list$a.an), length.out = 1000)){
        lines(l, post$BUGSoutput$sims.list$a.an[i] + l * 
                      post$BUGSoutput$sims.list$b.an[i] + 
                      l^2 * post$BUGSoutput$sims.list$c.an[i], 
              col = rgb(0.2,0.6,1,0.01))
}
for(i in seq(1, nrow(post$BUGSoutput$sims.list$a.car), length.out = 1000)){
        lines(l, post$BUGSoutput$sims.list$a.car[i] + l * 
                      post$BUGSoutput$sims.list$b.car[i] + 
                      l^2 * post$BUGSoutput$sims.list$c.car[i], 
              col = rgb(1,0.2,0.6,0.01))
}
lines(l, c(post$BUGSoutput$mean$a.an) + l * c(post$BUGSoutput$mean$b.an) + 
              l^2 * c(post$BUGSoutput$mean$c.an), col = rgb(0.2,0.6,1), lwd = 2)
lines(l, c(post$BUGSoutput$mean$a.car) + l * c(post$BUGSoutput$mean$b.car) + 
              l^2 * c(post$BUGSoutput$mean$c.car), col = rgb(1,0.2,0.6), lwd = 2)

arrows(post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an - post$BUGSoutput$sd$t_mar.an, 
       post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an + post$BUGSoutput$sd$t_mar.an, 
       angle = 90, length = 0.05, code = 3, col = rgb(0.2,0.6,1))
points(post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an, pch = 21, bg = "white", 
       col = rgb(0.2,0.6,1))
arrows(post$BUGSoutput$mean$lat_cont.an, 
       post$BUGSoutput$mean$t_cont.an - post$BUGSoutput$sd$t_cont.an, 
       post$BUGSoutput$mean$lat_cont.an, 
       post$BUGSoutput$mean$t_cont.an + post$BUGSoutput$sd$t_cont.an, 
       angle = 90, length = 0.05, code = 3, 
       col = rgb(0.2,0.6,1))
points(post$BUGSoutput$mean$lat_cont.an, post$BUGSoutput$mean$t_cont.an, 
       pch = 21, bg = "dark grey", col = rgb(0.2,0.6,1))

arrows(post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car - post$BUGSoutput$sd$t_mar.car, 
       post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car + post$BUGSoutput$sd$t_mar.car, 
       angle = 90, length = 0.05, code = 3, col = rgb(1,0.2,0.6))
points(post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car, pch = 21, bg = "white", 
       col = rgb(1,0.2,0.6))
arrows(post$BUGSoutput$mean$lat_cont.car, 
       post$BUGSoutput$mean$t_cont.car - post$BUGSoutput$sd$t_cont.car, 
       post$BUGSoutput$mean$lat_cont.car, 
       post$BUGSoutput$mean$t_cont.car + post$BUGSoutput$sd$t_cont.car, 
       angle = 90, length = 0.05, code = 3, 
       col = rgb(1,0.2,0.6))
points(post$BUGSoutput$mean$lat_cont.car, post$BUGSoutput$mean$t_cont.car, 
       pch = 21, bg = "dark grey", col = rgb(1,0.2,0.6))

# x = post$BUGSoutput$summary[c("lat_cont.an[1]", "lat_cont.an[2]"),1]
# y = post$BUGSoutput$summary[c("t_cont.an[1]", "t_cont.an[2]"),1]
# z = cont_sites.an$Site[c(1,2)]
# text(x, y, z, pos = 4, col = rgb(0.2,0.6,1))
# 
# x = post$BUGSoutput$summary[c("lat_cont.car[1]", "lat_cont.car[2]", 
#                               "lat_cont.car[4]", "lat_cont.car[5]", 
#                               "lat_cont.car[6]"),1]
# y = post$BUGSoutput$summary[c("t_cont.car[1]", "t_cont.car[2]", 
#                               "t_cont.car[4]", "t_cont.car[5]",
#                               "t_cont.car[6]"),1]
# z = cont_sites.car$Site[c(1,2,4,5,6)]
# z[1] = "Los Rastros (SI)"
# z[2] = "Ischigualasto (SJ)"
# z[3] = "Los Rastros"
# text(x[c(1,4,5)], y[c(1,4,5)], z[c(1,4,5)], pos = 2, col = rgb(1,0.2,0.6))
# text(x[c(2,3)], y[c(2,3)], z[c(2,3)], pos = 4, col = rgb(1,0.2,0.6))
dev.off()

#Temperature gradient parameters
png("out/T_grad_parms.png", width = 9, height = 4, units = "in", res = 600)
layout(matrix(c(1,2,3), ncol = 3))

plot(density(rnorm(100000, 23, 2)), main = "a", ylim = c(0, 0.8), 
     xlim = c(18, 42), xlab = "", lwd = 2)
lines(density(post$BUGSoutput$sims.list$a.an), col = rgb(0.2,0.6,1), lwd = 2)
lines(density(post$BUGSoutput$sims.list$a.car), col = rgb(1,0.2,0.6), lwd = 2)

plot(density(rnorm(100000, 0.22, sqrt(1/500))), main = "b", ylim = c(0, 10),
     xlim = c(0.05, 0.45), xlab = "", lwd = 2)
lines(density(post$BUGSoutput$sims.list$b.an), col = rgb(0.2,0.6,1), lwd = 2)
lines(density(post$BUGSoutput$sims.list$b.car), col = rgb(1,0.2,0.6), lwd = 2)

plot(density(rnorm(100000, -0.008, sqrt(1e-5))), main = "c", ylim = c(0, 400),
     xlab = "", lwd = 2)
lines(density(post$BUGSoutput$sims.list$c.an), col = rgb(0.2,0.6,1), lwd = 2)
lines(density(post$BUGSoutput$sims.list$c.car), col = rgb(1,0.2,0.6), lwd = 2)
dev.off()

#Argentina offsets
png("out/A_off_parms.png", width = 6, height = 4, units = "in", res = 600)
layout(matrix(c(1,2), ncol = 2))

plot(density(rnorm(100000, 0, 2)), main = "T_off", ylim = c(0, 0.3), 
     xlab = "")
lines(density(post$BUGSoutput$sims.list$t_cont_off[,1]), col = "red")
lines(density(post$BUGSoutput$sims.list$t_cont_off[,2]), col = "red")
lines(density(post$BUGSoutput$sims.list$t_cont_off[,3]), col = "red", lty = 2)
lines(density(post$BUGSoutput$sims.list$t_cont_off[,4]), col = "red", lty = 3)
lines(density(post$BUGSoutput$sims.list$t_cont_off[,5]), col = "red", lty = 4)

plot(density(rnorm(100000, 0, 2)), main = "d18O_off",
     ylim = c(0, 0.4), xlab = "")
lines(density(post$BUGSoutput$sims.list$dO_cont_off[,1]), col = "red")
lines(density(post$BUGSoutput$sims.list$dO_cont_off[,2]), col = "red")
lines(density(post$BUGSoutput$sims.list$dO_cont_off[,3]), col = "red", lty = 2)
lines(density(post$BUGSoutput$sims.list$dO_cont_off[,4]), col = "red", lty = 3)
lines(density(post$BUGSoutput$sims.list$dO_cont_off[,5]), col = "red", lty = 4)
dev.off()

#d18O-t parameters
png("out/d18O_parms.png", width = 6, height = 4, units = "in", res = 600)
layout(matrix(c(1,2), ncol = 2))

plot(density(rnorm(100000, -14, 2)), main = "d", xlim = c(-25, -8), 
     ylim = c(0, 0.5), xlab = "", lwd = 2)
lines(density(post$BUGSoutput$sims.list$d.an), col = rgb(0.2,0.6,1), lwd = 2)
lines(density(post$BUGSoutput$sims.list$d.car), col = rgb(1,0.2,0.6), lwd = 2)

plot(density(rnorm(100000, 0.59, sqrt(1/250))), main = "e", xlim = c(0.2, 0.8),
     ylim = c(0, 10), xlab = "", lwd = 2)
lines(density(post$BUGSoutput$sims.list$e.an[,1]), col = rgb(0.2,0.6,1), lwd = 2)
lines(density(post$BUGSoutput$sims.list$e.car[,1]), col = rgb(1,0.2,0.6), lwd = 2)
dev.off()

## Temperature Gradient all
l = seq(1:70)
png("out/T_grad.png", width = 6, height = 5, units = "in", res = 600)
plot(l, 23 + l * 0.22 + l^2 * -0.008, type = "l", ylim = c(0,43),
     xlab = "Latitude", ylab = "Mean temperature")
for(i in seq(1, nrow(post$BUGSoutput$sims.list$a.an), length.out = 1000)){
  lines(l, post$BUGSoutput$sims.list$a.an[i] + l * 
          post$BUGSoutput$sims.list$b.an[i] + 
          l^2 * post$BUGSoutput$sims.list$c.an[i], 
        col = rgb(0.2,0.6,1,0.01))
}
for(i in seq(1, nrow(post$BUGSoutput$sims.list$a.lad), length.out = 1000)){
  lines(l, post$BUGSoutput$sims.list$a.lad[i] + l * 
          post$BUGSoutput$sims.list$b.lad[i] + 
          l^2 * post$BUGSoutput$sims.list$c.lad[i], 
        col = rgb(1,0.6,0.2,0.01))
}
for(i in seq(1, nrow(post$BUGSoutput$sims.list$a.car), length.out = 1000)){
  lines(l, post$BUGSoutput$sims.list$a.car[i] + l * 
          post$BUGSoutput$sims.list$b.car[i] + 
          l^2 * post$BUGSoutput$sims.list$c.car[i], 
        col = rgb(1,0.2,0.6,0.01))
}
lines(l, c(post$BUGSoutput$mean$a.an) + l * c(post$BUGSoutput$mean$b.an) + 
        l^2 * c(post$BUGSoutput$mean$c.an), col = rgb(0.2,0.6,1))
lines(l, c(post$BUGSoutput$mean$a.lad) + l * c(post$BUGSoutput$mean$b.lad) + 
        l^2 * c(post$BUGSoutput$mean$c.lad), col = rgb(1,0.6,0.2))
lines(l, c(post$BUGSoutput$mean$a.car) + l * c(post$BUGSoutput$mean$b.car) + 
        l^2 * c(post$BUGSoutput$mean$c.car), col = rgb(1,0.2,0.6))

arrows(post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an - post$BUGSoutput$sd$t_mar.an, 
       post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an + post$BUGSoutput$sd$t_mar.an, 
       angle = 90, length = 0.05, code = 3, col = rgb(0.2,0.6,1))
points(post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an, pch = 21, bg = "white", 
       col = rgb(0.2,0.6,1))
arrows(post$BUGSoutput$mean$lat_cont.an, 
       post$BUGSoutput$mean$t_cont.an - post$BUGSoutput$sd$t_cont.an, 
       post$BUGSoutput$mean$lat_cont.an, 
       post$BUGSoutput$mean$t_cont.an + post$BUGSoutput$sd$t_cont.an, 
       angle = 90, length = 0.05, code = 3, 
       col = rgb(0.2,0.6,1))
points(post$BUGSoutput$mean$lat_cont.an, post$BUGSoutput$mean$t_cont.an, 
       pch = 21, bg = "dark grey", col = rgb(0.2,0.6,1))

arrows(post$BUGSoutput$mean$lat_mar.lad, 
       post$BUGSoutput$mean$t_mar.lad - post$BUGSoutput$sd$t_mar.lad, 
       post$BUGSoutput$mean$lat_mar.lad, 
       post$BUGSoutput$mean$t_mar.lad + post$BUGSoutput$sd$t_mar.lad, 
       angle = 90, length = 0.05, code = 3, col = rgb(1,0.6,0.2))
points(post$BUGSoutput$mean$lat_mar.lad, 
       post$BUGSoutput$mean$t_mar.lad, pch = 21, bg = "white", 
       col = rgb(1,0.6,0.2))
arrows(post$BUGSoutput$mean$lat_cont.lad, 
       post$BUGSoutput$mean$t_cont.lad - post$BUGSoutput$sd$t_cont.lad, 
       post$BUGSoutput$mean$lat_cont.lad, 
       post$BUGSoutput$mean$t_cont.lad + post$BUGSoutput$sd$t_cont.lad, 
       angle = 90, length = 0.05, code = 3, 
       col = rgb(1,0.6,0.2))
points(post$BUGSoutput$mean$lat_cont.lad, post$BUGSoutput$mean$t_cont.lad, 
       pch = 21, bg = "dark grey", col = rgb(1,0.6,0.2))

arrows(post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car - post$BUGSoutput$sd$t_mar.car, 
       post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car + post$BUGSoutput$sd$t_mar.car, 
       angle = 90, length = 0.05, code = 3, col = rgb(1,0.2,0.6))
points(post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car, pch = 21, bg = "white", 
       col = rgb(1,0.2,0.6))
arrows(post$BUGSoutput$mean$lat_cont.car, 
       post$BUGSoutput$mean$t_cont.car - post$BUGSoutput$sd$t_cont.car, 
       post$BUGSoutput$mean$lat_cont.car, 
       post$BUGSoutput$mean$t_cont.car + post$BUGSoutput$sd$t_cont.car, 
       angle = 90, length = 0.05, code = 3, 
       col = rgb(1,0.2,0.6))
points(post$BUGSoutput$mean$lat_cont.car, post$BUGSoutput$mean$t_cont.car, 
       pch = 21, bg = "dark grey", col = rgb(1,0.2,0.6))

dev.off()
