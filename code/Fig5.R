source("code/prepData.R")
load("bigout/post.rda")

pm = post$BUGSoutput$median

col.c = rgb(1, 0.2, 0.6)
col.a = rgb(0.2, 0.6, 1)
par(mar = c(5, 5, 4, 1))

pdf("out/Fig5.pdf", 12, 12, units = "in", res = 600)
layout(matrix(1:5, nrow = 3, byrow = TRUE))

# d18O
mat = 0:40
c_cont = -mat + 1.4836 - 0.2738 * 45
t_seas = (-0.9189 + sqrt(0.9189 ^ 2 - 4 * -0.0015 * c_cont)) /
  (2 * -0.0015)
d18O.prior = -14 + 0.59 * mat
d18Oc.prior = (d18O.prior + (2.78e6 / (mat + 273) ^ 2 + -2.98)) * 0.97002 - 29.98
d18Oc.prior.seas = (d18O.prior + (2.78e6 / (t_seas + 273) ^ 2 + -2.98)) * 0.97002 - 29.98

d18O.car.post = c(pm$d.car) + c(pm$e.car) * mat
d18Oc.car.post = (d18O.car.post + (2.78e6 / (mat + 273) ^ 2 + -2.98)) * 0.97002 - 29.98
d18Oc.car.post.seas = (d18O.car.post + (2.78e6 / (t_seas + 273) ^ 2 + -2.98)) * 0.97002 - 29.98

d18O.an.post = c(pm$d.an) + c(pm$e.an) * mat
d18Oc.an.post = (d18O.an.post + (2.78e6 / (mat + 273) ^ 2 + -2.98)) * 0.97002 - 29.98
d18Oc.an.post.seas = (d18O.an.post + (2.78e6 / (t_seas + 273) ^ 2 + -2.98)) * 0.97002 - 29.98

plot(mat, d18Oc.prior, type = "l", 
     ylim = range(c(d18Oc.prior, d18Oc.an.post, d18Oc.car.post,
                    pc_samples.car$d18O, pc_samples.an$d18O)),
     xlab = "Mean annual temperature (°C)", 
     ylab = expression(delta^{18}*"O"["carb"]))
lines(mat, d18Oc.prior.seas, lty = 3)
lines(mat, d18Oc.car.post, col = col.c)
lines(mat, d18Oc.car.post.seas, col = col.c, lty = 3)
lines(mat, d18Oc.an.post, col = col.a)
lines(mat, d18Oc.an.post.seas, col = col.a, lty = 3)

points(pm$t_cont.car[pc_sites.ind.car], pc_samples.car$d18O, pch = 20, col = col.c)
points(pm$t_cont.an[pc_sites.ind.an], pc_samples.an$d18O, pch = 20, col = col.a)

text(pm$t_cont.car[pc_sites.ind.car], par("usr")[4], pc_sites.ind.car, 
     xpd = NA, pos = 3, col = col.c)

text(pm$t_cont.an[pc_sites.ind.an], par("usr")[4], pc_sites.ind.an, 
     xpd = NA, pos = 3, offset = 1.25, col = col.a)

# Clumped
plot(20:45, 6.36e4 / (20:45 + 273) ^ 2 + -4.7e-3, type = "l", 
     xlab = "Temperature (°C)", ylab = expression(Delta[47]),
     ylim = range(c(6.36e4 / (20:45 + 273) ^ 2 + -4.7e-3, 
                    clump_samples.an$cap_47)))

points(pm$t_cont.an[clump_sites.ind.an], clump_samples.an$cap_47,
       pch = 20, col = col.a)

points(pm$t_seas.an[clump_sites.ind.an], clump_samples.an$cap_47,
       col = col.a)

text(pm$t_cont.an[clump_sites.ind.an], par("usr")[4], clump_sites.ind.an, 
     xpd = NA, pos = 3, col = col.a)

# CIA
plot(CIA ~ Temp, data = cia_cal, xlab = "Mean annual temperature (°C)",
     xlim = range(c(as.data.frame(cia_cal)$Temp, pm$t_cont.car[cia_sites.ind.car],
                    pm$t_cont.an[cia_sites.ind.an])),
     ylim = range(c(as.data.frame(cia_cal)$CIA, cia_samples.car$CIA,
                    cia_samples.an$CIA)))

abline(0.75, -0.0217, col = "grey")
abline(pm$cia_int, pm$cia_slope)

points(pm$t_cont.car[cia_sites.ind.car], cia_samples.car$CIA,
       col = col.c, pch = 20)
points(pm$t_cont.an[cia_sites.ind.an], cia_samples.an$CIA,
       col = col.a, pch = 20)

text(pm$t_cont.car[cia_sites.ind.car], par("usr")[4], cia_sites.ind.car, 
     xpd = NA, pos = 3, col = col.c)
text(pm$t_cont.an[cia_sites.ind.an], par("usr")[4], cia_sites.ind.an, 
     xpd = NA, pos = 3, offset = 1.25, col = col.a)

# PWI
plot(PWI ~ MAT, data = pwi_cal, xlab = "Mean annual temperature (°C)"
     xlim = range(c(as.data.frame(pwi_cal)$MAT, pm$t_cont.car[pwi_sites.ind.car],
                    pm$t_cont.an[pwi_sites.ind.an])),
     ylim = range(c(as.data.frame(pwi_cal)$PWI, pwi_samples.car$PWI, 
                    pwi_samples.an$PWI)))

xs = seq(par("usr")[1], par("usr")[2], length = 100)
ysp = exp(-0.215 * xs + 5.6)
ys = exp((xs + c(pm$pwi_a)) / c(pm$pwi_b))
lines(xs, ysp, col = "grey")
lines(xs, ys)

points(pm$t_cont.car[pwi_sites.ind.car], pwi_samples.car$PWI,
       col = col.c, pch = 20)
points(pm$t_cont.an[pwi_sites.ind.an], pwi_samples.an$PWI,
       col = col.a, pch = 20)

text(pm$t_cont.car[pwi_sites.ind.car], par("usr")[4], pwi_sites.ind.car, 
     xpd = NA, pos = 3, col = col.c)
text(pm$t_cont.an[pwi_sites.ind.an], par("usr")[4], pwi_sites.ind.an, 
     xpd = NA, pos = 3, offset = 1.25, col = col.a)

# Al/Si
plot(AlSi ~ MAT, data = alsi_cal, ylab = "Al/Si", 
     xlab = "Mean annual temperature (°C)",
     xlim = range(c(as.data.frame(alsi_cal)$MAT, pm$t_cont.car[alsi_sites.ind.car],
                    pm$t_cont.an[alsi_sites.ind.an])),
     ylim = range(c(as.data.frame(alsi_cal)$AlSi, alsi_samples.car$AlSi,
                    alsi_samples.an$AlSi)))

abline(-0.07081, 0.01983, col = "grey")
abline(pm$alsi_int, pm$alsi_slope)

points(pm$t_cont.car[alsi_sites.ind.car], alsi_samples.car$AlSi,
       pch = 20, col = col.c)

points(pm$t_cont.an[alsi_sites.ind.an], alsi_samples.an$AlSi,
       pch = 20, col = col.a)

text(pm$t_cont.car[alsi_sites.ind.car], par("usr")[4], alsi_sites.ind.car, 
     xpd = NA, pos = 3, col = col.c)

text(pm$t_cont.an[alsi_sites.ind.an], par("usr")[4], alsi_sites.ind.an, 
     xpd = NA, pos = 3, offset = 1.25, col = col.a)