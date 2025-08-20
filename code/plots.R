source("code/prepData.R")
source("code/prepBridge.R")

# Plots ----
load("bigout/post.rda")
sl = post$BUGSoutput$sims.list

###Without Ladinian
l = 0:90
pdf("out/T_grad_noLad.pdf", width = 6, height = 8)
layout(matrix(c(1, 2), ncol = 1))

par(mai = c(1, 1, 0.1, 0.1))
plot(l, 23 + l * 0.22 + l^2 * -0.008, type = "l", lwd = 2, ylim = c(0,43),
     xlim = c(2, 80), xlab = "Latitude", ylab = "Mean temperature")
for(i in seq(1, nrow(sl$a.car), length.out = 1000)){
  lines(l, sl$a.car[i] + l * 
          sl$b.car[i] + 
          l^2 * sl$c.car[i], 
        col = rgb(1, 0.2, 0.6, 0.02))
}
lines(l, c(post$BUGSoutput$mean$a.car) + l * c(post$BUGSoutput$mean$b.car) + 
        l^2 * c(post$BUGSoutput$mean$c.car), col = rgb(1, 0.2, 0.6), lwd = 2)

lines(zMeans$lat, zMeans$car, col = rgb(1, 0.2, 0.6))
lines(zMeans$lat, zMeans$carC, col = rgb(1, 0.2, 0.6), lty = 2)

arrows(post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car - post$BUGSoutput$sd$t_mar.car, 
       post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car + post$BUGSoutput$sd$t_mar.car, 
       angle = 90, length = 0.05, code = 3, col = rgb(1, 0.2, 0.6))
points(post$BUGSoutput$mean$lat_mar.car, 
       post$BUGSoutput$mean$t_mar.car, pch = 21, bg = "white", 
       col = rgb(1, 0.2, 0.6))
arrows(post$BUGSoutput$mean$lat_cont.car, 
       post$BUGSoutput$mean$t_cont.car - post$BUGSoutput$sd$t_cont.car, 
       post$BUGSoutput$mean$lat_cont.car, 
       post$BUGSoutput$mean$t_cont.car + post$BUGSoutput$sd$t_cont.car, 
       angle = 90, length = 0.05, code = 3, 
       col = rgb(1, 0.2, 0.6))
points(post$BUGSoutput$mean$lat_cont.car, post$BUGSoutput$mean$t_cont.car, 
       pch = 21, bg = "dark grey", col = rgb(1, 0.2, 0.6))

plot(l, 23 + l * 0.22 + l^2 * -0.008, type = "l", lwd = 2, ylim = c(0,43),
     xlim = c(2, 80), xlab = "Latitude", ylab = "Mean temperature")

for(i in seq(1, nrow(sl$a.an), length.out = 1000)){
  lines(l, sl$a.an[i] + l * 
          sl$b.an[i] + 
          l^2 * sl$c.an[i], 
        col = rgb(0.2, 0.6, 1, 0.02))
}
lines(l, c(post$BUGSoutput$mean$a.an) + l * c(post$BUGSoutput$mean$b.an) + 
        l^2 * c(post$BUGSoutput$mean$c.an), col = rgb(0.2, 0.6, 1), lwd = 2)

lines(zMeans$lat, zMeans$an, col = rgb(0.2, 0.6, 1))
lines(zMeans$lat, zMeans$anC, col = rgb(0.2, 0.6, 1), lty = 2)

arrows(post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an - post$BUGSoutput$sd$t_mar.an, 
       post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an + post$BUGSoutput$sd$t_mar.an, 
       angle = 90, length = 0.05, code = 3, col = rgb(0.2, 0.6, 1))
points(post$BUGSoutput$mean$lat_mar.an, 
       post$BUGSoutput$mean$t_mar.an, pch = 21, bg = "white", 
       col = rgb(0.2, 0.6, 1))
arrows(post$BUGSoutput$mean$lat_cont.an, 
       post$BUGSoutput$mean$t_cont.an - post$BUGSoutput$sd$t_cont.an, 
       post$BUGSoutput$mean$lat_cont.an, 
       post$BUGSoutput$mean$t_cont.an + post$BUGSoutput$sd$t_cont.an, 
       angle = 90, length = 0.05, code = 3, 
       col = rgb(0.2, 0.6, 1))
points(post$BUGSoutput$mean$lat_cont.an, post$BUGSoutput$mean$t_cont.an, 
       pch = 21, bg = "dark grey", col = rgb(0.2, 0.6, 1))

dev.off()

#Temperature gradient parameters
png("out/T_grad_parms.png", width = 9, height = 4, units = "in", res = 600)
layout(matrix(c(1,2,3), ncol = 3))

plot(density(rnorm(100000, 23, 2)), main = "a", ylim = c(0, 0.8), 
     xlim = c(18, 42), xlab = "", lwd = 2)
lines(density(sl$a.an), col = rgb(0.2, 0.6, 1), lwd = 2)
lines(density(sl$a.car), col = rgb(1, 0.2, 0.6), lwd = 2)

plot(density(rnorm(100000, 0.22, sqrt(1 / 500))), main = "b", ylim = c(0, 10),
     xlim = c(0.05, 0.45), xlab = "", lwd = 2)
lines(density(sl$b.an), col = rgb(0.2, 0.6, 1), lwd = 2)
lines(density(sl$b.car), col = rgb(1, 0.2, 0.6), lwd = 2)

plot(density(rnorm(100000, -0.008, sqrt(1e-5))), main = "c", ylim = c(0, 400),
     xlab = "", lwd = 2)
lines(density(sl$c.an), col = rgb(0.2, 0.6, 1), lwd = 2)
lines(density(sl$c.car), col = rgb(1, 0.2, 0.6), lwd = 2)
dev.off()

#d18O-t parameters
png("out/d18O_parms.png", width = 6, height = 4, units = "in", res = 600)
layout(matrix(c(1,2), ncol = 2))

plot(density(rnorm(100000, -14, 2)), main = "d", xlim = c(-25, -8), 
     ylim = c(0, 0.5), xlab = "", lwd = 2)
lines(density(sl$d.an), col = rgb(0.2,0.6,1), lwd = 2)
lines(density(sl$d.car), col = rgb(1,0.2,0.6), lwd = 2)

plot(density(rnorm(100000, 0.59, sqrt(1/250))), main = "e", xlim = c(0.2, 0.8),
     ylim = c(0, 10), xlab = "", lwd = 2)
lines(density(sl$e.an[,1]), col = rgb(0.2,0.6,1), lwd = 2)
lines(density(sl$e.car[,1]), col = rgb(1,0.2,0.6), lwd = 2)
dev.off()

#################################

#Argentina offsets
png("out/A_off_parms.png", width = 6, height = 4, units = "in", res = 600)
layout(matrix(c(1,2), ncol = 2))

plot(density(rnorm(100000, 0, 2)), main = "T_off", ylim = c(0, 0.3), 
     xlab = "")
lines(density(sl$t_cont_off[,1]), col = "red")
lines(density(sl$t_cont_off[,2]), col = "red")
lines(density(sl$t_cont_off[,3]), col = "red", lty = 2)
lines(density(sl$t_cont_off[,4]), col = "red", lty = 3)
lines(density(sl$t_cont_off[,5]), col = "red", lty = 4)

plot(density(rnorm(100000, 0, 2)), main = "d18O_off",
     ylim = c(0, 0.4), xlab = "")
lines(density(sl$dO_cont_off[,1]), col = "red")
lines(density(sl$dO_cont_off[,2]), col = "red")
lines(density(sl$dO_cont_off[,3]), col = "red", lty = 2)
lines(density(sl$dO_cont_off[,4]), col = "red", lty = 3)
lines(density(sl$dO_cont_off[,5]), col = "red", lty = 4)
dev.off()

## Temperature Gradient all
l = seq(1:70)
png("out/T_grad.png", width = 6, height = 5, units = "in", res = 600)
plot(l, 23 + l * 0.22 + l^2 * -0.008, type = "l", ylim = c(0,43),
     xlab = "Latitude", ylab = "Mean temperature")
for(i in seq(1, nrow(sl$a.an), length.out = 1000)){
  lines(l, sl$a.an[i] + l * 
          sl$b.an[i] + 
          l^2 * sl$c.an[i], 
        col = rgb(0.2,0.6,1,0.01))
}
for(i in seq(1, nrow(sl$a.car), length.out = 1000)){
  lines(l, sl$a.car[i] + l * 
          sl$b.car[i] + 
          l^2 * sl$c.car[i], 
        col = rgb(1,0.2,0.6,0.01))
}
lines(l, c(post$BUGSoutput$mean$a.an) + l * c(post$BUGSoutput$mean$b.an) + 
        l^2 * c(post$BUGSoutput$mean$c.an), col = rgb(0.2,0.6,1))
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
