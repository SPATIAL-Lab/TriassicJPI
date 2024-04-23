library(terra)

# Get model results and mask
an = rast("data/tfkSx_temp2m_ann_fsy.nc")
anMask = rast("data/teyFx3_ancil_landmask_jan_fsy.nc")
anMaskC = classify(anMask, matrix(c(0, 1, NA, 1), ncol = 2))
anMaskO = classify(anMask, matrix(c(0, 1, 1, NA), ncol = 2))

anC = an * anMaskC
anO = an * anMaskO

car = rast("data/tfkSu_temp2m_ann_fsy.nc")
carMask = rast("data/teyFu3_ancil_landmask_jan_fsy.nc")
carMaskC = classify(carMask, matrix(c(0, 1, NA, 1), ncol = 2))
carMaskO = classify(carMask, matrix(c(0, 1, 1, NA), ncol = 2))

carC = car * carMaskC
carO = car * carMaskO

# Zonal means
anm = as.matrix(an, wide = TRUE)
anCm = as.matrix(anC, wide = TRUE)
anOm = as.matrix(anO, wide = TRUE)
carm = as.matrix(car, wide = TRUE)
carCm = as.matrix(carC, wide = TRUE)
carOm = as.matrix(carO, wide = TRUE)

lats = yFromRow(anC)
alats = lats[lats >= 0]
lats = abs(lats)

zMeans = data.frame("lat" = alats, "an" = rep(NA), "anC" = rep(NA),
                    "anO" = rep(NA), "car" = rep(NA), "carC" = rep(NA),
                    "carO" = rep(NA))

for(i in seq_along(alats)){
  rn = alats[i] == lats
  zMeans$an[i] = mean(anm[rn, ], na.rm = TRUE) - 273.15
  zMeans$anC[i] = mean(anCm[rn, ], na.rm = TRUE) - 273.15
  zMeans$anO[i] = mean(anOm[rn, ], na.rm = TRUE) - 273.15
  zMeans$car[i] = mean(carm[rn, ], na.rm = TRUE) - 273.15
  zMeans$carC[i] = mean(carCm[rn, ], na.rm = TRUE) - 273.15
  zMeans$carO[i] = mean(carOm[rn, ], na.rm = TRUE) - 273.15
}

