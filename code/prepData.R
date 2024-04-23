library(openxlsx)

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
