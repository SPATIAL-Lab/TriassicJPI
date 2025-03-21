library(openxlsx)

# Read in calibration data
alsi_cal = as.matrix(read.xlsx("data/alsi_cal.xlsx"))
cia_cal = as.matrix(read.csv("data/Sheldon_climofunc.txt"))
pwi_cal = as.matrix(read.csv("data/pwi.txt"))

# Read in Anisian data ----
dfile = "data/dataAnisian.xlsx"
pc_samples.an = read.xlsx(dfile, sheet = "Cont_d18O")
cia_samples.an = read.xlsx(dfile, sheet = "Cont_CIA")
alsi_samples.an = read.xlsx(dfile, sheet = "Cont_AlSi")
cont_sites.an = read.xlsx(dfile, sheet = "Cont_Sites")
clump_samples.an = read.xlsx(dfile, sheet = "Cont_Clump")
mSamples.an = read.xlsx(dfile, sheet = "Mar_d18O")
mar_sites.an = read.xlsx(dfile, sheet = "Mar_Sites")

# Split out marine phosphate and carbonate d18O
mp_samples.an = mSamples.an[mSamples.an$Material == "conP",]
mc_samples.an = mSamples.an[mSamples.an$Material != "conP",]

# Generate site index vector for each dataset
mp_sites.ind.an = match(mp_samples.an$Site, mar_sites.an$Site)
mc_sites.ind.an = match(mc_samples.an$Site, mar_sites.an$Site)
clump_sites.ind.an = match(clump_samples.an$Site, cont_sites.an$Site)
pc_sites.ind.an = match(pc_samples.an$Site, cont_sites.an$Site)
cia_sites.ind.an = match(cia_samples.an$Site, cont_sites.an$Site)
alsi_sites.ind.an = match(alsi_samples.an$Site, cont_sites.an$Site)

# Clump data as matrix: Cap47, d18O
clump_data.an = as.matrix(clump_samples.an[,c("cap_47", "d18O")])

# Lats as matrices: min, max
lats_cont.an = as.matrix(cont_sites.an[,c("lat_min", "lat_max")])
lats_mar.an = as.matrix(mar_sites.an[,c("lat_min", "lat_max")])

# Read in Carnian data ----
dfile = "data/dataCarnian.xlsx"
pc_samples.car = read.xlsx(dfile, sheet = "Cont_d18O")
cia_samples.car = read.xlsx(dfile, sheet = "Cont_CIA")
## Remove a couple of rogue data from Germany
cia_samples.car = cia_samples.car[cia_samples.car$CIA < 5,]
alsi_samples.car = read.xlsx(dfile, sheet = "Cont_AlSi")
pwi_samples.car = read.xlsx(dfile, sheet = "Cont_PWI")
## Test removing these very high PWI values
#pwi_samples.car = pwi_samples.car[pwi_samples.car$Site != "Chanares",]
cont_sites.car = read.xlsx(dfile, sheet = "Cont_Sites")
mSamples.car = read.xlsx(dfile, sheet = "Mar_d18O")
mar_sites.car = read.xlsx(dfile, sheet = "Mar_Sites")

# Split out marine phosphate and carbonate d18O
mp_samples.car = mSamples.car[mSamples.car$Material == "conP",]
md_samples.car = mSamples.car[mSamples.car$Material == "dolo",]
mc_samples.car = mSamples.car[mSamples.car$Material %in% c("bC", "brach"),]

# Generate site index vector for each dataset
mp_sites.ind.car = match(mp_samples.car$Site, mar_sites.car$Site)
md_sites.ind.car = match(md_samples.car$Site, mar_sites.car$Site)
mc_sites.ind.car = match(mc_samples.car$Site, mar_sites.car$Site)
pc_sites.ind.car = match(pc_samples.car$Site, cont_sites.car$Site)
cia_sites.ind.car = match(cia_samples.car$Site, cont_sites.car$Site)
alsi_sites.ind.car = match(alsi_samples.car$Site, cont_sites.car$Site)
pwi_sites.ind.car = match(pwi_samples.car$Site, cont_sites.car$Site)

# Lats as matrices: min, max
lats_cont.car = as.matrix(cont_sites.car[,c("lat_min", "lat_max")])
lats_mar.car = as.matrix(mar_sites.car[,c("lat_min", "lat_max")])
