model{
  
  # Data model for PWI
  for(i in 1:length(pwi_cal[, 1])){
    pwi_cal[i, 1] ~ dnorm(pwi_cal_m[i], 1 / pwi_var)
    pwi_cal_m[i] = exp((pwi_cal[i, 2] + pwi_a) / pwi_b)
  }
  
  for(i in 1:length(pwi_data.an)){
    pwi_data.an[i] ~ dnorm(pwi_data_m.an[i], 1 / pwi_var)
    pwi_data_m.an[i] = exp((t_cont.an[pwi_sites.ind.an[i]] + pwi_a) / pwi_b)
  }
  
  for(i in 1:length(pwi_data.car)){
    pwi_data.car[i] ~ dnorm(pwi_data_m.car[i], 1 / pwi_var)
    pwi_data_m.car[i] = exp((t_cont.car[pwi_sites.ind.car[i]] + pwi_a) / pwi_b)
  }
  
  pwi_var ~ dgamma(2, 1)
  pwi_a ~ dnorm(-22, 1 / 1)
  pwi_b ~ dnorm(-3, 1 / (0.2 ^ 2))
  
  # Data model for AlSi
  for(i in 1:length(alsi_cal[, 1])){
    alsi_cal[i, 2] ~ dnorm(alsi_cal_m[i], 1 / alsi_var)
    alsi_cal_m[i] = alsi_cal[i ,1] * alsi_slope + alsi_int
  }
  
  for(i in 1:length(alsi_data.an)){
    alsi_data.an[i] ~ dnorm(alsi_data_m.an[i], 1 / alsi_var)
    alsi_data_m.an[i] = alsi_int + t_cont.an[alsi_sites.ind.an[i]] * alsi_slope
  }

  for(i in 1:length(alsi_data.car)){
    alsi_data.car[i] ~ dnorm(alsi_data_m.car[i], 1 / alsi_var)
    alsi_data_m.car[i] = alsi_int + t_cont.car[alsi_sites.ind.car[i]] * alsi_slope
  }
  
  alsi_var ~ dgamma(2, 10)
  alsi_slope ~ dnorm(0.01983, 1 / (0.002 ^ 2))
  alsi_int ~ dnorm(-0.07081, 1 / (0.01 ^ 2))
  
  # Data model for CIA
  for(i in 1:length(cia_cal[, 1])){
    cia_cal[i, 1] ~ dnorm(cia_cal_m[i], 1 / cia_var)
    cia_cal_m[i] = (cia_cal[i, 2] * cia_slope + cia_int) ^ 2
  }
  
  for(i in 1:length(cia_data.an)){
    cia_data.an[i] ~ dnorm(cia_data_m.an[i], 1 / cia_var)
    cia_data_m.an[i] = 
      (cia_int + t_cont.an[cia_sites.ind.an[i]] * cia_slope) ^ 2
  }

  for(i in 1:length(cia_data.car)){
    cia_data.car[i] ~ dnorm(cia_data_m.car[i], 1 / cia_var)
    cia_data_m.car[i] = 
      (cia_int + t_cont.car[cia_sites.ind.car[i]] * cia_slope) ^ 2
  }
  
  cia_var ~ dgamma(2, 10)
  cia_slope ~ dnorm(-0.0217, 1 / (0.001 ^ 2))
  cia_int ~ dnorm(0.75, 1 / (0.02 ^ 2))
  
  # Data model for terrestrial carbonates
  for(i in 1:length(pc_data.an)){
    pc_data.an[i] ~ dnorm(pc_data_m.an[i], 1)
    pc_data_m.an[i] = (dO_cont.an[pc_sites.ind.an[i]] + 
                         (cSlope / (t_seas.an[pc_sites.ind.an[i]] + 273) ^ 2 + 
                            cInt)) * 0.97002 - 29.98
  }

  for(i in 1:length(pc_data.car)){
    pc_data.car[i] ~ dnorm(pc_data_m.car[i], 1)
    pc_data_m.car[i] = (dO_cont.car[pc_sites.ind.car[i]] + 
                         (cSlope / (t_seas.car[pc_sites.ind.car[i]] + 273) ^ 2 + 
                            cInt)) * 0.97002 - 29.98
  }
  
  # Data model for clumped terrestrial carbonates
  for(i in 1:length(clump_data.an[,1])){
    clump_data.an[i, 2] ~ dnorm(clump_data_m.an[i, 2], 1)
    clump_data_m.an[i, 2] = (dO_cont.an[clump_sites.ind.an[i]] + 
                               (cSlope / (t_seas.an[clump_sites.ind.an[i]] + 
                                            273) ^ 2 + cInt)) * 0.97002 - 29.98
    clump_data.an[i, 1] ~ dnorm(clump_data_m.an[i, 1], 1/(0.05 ^ 2))
    clump_data_m.an[i, 1] = clump_slope / (t_seas.an[clump_sites.ind.an[i]] + 
                                             273) ^ 2 + clump_int
  }

  # Dennis 2011 calibration parameters
  clump_slope = 6.36e4
  clump_int = -4.7e-3

  # Data model for phosphate
  for(i in 1:length(mp_data.an)){
    mp_data.an[i] ~ dnorm(mp_data_m.an[i], 5)
    mp_data_m.an[i] = (t_mar.an[mp_sites.ind.an[i]] - pInt) / pSlope + 
      dO_mar.an[mp_sites.ind.an[i]]
  }

  for(i in 1:length(mp_data.car)){
    mp_data.car[i] ~ dnorm(mp_data_m.car[i], 5)
    mp_data_m.car[i] = (t_mar.car[mp_sites.ind.car[i]] - pInt) / pSlope + 
      dO_mar.car[mp_sites.ind.car[i]]
  }
  
  # Data model for calcite
  for(i in 1:length(mc_data.an)){
    mc_data.an[i] ~ dnorm(mc_data_m.an[i], 10)
    mc_data_m.an[i] = (dO_mar.an[mc_sites.ind.an[i]] + 
                         (cSlope / (t_mar.an[mc_sites.ind.an[i]] + 273) ^ 2 
                          + cInt)) * 0.97002 - 29.98
  }

  for(i in 1:length(mc_data.car)){
    mc_data.car[i] ~ dnorm(mc_data_m.car[i], 10)
    mc_data_m.car[i] = (dO_mar.car[mc_sites.ind.car[i]] + 
                         (cSlope / (t_mar.car[mc_sites.ind.car[i]] + 273) ^ 2 
                          + cInt)) * 0.97002 - 29.98
  }
  
  # Data model for dolomite
  for(i in 1:length(md_data.car)){
    md_data.car[i] ~ dnorm(md_data_m.car[i], 10)
    md_data_m.car[i] = (dO_mar.car[md_sites.ind.car[i]] + 
                          (dSlope / (t_mar.car[mc_sites.ind.car[i]] + 273) ^ 2 
                           + dInt)) * 0.97002 - 29.98
  }
  
  # Equilibrium fractionation parameters
  # Calcite: O'Neil et al., 1969
  # Dolomite: Vasconcelos et al., 2025, Geology
  # Phosphate: Lecuyer et al., 2013, Chem Geo
  cSlope = 2.78e6
  cInt = -2.98
  dSlope = 2.73e6
  dInt = 0.26
  pSlope = -4.5
  pInt = 117.4
  
  # Process model for continental offsets
  for(i in 1:length(lats_cont.an[, 1])){
    dO_cont.an[i] = dO_cont_m.an[i] + dO_cont_off.an[i]
    dO_cont_off.an[i] ~ dnorm(dO_cont_off.mu, dO_cont_off.pre)
    dO_cont_m.an[i] = dO_mar.mu + d.an + e.an * t_cont.an[i]

    t_seas.an[i] = (-0.9189 + sqrt(0.9189 ^ 2 - 4 * -0.0015 * c_cont.an[i])) /
      (2 * -0.0015)
    c_cont.an[i] = -t_cont.an[i] + 1.4836 - 0.2738 * lat_cont.an[i]
    t_cont.an[i] = t_cont_m.an[i] + t_cont_off.an[i]
    t_cont_off.an[i] ~ dnorm(t_cont_off.mu, t_cont_off.pre)
    t_cont_m.an[i] = a.an + b.an * lat_cont.an[i] + c.an * lat_cont.an[i] ^ 2 
    
    lat_cont.an[i] ~ dunif(lats_cont.an[i, 1], lats_cont.an[i, 2])
  }

  for(i in 1:length(lats_cont.car[, 1])){
    dO_cont.car[i] = dO_cont_m.car[i] + dO_cont_off.car[i]
    dO_cont_off.car[i] ~ dnorm(dO_cont_off.mu, dO_cont_off.pre)
    dO_cont_m.car[i] = dO_mar.mu + d.car + e.car * t_cont.car[i]
    
    t_seas.car[i] = (-0.9189 + sqrt(0.9189 ^ 2 - 4 * -0.0015 * c_cont.car[i])) /
      (2 * -0.0015)
    c_cont.car[i] = -t_cont.car[i] + 1.4836 - 0.2738 * lat_cont.car[i]
    t_cont.car[i] = t_cont_m.car[i] + t_cont_off.car[i]
    t_cont_off.car[i] ~ dnorm(t_cont_off.mu, t_cont_off.pre)
    t_cont_m.car[i] = a.car + b.car * lat_cont.car[i] + c.car * lat_cont.car[i] ^ 2 
    
    lat_cont.car[i] ~ dunif(lats_cont.car[i, 1], lats_cont.car[i, 2])
  }
  
  # Priors on continental model offsets
  t_cont_off.mu = 0
  t_cont_off.pre = 0.25

  dO_cont_off.mu = 0
  dO_cont_off.pre = 0.25

  # Priors on d18O - t relationship
  d.an ~ dnorm(d.mu, d.pre)
  e.an ~ dnorm(e.mu, e.pre)
  d.car ~ dnorm(d.mu, d.pre)
  e.car ~ dnorm(e.mu, e.pre)
  
  d.mu = -14
  d.pre = 1 # 0.25
  e.mu = 0.59
  e.pre = 250
  
  # Process model for marine temperature gradient and sw isotopes
  for(i in 1:length(lats_mar.an[, 1])){
    dO_mar.an[i] ~ dnorm(dO_mar.mu, dO_mar.pre)
    t_mar.an[i] ~ dnorm(t_mar_m.an[i], t_mar.pre)
    t_mar_m.an[i] = a.an + b.an * lat_mar.an[i] + c.an * lat_mar.an[i] ^ 2
    lat_mar.an[i] ~ dunif(lats_mar.an[i, 1], lats_mar.an[i, 2])
  }

  for(i in 1:length(lats_mar.car[, 1])){
    dO_mar.car[i] ~ dnorm(dO_mar.mu, dO_mar.pre)
    t_mar.car[i] ~ dnorm(t_mar_m.car[i], t_mar.pre)
    t_mar_m.car[i] = a.car + b.car * lat_mar.car[i] + c.car * lat_mar.car[i] ^ 2
    lat_mar.car[i] ~ dunif(lats_mar.car[i, 1], lats_mar.car[i, 2])
  }
  
  dO_mar.mu ~ dnorm(-0.5, 80)
  dO_mar.pre = 10
  
  t_mar.pre = 1
  
  c.an ~ dnorm(c.mu, c.pre)T(, 0)
  b.an ~ dnorm(b.mu, b.pre)
  a.an ~ dnorm(a.mu, a.pre)
  c.car ~ dnorm(c.mu, c.pre)T(, 0)
  b.car ~ dnorm(b.mu, b.pre)
  a.car ~ dnorm(a.mu, a.pre)
  
  c.mu = -0.008
  c.pre = 5e4 # 1e5
  b.mu = 0.22
  b.pre = 750 # 1000
  a.mu = 23
  a.pre = 0.25
  
}