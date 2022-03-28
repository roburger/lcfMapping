# MSc Thesis
# 28/03/2023
# Get harmonic and temporal information


getHarmonics = function(TS){
  
  if (all(is.na(TS))){
    c(min=NA, max=NA, intercept=NA, co=NA, si=NA, co2=NA, si2=NA, trend=NA,
      phase1=NA, amplitude1=NA, phase2=NA, amplitude2=NA)
  }
  
  else {
    HarmCoefs = getHarmMetrics(TS, dates=dates, order=2)
    #HarmCoefs = getHarmMetrics(TS, dates=dates, order=2, return_model = T)
    p1 = phaser(HarmCoefs["co"], HarmCoefs["si"])
    p2 = phaser(HarmCoefs["co2"], HarmCoefs["si2"])
    a1 = amplituder(HarmCoefs["co"], HarmCoefs["si"])
    a2 = amplituder(HarmCoefs["co2"], HarmCoefs["si2"])
    c(HarmCoefs, phase1=p1, amplitude1=a1, phase2=p2, amplitude2=a2)
  }
  #getHarmMetrics(TS, dates=dates, order=2, lin_trend=F)
}

# Phase and Amplitude 
phaser = function(co, si){
  tau = 2*pi
  return(atan2(si, co) %% tau)
}

amplituder = function(co, si){
  return(sqrt(co^2 + si^2))
}