# This scripts generates the functions necessary for carrying out the 
# simulation study aimed at exploring the evolution of ballistic motion


#Written by Michael Noonan

#Last updated: Jan 22nd 2022


#----------------------------------------------------------------------
# Package import

library(ctmm)
library(raster)

#----------------------------------------------------------------------
# Calculate the euclidean distance between two points
#----------------------------------------------------------------------

SLD <- function(x_1, y_1, x_2, y_2){
  sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2)
}

#----------------------------------------------------------------------
# Generate prey var[position] based on mass (in g)
#----------------------------------------------------------------------

# Model comes from Noonan et al. 2020  https://doi.org/10.1111/cobi.13495

prey.SIG <- function(mass, variance = FALSE) {
  #Calculate
  HR <- 0.5078955 + 1.372162*log10(mass)
  #Back transform
  HR <- 10^(HR)
  #Convert from 95% HR to var[position]
  SIG <- HR/(-2*log(0.05)*pi)
  #Add variance if desired
  if(variance == TRUE){SIG <- rchisq(n = length(mass), df = SIG)}
  #Return
  return(SIG)
}


#----------------------------------------------------------------------
# Generate prey E[tau_p] based on mass (in g)
#----------------------------------------------------------------------

# Model comes from Noonan et al. 2020  https://doi.org/10.1111/cobi.13495

prey.tau_p <- function(mass, variance = FALSE) {
  #Calculate
  tau_p <- 1.2994292 + 0.8129125*log10(mass)
  #Back transform
  tau_p <- 10^(tau_p)
  #Add variance if desired
  if(variance == TRUE){tau_p <- rchisq(n = length(mass), df = tau_p)}
  #Return
  return(tau_p)
}

#----------------------------------------------------------------------
# Generate prey E[tau_v] based on mass (in g)
#----------------------------------------------------------------------

# Model comes from Noonan et al. 2020  https://doi.org/10.1111/cobi.13495

prey.tau_v <- function(mass, variance = FALSE) {
  #Calculate
  tau_v <- -1.365200 + 0.787177*log10(mass)
  #Back transform
  tau_v <- 10^(tau_v)
  #Add variance if desired
  if(variance == TRUE){tau_v <- rchisq(n = length(mass), df = tau_v)}
  #Return
  return(tau_v)
}




#----------------------------------------------------------------------
# Generate predator var[position] based on mass (in g)
#----------------------------------------------------------------------

# Model comes from Noonan et al. 2020  https://doi.org/10.1111/cobi.13495

pred.SIG <- function(mass, variance = FALSE) {
  #Calculate
  HR <- 1.089972 + 1.478050*log10(mass)
  #Back transform
  HR <- 10^(HR)
  #Convert from 95% HR to var[position]
  SIG <- HR/(-2*log(0.05)*pi)
  #Add variance if desired
  if(variance == TRUE){SIG <- rchisq(n = length(mass), df = SIG)}
  #Return
  return(SIG)
}


#----------------------------------------------------------------------
# Generate predator E[tau_p] based on mass (in g)
#----------------------------------------------------------------------

# Model comes from Noonan et al. 2020  https://doi.org/10.1111/cobi.13495

pred.tau_p <- function(mass, variance = FALSE) {
  #Calculate
  tau_p <- 1.612761 + 0.766461*log10(mass)
  #Back transform
  tau_p <- 10^(tau_p)
  #Add variance if desired
  if(variance == TRUE){tau_p <- rchisq(n = length(mass), df = tau_p)}
  #Return
  return(tau_p)
}

#----------------------------------------------------------------------
# Generate predator E[tau_v] based on mass (in g)
#----------------------------------------------------------------------

# Model comes from Noonan et al. 2020  https://doi.org/10.1111/cobi.13495

pred.tau_v <- function(mass, variance = FALSE) {
  #Calculate
  tau_v <- -0.1005302 + 0.7403169*log10(mass)
  #Back transform
  tau_v <- 10^(tau_v)
  #Add variance if desired
  if(variance == TRUE){tau_v <- rchisq(n = length(mass), df = tau_v)}
  #Return
  return(tau_v)
}
#----------------------------------------------------------------------
# Generate E[mass_prey] based on mass_pred (in g)
#----------------------------------------------------------------------

# Model comes from Tucker & Rogers 2014  https://doi.org/10.1371/journal.pone.0106402

prey.mass <- function(mass, variance = FALSE) {
  #Convert to kg
  mass <- mass * (1 %#% "g")
  #Calculate
  prey_mass <- -0.87 + 1.26*log10(mass)
  #Back transform
  prey_mass <- 10^(prey_mass)
  #Convert to g
  prey_mass <-prey_mass / (1 %#% "g")
  #Add variance if desired
  if(variance == TRUE){prey_mass <- rchisq(n = length(mass), df = prey_mass)}
  #Return
  return(prey_mass)
}

#----------------------------------------------------------------------
# Generate raster of food patches based on mass_prey (g)
#----------------------------------------------------------------------

patches <- function(mass, width = 20, pred = FALSE) {
  
  # var[position]
  if(pred){SIG <- pred.SIG(mass)} else{
    SIG <- prey.SIG(mass)}
  
  # Range of raster based on 99.9% HR area
  EXT <- round(sqrt((-2*log(0.0001)*pi)* SIG))
  
  #number of patches based on fixed patch width
  #N <- EXT/n
  N <- EXT/width
  
  #Build the raster
  FOOD <- raster(matrix(1,N,N),
                 xmx=EXT, xmn=-EXT,
                 ymx=EXT, ymn=-EXT)
  
  #Return the raster of food patches
  return(FOOD)
}

#----------------------------------------------------------------------
# Count the number of patches visited (assumes immediate renewal)
#----------------------------------------------------------------------

grazing <- function(track, habitat, metric = "patches") {
   
   #Patch identities
   IDs <- suppressWarnings(cellFromXY(habitat,
                                      SpatialPoints.telemetry(track))
                           )
   # Count the number of times it moved to a new food patch
   PATCHES <- sum(diff(IDs) != 0)
   
   #Mean time between patches 
   TIME <- mean(rle(c(FALSE, diff(IDs) != 0))$lengths)

   if(metric == "patches"){return(PATCHES)}
   if(metric == "time"){return(TIME)}
 }

#----------------------------------------------------------------------
# Determine "Lifespan" and sampling interval based on mass_prey (g)
#----------------------------------------------------------------------

sampling <- function(mass, crossings = 20) {
  
  # total lifespan (based on number of range crossings)
  lifespan <- round(prey.tau_p(mass)*crossings)
  
  # sampling interval (tau_v)
  interval <- round(prey.tau_v(mass))
  
  #"Lifespan" and sampling interval for the simulations
  t <- seq(0,
           lifespan,
           interval)
  
  #return the vector of sampling times
  return(t)
}


#----------------------------------------------------------------------
# Prey fitness function
#----------------------------------------------------------------------

prey.fitness <- function(benefits, mass, costs = NULL, models, crossings = 20, calories = 10, constant = 1){
  
  # Extract movement speeds from the models
  SPEED <- vector()
  for(i in 1:length(models)){SPEED[i] <- if(nrow(summary(models[[i]], units = FALSE)$CI)==4){summary(models[[i]], units = FALSE)$CI[4,2]} else{Inf}}
  
  # Basal metabolic rate (in kj/day) from Nagy 1987 https://doi.org/10.2307/1942620 
  BMR <- 0.774 + 0.727*log10(mass)
  
  #Back transform 
  BMR <- 10^BMR
  
  # total lifespan in days (based on number of range crossings)
  lifespan <- round(prey.tau_p(mass)*crossings) /60/60/24
  
  # Metabolic cost of movement in watts/kg from Taylor et al. 1982 https://doi.org/10.1242/jeb.97.1.1 
  E = 10.7*(mass/1000)^(-0.316)*SPEED + 6.03*(mass/1000)^(-0.303)
  
  #Convert to kJ/s
  E <- (E * (mass/1000))/1000
  
  # Maximum running speed in km/hr from Hirt et al. 2017 https://doi.org/10.1038/s41559-017-0241-4
  v_max <- 25.5 * (mass/1000)^(0.26) * (1 - exp(-22*(mass/1000)^(-0.66)))
  
  #Convert to m/s
  v_max <- v_max/3.6
  
  #Total energetic cost in kj as a function of BMR and movement speed
  COST <- BMR * lifespan + E*prey.tau_p(mass)*crossings*constant
  
  # Excess energy
  excess <- benefits*calories - COST
  excess[is.infinite(excess)] <- NA
  
  # Define number of prey offspring based on their excess energy and metabolic rate
  offspring <- floor(excess/BMR)
  offspring[is.na(offspring)] <- 0
  offspring <- ctmm:::clamp(offspring, min = 0, max = Inf) #Clamp the minimum to 0
  
  # If predator encounters are being considered,
  # individuals that encountered a predator are killed and don't reproduce.
  if(!is.null(costs)){offspring[costs] <- 0}
  offspring
}

#----------------------------------------------------------------------
# Identify Encounter Events
#----------------------------------------------------------------------

encounter <- function(prey.tracks, pred.tracks, range = 50){
  distances <- list()
  encounters <- vector()
  for(i in 1:length(prey.tracks)){
    #Pairwise separation distances over time
    distances[[i]] <- SLD(PRED_tracks[[1]]$x,PRED_tracks[[1]]$y,
                          PREY_tracks[[i]]$x, PREY_tracks[[i]]$y)
    
    #Did it encounter a predator
    encounters[i] <- any(distances[[i]]<range)
  }
  return(encounters)
}

#----------------------------------------------------------------------
# Predator fitness function
#----------------------------------------------------------------------

pred.fitness <- function(encounters, mass, costs = NULL, models, time = t, calories = 10, constant = 1){
  
  # Extract movement speeds from the models
  SPEED <- vector()
  for(i in 1:length(models)){SPEED[i] <- if(nrow(summary(models[[i]], units = FALSE)$CI)==4){summary(models[[i]], units = FALSE)$CI[4,2]} else{Inf}}
  
  # Basal metabolic rate (in kj/day) from Nagy 1987 https://doi.org/10.2307/1942620 
  BMR <- 0.774 + 0.727*log10(mass)
  
  #Back transform 
  BMR <- 10^BMR
  
  # total lifespan in days (based on number of range crossings)
  #lifespan <- round(pred.tau_p(mass)*crossings) /60/60/24
  lifespan <- tail(t, n=1) /60/60/24
  
  # Metabolic cost of movement in watts/kg from Taylor et al. 1982 https://doi.org/10.1242/jeb.97.1.1 
  E = 10.7*(mass/1000)^(-0.316)*SPEED + 6.03*(mass/1000)^(-0.303)
  
  #Convert to kJ/s
  E <- (E * (mass/1000))/1000
  
  # Maximum running speed in km/hr from Hirt et al. 2017 https://doi.org/10.1038/s41559-017-0241-4
  v_max <- 25.5 * (mass/1000)^(0.26) * (1 - exp(-22*(mass/1000)^(-0.66)))
  
  #Convert to m/s
  v_max <- v_max/3.6
  
  #Total energetic cost in kj as a function of BMR and movement speed
  COST <- BMR * lifespan + E*tail(t, n=1)*constant
  
  #Energy intake in kj based on Gorecki 1965 http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.915.5227&rep=rep1&type=pdf
  mass_prey <- prey.mass(mass)
  intake <- 1.5 * mass_prey * 4.184 * sum(encounters)
  
  # Excess energy
  excess <- intake - COST
  excess[is.infinite(excess)] <- NA
  
  # Define number of prey offspring based on their excess energy and metabolic rate
  offspring <- floor(excess/BMR)
  offspring[is.na(offspring)] <- 0
  offspring <- ctmm:::clamp(offspring, min = 0, max = Inf) #Clamp the minimum to 0

  return(offspring)
}

#----------------------------------------------------------------------
# 
#----------------------------------------------------------------------
