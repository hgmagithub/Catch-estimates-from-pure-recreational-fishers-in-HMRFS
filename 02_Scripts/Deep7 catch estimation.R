
# ------- Section 1: Setup and Data Loading ------------
remove(list=ls())
library(sas7bdat)
library(zoo)
library(KFAS)
library(this.path)
root_dir <- here(..=1)

# *** Read the original SAS data files and manipulate files (filtering out sellers and sold catch) for Deep7 research explorations 
i1 = read.sas7bdat(file.path(root_dir, "01_Data", "i1.sas7bdat")) 
# trip and fisher profile data, 59080 records 
observed_catch = read.sas7bdat(file.path(root_dir, "01_Data", "i3.sas7bdat"))
unavailable_catch = read.sas7bdat(file.path(root_dir, "01_Data", "i2.sas7bdat")) 
nonsellers = read.sas7bdat(file.path(root_dir, "01_Data", "Nonsellers.sas7bdat"))
#read the SAS file (52758 ID_CODEs) for fishers who didn't not ever sell catch (52702) or cannot be identified as selling (56)

catch_type <- "Pure recreational"
#default is "Pure recreational" for Deep7 research track, can be assigned as "Non-sold catch" for 2024 stock assessment or "Total catch" for comparing with MRIP query
if(catch_type=="Pure recreational") {
  i1 <- i1[i1$ID_CODE %in% nonsellers$ID_CODE,] # records from 59080 to 52758
  # Only trip and fisher profile data are kept from fishers who did not ever sell any catch
}
if(catch_type=="Non-sold catch"){
  observed_catch <- observed_catch[observed_catch$DISP3 %in% c(3,4,6,7,8,9,0),]
  # exclude disposition 5 (sold/to be sold), records from 22914 to 19947
  unavailable_catch <- unavailable_catch[unavailable_catch$DISPO %in% c(1,2,3,4,6,7,8,9,0),] 
  # exclude disposition of 5, records from 17915 to 15963
}

if(catch_type=="Pure recreational"){
  observed_catch <- observed_catch[observed_catch$DISP3 %in% c(3,4,6,7,8,9,0),]
  # exclude disposition 5 (sold/to be sold), records from 22914 to 19947  
  observed_catch <- observed_catch[observed_catch$ID_CODE %in% nonsellers$ID_CODE,]
  # exclude catch records from sellers, # of records from 19947 to 17140
  unavailable_catch <- unavailable_catch[unavailable_catch$DISPO %in% c(1,2,3,4,6,7,8,9,0),] 
  # exclude disposition of 5, records from 17915 to 15963
  unavailable_catch <- unavailable_catch[unavailable_catch$ID_CODE %in% nonsellers$ID_CODE,]
  # exclude catch records from sellers, records from 15963 to 13859
}

# ********* Create species data frame for Deep7 with names and parameters for length-weight regression 
species_df = data.frame("key" = c(8835020413, 8835360304, 8835360302, 8835360704, 8835360706, 8835360901, 8835360707),
                        "common_name" = c("HAWAIIAN GROUPER", "LONGTAILED RED SNAPPER", "RUBY SNAPPER", "PINK SNAPPER", "VON SIEBOLDS SNAPPER", "IRONJAW SNAPPER", "BINGHAMS SNAPPER"),
                        "hawaiian_name" = c("Hapu'upu'u", "Onaga", "Ehu", "Opakapaka", "Kalekale", "Lehi", "Gindai"),
                        "alpha" = c(2.884, 2.673, 3.026, 2.928, 2.932, 2.458, 2.859), # for conversion from length (cm) to weight
                        "beta" = c(3.065*10^-5, 6.005*10^-5, 1.551*10^-5, 2.311*10^-5, 2.243*10^-5, 1.298*10^-4, 3.526*10^-5)) # for conversion from length (cm) to weight



# ------------------- Section 2: Data Pre-Processing and Structuring ------------------------
#This section cleans the raw data, creates new variables, and initializes the multi-dimensional arrays that will store survey data and calculated results.
# for mode: index 1 = private boat, 2 = shore
#i1$mode = ifelse(i1$MODE_F == 8, 1, ifelse(i1$MODE_FX %in% 1:5, 2, NA))
i1$mode = ifelse(i1$MODE_F == 8, 1, ifelse(i1$MODE_F %in% 1:5, 2, NA)) # Ma changed MODE_FX to MODE_F in ifsle(....)
# for area: index 1 = ocean (> 3 mi), 2 = ocean (<= 3 mi), 3 = inland
i1$area_exp = ifelse(i1$AREA_X == 2, 1, ifelse(i1$AREA_X == 1, 2, ifelse(i1$AREA_X == 5, 3, NA)))

observed_catch = observed_catch[observed_catch$DISP3 %in% c(3,4,5,6,7,8,9,0) & observed_catch$MODE_F != 7,] # fish that were not released and not from a charter trip, 8=don't know/didn't ask
unavailable_catch = unavailable_catch[unavailable_catch$DISPO %in% c(3,4,5,6,7,8,9,0) & unavailable_catch$MODE_F != 7,] # fish that were not released and not from a charter trip
# added DISPO "8" on Mar12, 2025
# for disposition: index 1 = sold, 2 = not sold, which are different from DISP3 (catch disposition, 0 to 9, for observed catch) and DISPO (catch disposition for unavailable catch)
observed_catch$sold = ifelse(observed_catch$DISP3 == 5, T, F)
unavailable_catch$sold = ifelse(unavailable_catch$DISPO == 5, T, F)

observed_catch$EST_WGT = sapply(1:nrow(observed_catch), function(f) { # estimate the weight for individual fish that were lengthed, but not weighed
  r = observed_catch[f,]
  sp = r$SP_CODE
  if((sp %in% species_df$key) && !is.nan(r$LNGTH) && is.nan(r$WGT)) { # deep 7 species with length, but no weight
    i = which(species_df$key == r$SP_CODE)
    return(species_df[i,]$beta * (r$LNGTH / 10) ^ species_df[i,]$alpha)
  } else {
    return(NaN)
  }
})

#**********Create arrays for assigning survey data*****
#years = sort(unique(c(observed_catch$YEAR, unavailable_catch$year)), decreasing = F) # unique years with data
years = sort(unique(c(observed_catch$YEAR, unavailable_catch$YEAR)), decreasing = F) # unique years with data
trips = unique(i1[!is.na(i1$mode) & !is.na(i1$ID_CODE),]$ID_CODE)

n_years = length(years)
n_waves = 6
n_species = nrow(species_df)
n_modes = 2 # 1 for boat and 2 for shore
n_areas = 3
n_trips = length(trips)
n_dispositions = 2 #TRUE or FALSE

num_trips = array(0, dim = c(n_years, n_waves, n_modes, n_areas))
# number of interviews in a domain

for(y in 1:n_years) {
  for(w in 1:n_waves) {
    for(m in 1:n_modes) {
      for(a in 1:n_areas) {
        num_trips[y, w, m, a] = length(unique((i1[i1$YEAR == years[y] & i1$WAVE == w & i1$mode == m & i1$area_exp == a  & !is.na(i1$ID_CODE),])$ID_CODE))
      }
    }
  }
}

domain_trips = array(F, dim = c(n_years, n_waves, n_modes, n_areas, n_trips))
# n_trips is ~50,000 thus domain_trips is a large array with (20*6*2*3*~50000) elements
for(t in 1:n_trips) {
  entry = i1[i1$ID_CODE == trips[t],][1,]
  
  #domain_trips[match(entry$YEAR, years), entry$WAVE, entry$mode, entry$area, t] = T
  domain_trips[match(entry$YEAR, years), entry$WAVE, entry$mode, entry$area_exp, t] = T #Ma changed entry$area to entry$area_exp
  # t is true when year, wave, mode, area match, which is relevant to "anglers_by_trip" and "observed/unavailable_caught_by_trip" below  
}

anglers_by_trip = array(0, dim = c(n_years, n_waves, n_modes, n_areas, n_trips))
observed_caught_by_trip = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas, n_trips))
observed_total_caught = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas, n_dispositions))
num_weighed = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas))
num_estimated = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas))
total_weight = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas))
unavailable_caught_by_trip = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas, n_trips))
unavailable_total_caught = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas, n_dispositions))


# ----------- Section 3: Main Data Aggregation Loop --------------------
#This is the core of the script, where it iterates through every unique trip to aggregate catch and effort data into the pre-defined strata.
for(t in 1:n_trips) {
  trip = trips[t]
  year = as.numeric(substr(trip, 6, 9))
  y = which(years == year)
  w = ceiling(as.numeric(substr(trip, 10, 11)) / 2) # convert from numeric month to wave
  m = i1[i1$ID_CODE == trip,]$mode
  a = i1[i1$ID_CODE == trip,]$area_exp
  
  anglers_by_trip[y, w, m, a, t] = max(c(i1[i1$ID_CODE == trip,]$CNTRBTRS, observed_catch[observed_catch$ID_CODE == trip,]$CNTRBTRS, unavailable_catch[unavailable_catch$ID_CODE == trip,]$CNTRBTRS))
  # $CNTRBTRS is number of contributors for group catch which is only applicable to observed catch
  # Observed catch
  # FSHINSP is the number of specimens of a species observed
  #   Each row represents one specimen measured, but all have the same FSHINSP
  # Catch can be from a group of fishers, and effort should be in angler trips
  #   CNTRBTRS is the number of fishers the catch represents
  
  observed = observed_catch[observed_catch$ID_CODE == trip,]
  
  if(nrow(observed) > 0) { # if any catch for the trip was observed
    species = unique(observed$SP_CODE)
    species = species[species %in% species_df$key]
     
    if(length(species) > 0) { # if any deep 7 species were observed
      for(i in 1:length(species)) {
        s = which(species_df$key == species[i])
        
        observed_s = observed[observed$SP_CODE == species[i],]
        
        observed_caught_by_trip[y, w, s, m, a, t] = max(observed_s$FSHINSP) # ****max picks the largest FSHINSP from records for a species in a trip
        # There should be only one catch disposition for a species in a trip for observed catch
        # There can be multiple records for a species in a trip but the catch number was the same for each record (multiple records are for length and weight measurements)
        
        if(all(observed_s$sold)) {
          observed_total_caught[y, w, s, m, a, 1] = observed_total_caught[y, w, s, m, a, 1] + max(observed_s$FSHINSP)
        } else if(any(!observed$sold)) { # using this "else if" will cause observed counts for a single species with multiple dispositions in a single trip to all be counted as non-sold
          observed_total_caught[y, w, s, m, a, 2] = observed_total_caught[y, w, s, m, a, 2] + max(observed_s$FSHINSP) # ***max picks one value (the largest), multiple non-sold dispos for pelagic??
        } 

        for(j in 1:nrow(observed_s)) {
          r = observed_s[j,]
          
          if(!is.nan(r$WGT)) { # if a weight was directly measured
            num_weighed[y, w, s, m, a] = num_weighed[y, w, s, m, a] + 1
            total_weight[y, w, s, m, a] = total_weight[y, w, s, m, a] + r$WGT
          } else if(!is.nan(r$LNGTH)) { # if a weight was indirectly estimated
            num_estimated[y, w, s, m, a] = num_estimated[y, w, s, m, a] + 1
            total_weight[y, w, s, m, a] = total_weight[y, w, s, m, a] + r$EST_WGT
          }
        }
      }
    }
  }
  
  # Unavailable catch
  # Catch can only be from individual fishers
  # NUM2: sequential numbering of species within interview
  # NUM_TYP2: total number of species within interview
  # NUM_FISH: number of of fish of each species
  
  unavailable = unavailable_catch[unavailable_catch$ID_CODE == trip,]
  
  if(nrow(unavailable) > 0) { # if any catch for the trip was reported but not observed
    species = unique(unavailable$SP_CODE)
    species = species[species %in% species_df$key]
    
    if(length(species) > 0) { # if any deep 7 species were observed
      for(i in 1:length(species)) {
        s = which(species_df$key == species[i])
        
        unavailable_s = unavailable[unavailable$SP_CODE == species[i],]
        
        unavailable_caught_by_trip[y, w, s, m, a, t] = max(unavailable_s$NUM_FISH) #***max works for deep7 where only 1 disp present for a sp in an interview and it works for Deep7 (may use sum)
        

        for(j in 1:nrow(unavailable_s)) {
          r = unavailable_s[j,]
          
          if(r$sold) {
            unavailable_total_caught[y, w, s, m, a, 1] = unavailable_total_caught[y, w, s, m, a, 1] + r$NUM_FISH
          } else {
            unavailable_total_caught[y, w, s, m, a, 2] = unavailable_total_caught[y, w, s, m, a, 2] + r$NUM_FISH
          }
        }
      }
    }
  }
}



# --------------- Section 4: CPUE, Variance, and Total Catch Calculation ------------------
# mean weight was calculated with unfiltered catch records (including sold catch), the mean weight values estimated for non-sold catch or pure recreational  are not used
mean_weight_wave = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas)) # mean weight by wave within year
mean_weight_year = array(0, dim = c(n_years, n_species, n_modes, n_areas)) # mean weight by year
mean_weight_all = array(0, dim = c(n_species)) # mean weight across years
#This section calculates the key metrics: mean weight, CPUE (catch rate), the variance of the CPUE, and finally, the total catch by combining CPUE with effort data.
# Setting up arrays to compute summary statistics

#*************Calculate catch rate*********
catch_rate = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas)) # CPUE in catch number per angler trip

for(s in 1:n_species) {
  mean_weight_wave[, , s, , ] = total_weight[, , s, , ] / (num_weighed[, , s, , ] + num_estimated[, , s, , ])
  mean_weight_year[, s, , ] = apply(total_weight[, , s, , ], c(1, 3, 4), sum) / apply(num_weighed[, , s, , ] + num_estimated[, , s, , ], c(1, 3, 4), sum)
  mean_weight_all[s] = sum(total_weight[, , s, , ]) / (sum(num_weighed[, , s, , ] + num_estimated[, , s, , ]))
  #catch_rate[, , s, , ] = apply(observed_total_caught[, , s, , , ] + unavailable_total_caught[, , s, , , ], c(1, 2, 3, 4), sum) / apply(anglers_by_trip, c(1, 2, 3, 4), sum)
  catch_rate[, , s, , ] = apply(observed_total_caught[, , s, , , ], c(1, 2, 3, 4), sum) / apply(anglers_by_trip, c(1, 2, 3, 4), sum) + apply(unavailable_total_caught[, , s, , , ], c(1,2,3,4), sum)/apply(num_trips, c(1,2,3,4), sum)
}
catch_rate[is.nan(catch_rate)] = 0 
catch_rate[is.na(catch_rate)] = 0
catch_rate[18, 3, , , ] = (catch_rate[17, 3, , , ]+catch_rate[16,3,,,])/2 # No data available for April - June 2020 because of Covid, so use catch rate from same wave 3 (May + June) in 2019 & 2018 for wave 3 in 2020
catch_rate[18, 2, , , ] = (catch_rate[17, 2, , , ]+catch_rate[16,2,,,])/2 # incomplete data in March-April2020
#catch_rate[is.nan(catch_rate)] = 0 

catch_rate_var = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas)) # variance of CPUE (catch number per angler trip), calculated as the sum of the variance from observed and unavailable catch
unavailable_catch_rate_var = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas))
observed_catch_rate_var = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas))

for(y in 1:n_years) {
  for(w in 1:n_waves) {
    for(s in 1:n_species) {
      for(m in 1:n_modes) {
        for(a in 1:n_areas) {
          unavailable_catch_rate = sum(unavailable_total_caught[y, w, s, m, a, ]) / num_trips[y, w, m, a]
          unavailable_catch_rate_var[y, w, s, m, a] = 1 / num_trips[y, w, m, a] * sum((unavailable_caught_by_trip[y, w, s, m, a, domain_trips[y, w, m, a, ]] - unavailable_catch_rate) ^ 2 / (num_trips[y, w, m, a] - 1)) #added domain_trips for catch by Ma
          # unavailable_caught_by_trip[y, w, s, m, a, domain_trips[y, w, m, a, ] only keep the catch_by_trip for trips in the domain so that variance can be properly calculated.
          observed_catch_rate = (sum(observed_total_caught[y, w, s, m, a, ]) / num_trips[y, w, m, a]) / mean(anglers_by_trip[y, w, m, a, domain_trips[y, w, m, a, ]]) 
          #the catch rate of observed catch is ratio of two means and is plugged in for the variance calculation below - Ma comment in 2025 
          observed_catch_rate_var[y, w, s, m, a] = 1 / (num_trips[y, w, m, a] * mean(anglers_by_trip[y, w, m, a, domain_trips[y, w, m, a, ]]) ^ 2) * (var(observed_caught_by_trip[y, w, s, m, a, domain_trips[y, w, m, a, ]]) + observed_catch_rate ^ 2 * var(anglers_by_trip[y, w, m, a, domain_trips[y, w, m, a, ]]) - 2 * observed_catch_rate*cov(observed_caught_by_trip[y, w, s, m, a, domain_trips[y, w, m, a, ]], anglers_by_trip[y, w, m, a, domain_trips[y, w, m, a, ]]))
          # Refer to equations on page 6 of Ma and Ogawa 2016 (NOAA Tech Memo)
    
          catch_rate_var[y, w, s, m, a] = unavailable_catch_rate_var[y, w, s, m, a] + observed_catch_rate_var[y, w, s, m, a]
        }
      }
    }
  }
}
catch_rate_var[is.nan(catch_rate_var)] = 0
catch_rate[is.nan(catch_rate)] = 0
#added two lines below in 2025 to make sure all nans and/or NAs are assigned with 0
catch_rate_var[is.na(catch_rate_var)] = 0
catch_rate[is.na(catch_rate)] = 0


#*************Assign fishing effort into arrays and calculate catch (product of catch rate and fishing effort)
effort_df = read.csv(file.path(root_dir,"01_Data", "Fishing effort_MRIP.csv")) # This version has updated PSE for waves 5&6, 2022 and estimate status for 2022 should be final
effort_df$Wave = ordered(effort_df$Wave, levels = c("JANUARY/FEBRUARY", "MARCH/APRIL", "MAY/JUNE", "JULY/AUGUST", "SEPTEMBER/OCTOBER", "NOVEMBER/DECEMBER")) # wave as a factor
effort_df$Wave_num = as.numeric(effort_df$Wave) # wave numbered 1 through 6
effort_df$mode = ifelse(effort_df$Fishing.Mode == "PRIVATE/RENTAL BOAT", 1, ifelse(effort_df$Fishing.Mode == "SHORE", 2, NA)) # matching fishing modes with previous defined indices
effort_df$area = ifelse(effort_df$Fishing.Area == "OCEAN (> 3 MI)", 1, ifelse(effort_df$Fishing.Area == "OCEAN (<= 3 MI)", 2, ifelse(effort_df$Fishing.Area == "INLAND", 3, NA))) # matching fishing areas with previously defined indices

effort = array(0, dim = c(n_years, n_waves, n_modes, n_areas)) # fishing effort in angler trips
effort_var = array(0, dim = c(n_years, n_waves, n_modes, n_areas))

for(y in 1:n_years) {
  for(w in 1:n_waves) {
    for(m in 1:n_modes) {
      for(a in 1:n_areas) {
        rows = effort_df[effort_df$Year == years[y] & effort_df$Wave_num == w & effort_df$mode == m & effort_df$area == a,]
        
        if(nrow(rows) > 0) {
          effort[y, w, m, a] = sum(rows$Angler.Trips)
          
          for(i in 1:nrow(rows)) {
            r = rows[i,]
            
            effort_var[y, w, m, a] = effort_var[y, w, m, a] + (r$Angler.Trips  * r$PSE / 100) ^ 2 # convert percent standard error to variance
          }
        }
      }
    }
  }
}
effort[is.nan(effort)]=0
effort_var[is.nan(effort_var)]=0
# added two lines below in 2025 to avoid nan and/or NA in the effort arrays
effort[is.na(effort)]=0
effort_var[is.na(effort_var)]=0
# Total catch

total_catch_num = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas))
total_catch_num_var = array(0, dim = c(n_years, n_waves, n_species, n_modes, n_areas))

for(s in 1:n_species) {
  total_catch_num[, , s, , ] = catch_rate[, , s, , ] * effort
  total_catch_num_var[, , s, , ] = catch_rate_var[, , s, , ] * effort ^ 2 + effort_var * catch_rate[, , s, , ] ^ 2 - catch_rate_var[, , s, , ] * effort_var
}

total_catch_num_var_annual = apply(total_catch_num_var, c(1, 3, 4, 5), sum)
total_catch_pse_annual = sqrt(total_catch_num_var_annual) / apply(total_catch_num, c(1, 3, 4, 5), sum) * 100

total_catch_weight = sweep(apply(total_catch_num, c(1, 3, 4, 5), sum), 2, mean_weight_all, FUN = "*")

proportion_kept = (observed_total_caught[, , , , , 2] + unavailable_total_caught[, , , , , 2]) / (apply(observed_total_caught + unavailable_total_caught, c(1, 2, 3, 4, 5), sum))
proportion_kept[is.nan(proportion_kept)] = 0

total_catch_num_kept = total_catch_num * proportion_kept
total_catch_weight_kept = sweep(apply(total_catch_num_kept, c(1, 3, 4, 5), sum), 2, mean_weight_all, FUN = "*")

# Checking catch disposition against Appendix Table 1c from Ma et al. submitted manuscript (2019)
# total_catch_proportion_kept = 1 - apply(total_catch_num_kept, c(1, 3), sum, na.rm = T) / apply(total_catch_num, c(1, 3), sum, na.rm = T) # [year, species]
total_catch_proportion_sold = 1 - apply(total_catch_num_kept, c(1, 3), sum, na.rm = T) / apply(total_catch_num, c(1, 3), sum, na.rm = T) # changed the variable ".._kept" to "_sold" by Ma in 2025
#official_total_catch_proportion_kept = array(c(NA, 0, 0, NA, 11, 0, 100, 0, 100, 16, 15, 40, 74, 0, NA, NA, NA, NA, NA, NA,              # changed the variable ".._kept" to ".._sold" by Ma
official_total_catch_proportion_sold = array(c(NA, 0, 0, NA, 11, 0, 100, 0, 100, 16, 15, 40, 74, 0, NA, NA, NA, NA, NA, NA,
                                               NA, 71, 51, 89, 54, 38, 44, 53, 62, 59, 53, 69, 39, 66, NA, NA, NA, NA, NA, NA,
                                               NA, 61, 31, 68, 68, 45, 46, 79, 77, 37, 37, 62, 10, 64, NA, NA, NA, NA, NA, NA,
                                               NA, 69, 27, 23, 55, 56, 61, 78, 65, 57, 55, 83, 60, 69, NA, NA, NA, NA, NA, NA,
                                               NA, 0, 0, 0, 5, 0, 0, 82, 71, 37, 48, 45, 75, 56, NA, NA, NA, NA, NA, NA,
                                               NA, 100, 100, NA, 100, 0, 0, 0, 100, 0, 100, NA, 0, 40, NA, NA, NA, NA, NA, NA,
                                               NA, 0, 0, NA, 60, 47, 0, 0, 0, 78, 0, 100, 82, 0, NA, NA, NA, NA, NA, NA) / 100, dim = c(n_years, n_species)) # no values from 2003 or after 2016

for(s in 1:n_species) {
  plot(x = years, y = official_total_catch_proportion_sold[, s], type = "l", xlab = "Year", ylab = paste0(species_df[s,]$hawaiian_name, " Catch Number % sold"))
  lines(x = years, y = total_catch_proportion_sold[, s], col = "red")
  legend(x = "topright", col = c("black", "red"), lty = 1, legend = c("Ma et al.", "R script"), bty = "n")
}
# the above plots are useful when catch_type is "Total catch"
# the kept proportion (i.e sold proportion) for waves 2&3 in 2020 (during Covid) is lacking  because the average from 2019 and 2021 (combined dispositions) was used as substitutes in 2020

# Checking catch number against official values from https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries

official_catch = read.csv(file.path(root_dir,"01_Data","HMRFS catch_MRIP.csv")) #This version has updated catch and variance estimates for waves 5&6 in 2022. The estimate status for 2022 should be final 
official_catch$Total.Harvest..A.B1. = as.numeric(gsub(",", "", official_catch$Total.Harvest..A.B1.)) # remove commas from catch values
official_catch$wave = match(official_catch$Wave, c("JANUARY/FEBRUARY", "MARCH/APRIL", "MAY/JUNE", "JULY/AUGUST", "SEPTEMBER/OCTOBER", "NOVEMBER/DECEMBER")) # convert wave to wave number

official_catch_num = array(0, dim = c(n_years, n_species)) # catch number by year
official_catch_num_wave = array(0, dim = c(n_years, n_waves, n_species)) # catch number by wave within year
official_catch_num_var = array(0, dim = c(n_years, n_species))
official_catch_num_var_wave = array(0, dim = c(n_years, n_waves, n_species))

for(y in 1:n_years) {
  for(s in 1:n_species) {
    rows = official_catch[official_catch$Year == years[y] & official_catch$Common.Name == species_df$common_name[s],]
    
    if(nrow(rows) > 0) {
      official_catch_num[y, s] = sum(rows$Total.Harvest..A.B1.)
      
      for(i in 1:nrow(rows)) {
        r = rows[i,]
        
        official_catch_num_var[y, s] = official_catch_num_var[y, s] + (r$Total.Harvest..A.B1.  * r$PSE / 100) ^ 2 # convert percent standard error to variance
      }
      
      for(w in 1:n_waves) {
        rows_w = rows[rows$wave == w,]
        
        if(nrow(rows_w) > 0) {
          official_catch_num_wave[y, w, s] = sum(rows_w$Total.Harvest..A.B1.)
          
          for(i in 1:nrow(rows_w)) {
            r = rows[i,]
            
            official_catch_num_var_wave[y, w, s] = official_catch_num_var_wave[y, w, s] + (r$Total.Harvest..A.B1.  * r$PSE / 100) ^ 2 # convert percent standard error to variance
          }
        }
      }
    }
  }
}

for(s in 1:n_species) {
  plot(x = years, y = official_catch_num[, s], type = "l", xlab = "Year", ylab = paste0(species_df[s,]$hawaiian_name, " Catch"))
  lines(x = years, y = apply(total_catch_num[, , s, , ], c(1), sum), col = "red")
  legend(x = "topright", col = c("black", "red"), lty = 1, legend = c("MRIP query", "R Script"), bty = "n")
}

# Checking catch number variance against https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries

sqrt(apply(total_catch_num_var_annual, c(1, 2), sum, na.rm = T)) / sqrt(official_catch_num_var)




# -------------------- Section 5: Temporal Smoothing -----------------
# ************Wave-level CPUE smoothing

n_year_waves = n_years * n_waves

# in the second-to-last dimension, index 1 = ocean (> 3 mi), index 2 = ocean (<= 3 mi), and index 3 = aggregated across the two areas; there is no deep 7 catch from inland fishing
# in the last dimension, index 1 = Moving average, index 2 = Kalman
# in total (without separation of areas for boat), smoothing is only applied to private boat fishing in the ocean (<= 3 mi) and ocean (> 3 mi) fishing areas since these contribute the vast majority of deep 7 catch
# smoothing is not applied to data from the shore fishing mode in any fishing area
wave_smoothed_catch_rate = array(0, dim = c(n_year_waves, n_species, 3, 2))
wave_smoothed_total_catch = array(0, dim = c(n_year_waves, n_species, 3, 2))
cpue_raw = array(0, dim=c(n_year_waves, n_species))
cpue_raw2 = array(0, dim=c(n_year_waves, n_species))
n_smooth_waves = 6 # number of waves on either side to smooth across for the moving average
q =0.01 # random walk parameter for Kalman filter
h =0.01 # observation process parameter for Kalman filter
# the smoothed catch rate estimates from Kalman filer are insensitive to the absolute q and h values when they are equal 
total_caught = observed_total_caught + unavailable_total_caught # may keep them separately with "num_trips" and "anglers_by_trip"

# first dimension in these arrays is the sequential waves in order from oldest to most recent
total_caught_yw = array(0, dim = c(n_year_waves, n_species, n_modes, n_areas, n_dispositions))
total_observed_yw = array(0, dim = c(n_year_waves, n_species, n_modes, n_areas, n_dispositions)) # added in Nov 2022
total_unavailable_yw = array(0, dim = c(n_year_waves, n_species, n_modes, n_areas, n_dispositions)) # added in Nov 2022
anglers_by_trip_yw = array(0, dim = c(n_year_waves, n_modes, n_areas, n_trips)) 
num_trips_yw = array(0, dim = c(n_year_waves, n_modes, n_areas)) # added in Nov 2022
effort_yw = array(0, dim = c(n_year_waves, n_modes, n_areas))

# reshaping the necessary arrays so that instead of year and wave being separate dimensions, they are instead a single dimension
for(y in 1:n_years) {
  for(w in 1:n_waves) {
    index = (y - 1) * 6 + w # sequential numbering of the current wave
    
    total_caught_yw[index, , , , ] = total_caught[y, w, , , , ]
    total_observed_yw[index, , , , ] = observed_total_caught[y, w, , , , ]
    total_unavailable_yw[index, , , , ] = unavailable_total_caught[y, w, , , , ]
    anglers_by_trip_yw[index, , , ] = anglers_by_trip[y, w, , , ]
    num_trips_yw[index, , ] = num_trips[y, w, , ]
    effort_yw[index, , ] = effort[y, w, , ]
  }
}

for(s in 1:n_species) {
  wave_smoothed_catch_rate[, s, 3, 1] = rollapplyr(apply(total_caught_yw[, s, 1, 1:2, ], c(1), sum) / apply(anglers_by_trip_yw[, 1, 1:2, ], c(1), sum), 1 + 2 * n_smooth_waves, FUN = mean, partial = T, align = "center", na.rm = T)
  wave_smoothed_total_catch[, s, 3, 1] = wave_smoothed_catch_rate[, s, 3, 1] * apply(effort_yw[, 1, 1:2], c(1), sum)
  
  for(a in 1:(n_areas - 1)) {
    wave_smoothed_catch_rate[, s, a, 1] = rollapplyr(apply(total_caught_yw[, s, 1, a, ], c(1), sum) / apply(anglers_by_trip_yw[, 1, a, ], c(1), sum), 1 + 2 * n_smooth_waves, FUN = mean, partial = T, align = "center", na.rm = T)
    wave_smoothed_total_catch[, s, a, 1] = wave_smoothed_catch_rate[, s, a, 1] * effort_yw[, 1, a]
  }
  cpue_raw[, s] = apply(total_caught_yw[, s, 1, 1:2, ], c(1), sum) / apply(anglers_by_trip_yw[, 1, 1:2, ], c(1), sum)
  # Deep7 catch only in area 1 and 2, not in 3=inland
  cpue_raw2[, s] = apply(total_observed_yw[, s, 1, 1:2, ], c(1), sum) / apply(anglers_by_trip_yw[, 1, 1:2, ], c(1), sum) + apply(total_unavailable_yw[, s, 1, 1:2, ], c(1), sum) / apply(num_trips_yw[, 1, 1:2], c(1), sum)
  # cpue_raw2 is more accurate than cpue_raw
  cpue_raw2[104, s]=(cpue_raw2[98, s]+cpue_raw2[92,s])/2
  cpue_raw2[105, s]=(cpue_raw2[99, s]+cpue_raw2[93,s])/2
  cpues = cpue_raw2[,s]
  wave_smoothed_catch_rate[, s, 3, 2] = KFS(SSModel(cpues ~ SSMtrend(1, Q=q), H=h))$alphahat #3 for comibned area and 2 for Kalman
  wave_smoothed_total_catch[, s, 3, 2] = wave_smoothed_catch_rate[, s, 3, 2] * sapply(1:n_year_waves, function(f) sum(effort_yw[f, 1, 1:2]))
#*** capture these wavely results*** 3=combined area and 2 for Kalman filter AND in each of n_year_waves, summing the fishing effort and federal and state waters
  # catch of opakapaka from shore in 2009 is not included yet at this stage
  
  for(a in 1:(n_areas - 1)) {
    cpues = apply(total_caught_yw[, s, 1, a, ], c(1), sum) / apply(anglers_by_trip_yw[, 1, a, ], c(1), sum)
    wave_smoothed_catch_rate[, s, a, 2] = KFS(SSModel(cpues ~ SSMtrend(1,Q=q ), H=h))$alphahat
    wave_smoothed_total_catch[, s, a, 2] = wave_smoothed_catch_rate[, s, a, 2] * sapply(1:n_year_waves, function(f) sum(effort_yw[f, 1, a]))
  }
}

year_smoothed_total_catch = array(0, dim = c(n_years, n_species, 3, 2))

# Computing catch across the smoothed wave data within each year
for(s in 1:n_species) {
  for(area in 1:3) {
    for(smoother in 1:2) {
      year_smoothed_total_catch[, s, area, smoother] = sapply(1:n_years, function(f) sum(wave_smoothed_total_catch[((f - 1) * 6 + 1):((f - 1) * 6 + 6), s, area, smoother], na.rm = T))
    }
  }
}

unaccounted_total_catch = apply(total_catch_num[, , , 2,], c(1, 3), sum, na.rm = T) # catch from the shore fishing mode, to be added to the smoothed catch from the private boat fishing mode
ma_smoothed_catch_plot = year_smoothed_total_catch[, , 3, 1] + unaccounted_total_catch
ma_smoothed_by_area_catch_plot = apply(year_smoothed_total_catch[, , 1:2, 1], c(1, 2), sum) + unaccounted_total_catch
kalman_smoothed_catch_plot = year_smoothed_total_catch[, , 3, 2] + unaccounted_total_catch #3 for all areas and 2 for smoother2
#capture the above results (20X7 array), don't need to define the array because year_smoothed_total_catch is defined as an array
#********#

kalman_smoothed_by_area_catch_plot = apply(year_smoothed_total_catch[, , 1:2, 2], c(1, 2), sum) + unaccounted_total_catch

year_totalcatch = array(0, dim=c(n_years, n_species))# create and capture this array
for(s in 1:n_species) {
 # y_max = max(c(ma_smoothed_catch_plot[, s], ma_smoothed_by_area_catch_plot[, s], kalman_smoothed_catch_plot[, s], kalman_smoothed_by_area_catch_plot[, s], official_catch_num[, s]))
  year_totalcatch[,s] = apply(total_catch_num[, , s, , ], c(1), sum) #***capture these results for double check****
  y_max = max(c(ma_smoothed_catch_plot[, s], ma_smoothed_by_area_catch_plot[, s], kalman_smoothed_catch_plot[, s], kalman_smoothed_by_area_catch_plot[, s], year_totalcatch[,s]))
  plot(years, ma_smoothed_catch_plot[, s], ylim = c(0, y_max), col = "red", type = "l", xlab = "Year", ylab = paste0(species_df[s,]$hawaiian_name, " Catch"))
  lines(years, ma_smoothed_by_area_catch_plot[, s], col = "red", lty = "dashed")
  lines(years, kalman_smoothed_catch_plot[, s], col = "green")
  lines(years, kalman_smoothed_by_area_catch_plot[, s], col = "green", lty = "dashed")
  #lines(years, official_catch_num[, s], col = "black")
  lines(x = years, y = apply(total_catch_num[, , s, , ], c(1), sum), col = "black")
  legend(x = "topright", col = c("red", "red", "green", "green", "black"), lty = c("solid", "dashed", "solid", "dashed", "solid"), legend = c("Moving average", "Moving average by area", "Kalman", "Kalman by area", "Original"), bty = "n")
}
# Smoothing without fishing area separation is only applied to fishing in the ocean <= 3 miles from shore and ocean >3 miles from shore (for private boats)


# --------------- Section 6: Catch number capture and converting catch number to catch weight and adjustments for fishing effort---------------------
#*******************capture  catch estimates by year, or by year_wave
year_totalcatch = array(0, dim=c(n_years, n_species))# create and capture this array
for(s in 1:n_species) {
  # y_max = max(c(ma_smoothed_catch_plot[, s], ma_smoothed_by_area_catch_plot[, s], kalman_smoothed_catch_plot[, s], kalman_smoothed_by_area_catch_plot[, s], official_catch_num[, s]))
  year_totalcatch[,s] = apply(total_catch_num[, , s, , ], c(1), sum) #***capture these results for double check****
}

year_totalcatchboat = array(0, dim=c(n_years, n_species))# create and capture this array
for(s in 1:n_species) {
  # y_max = max(c(ma_smoothed_catch_plot[, s], ma_smoothed_by_area_catch_plot[, s], kalman_smoothed_catch_plot[, s], kalman_smoothed_by_area_catch_plot[, s], official_catch_num[, s]))
  year_totalcatchboat[,s] = apply(total_catch_num[, , s, 1, ], c(1), sum) #***capture these results for double check****
}


#capture original catch and variance by wave
n_year_waves = n_years * n_waves
# in the second-to-last dimension, index 1 = ocean (> 3 mi), index 2 = ocean (<= 3 mi), and index 3 = aggregated across the two areas; there is no deep 7 catch from inland fishing
# first dimension in these arrays is the sequential waves in order from oldest to most recent
#total_caught_yw = array(0, dim = c(n_year_waves, n_species, n_modes, n_areas, n_dispositions))
total_catch_num_yw = array(0, dim = c(n_year_waves, n_species, n_modes, n_areas))
total_catch_num_var_yw = array(0, dim = c(n_year_waves, n_species, n_modes, n_areas))
#num_trips_yw = array(0, dim = c(n_year_waves, n_modes, n_areas)) # added in Nov 2022
#effort_yw = array(0, dim = c(n_year_waves, n_modes, n_areas))

# reshaping the necessary arrays so that instead of year and wave being separate dimensions, they are instead a single dimension
for(y in 1:n_years) {
  for(w in 1:n_waves) {
    index = (y - 1) * 6 + w # sequential numbering of the current wave
     #total_caught_yw[index, , , , ] = total_caught[y, w, , , , ]
    total_catch_num_yw[index, , , ] = total_catch_num[y, w, , , ]
    total_catch_num_var_yw[index, , , ] = total_catch_num_var[y, w, , , ]
   # num_trips_yw[index, , ] = num_trips[y, w, , ]
   # effort_yw[index, , ] = effort[y, w, , ]
  }
}

yearwave_totalcatch_num = array(0, dim=c(n_year_waves, n_species))
yearwave_totalcatch_num_var= array(0, dim=c(n_year_waves, n_species))
yearwave_totalcatchboat_num = array(0, dim=c(n_year_waves, n_species))
yearwave_totalcatchboat_num_var= array(0, dim=c(n_year_waves, n_species))

for(s in 1:n_species) {
  # y_max = max(c(ma_smoothed_catch_plot[, s], ma_smoothed_by_area_catch_plot[, s], kalman_smoothed_catch_plot[, s], kalman_smoothed_by_area_catch_plot[, s], official_catch_num[, s]))
  yearwave_totalcatch_num[,s] = apply(total_catch_num_yw[, s, , ], c(1), sum)
  yearwave_totalcatch_num_var[,s] = apply(total_catch_num_var_yw[, s, , ], c(1), sum)
  yearwave_totalcatchboat_num[,s] = apply(total_catch_num_yw[, s,1 , ], c(1), sum) # capture catch from boat fishing
  yearwave_totalcatchboat_num_var[,s] = apply(total_catch_num_var_yw[, s,1 , ], c(1), sum)
  }

#******************catch number to weight and adjusted by multiplies and capture output in the output file directory

write.csv(as.data.frame(wave_smoothed_catch_rate[,,3,2]), file.path(root_dir, "03_outputs","CPUE_Kalmansmoothed.csv"))
#CPUE is for combined oceans (<=3 miles and > 3miles) without inland
#write.csv(as.data.frame(kalman_smoothed_catch_plot),file.path(root_dir, "03_outputs", "SmoothedYearlynonsold_includeSH.csv"))
write.csv(as.data.frame(kalman_smoothed_catch_plot),file.path(root_dir, "03_outputs", "SmoothedYearlyCatchnumber_includeSH.csv"))
write.csv(as.data.frame(cpue_raw2),file.path(root_dir, "03_outputs","CPUE_raw.csv"))
write.csv(as.data.frame(yearwave_totalcatch_num),file.path(root_dir, "03_outputs","UnsmoothedWavelyCatchnumber_includeSH.csv"))

catchwgt = array(0, dim = c(120,7))
catchnum <- as.data.frame(wave_smoothed_total_catch[, , 3, 2])
df = catchnum
df[42,4]=df[42,4]+unaccounted_total_catch[7,4]
#adding catch from shore fishing
# the unsmoothed wavely nonsold catch 
modify = read.csv(file.path(root_dir, "01_Data","Multiplier.csv"))
M1 = modify$M1
#M1 for calibrating the phone survey fishing effort estimates
M2 = modify$M2
#M2 for correction in phone survey estimates in 2003-2010
M3 = modify$M3
#M3 is the proportion of pure recreational fishing trips
meanwgt = read.csv(file.path(root_dir, "01_Data", "Meanweight.csv"))
wgt = meanwgt$lbs
if(catch_type=="Pure recreational"){
  for(s in 1:7) {
  for(w in 1:120) {
    catchwgt[w,s] = df[w,s]*M1[w]*M2[w]*M3[w]*wgt[s]
  }
  }}else{
    for(s in 1:7) {
      for(w in 1:120) {
        catchwgt[w,s] = df[w,s]*M1[w]*M2[w]*wgt[s]
      }
    }
  
}
dfwgt= as.data.frame(catchwgt)
dfwgt$FY = modify$FY
dfwgt$CY = modify$CY
write.csv(dfwgt,file.path(root_dir, "03_Outputs","SmoothedwavelyCachweight_effortadjusted.csv"))


# The variance estimates of the smoothed catch rate (in the Kalman filter and smoother output) were a deterministic function of the input variance values for observation error H and process error Q
# Thus, the percent standard error (PSE) from the unsmoothed catch estimates is calculated and used                                                               
origcatch = as.data.frame(yearwave_totalcatch_num)
variance = as.data.frame(yearwave_totalcatch_num_var)

dfvar = variance
dfvar$C1=origcatch$V1
dfvar$C2=origcatch$V2
dfvar$C3=origcatch$V3
dfvar$C4=origcatch$V4
dfvar$C5=origcatch$V5
dfvar$C6=origcatch$V6
dfvar$C7=origcatch$V7
dfvar$FY = modify$FY
dfvar$CY = modify$CY
write.csv(dfvar, file.path(root_dir, "03_Outputs","dataforPSE.csv"))


















