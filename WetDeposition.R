#### Description ####

#' @description This function is used to calculate the total wet nitrogen deposition in Joshua Tree 
#' @authors Kym Howo, Alex Irvin, Margaux Sleckman, Caitlin Swalec

#### Parameters ####

#' @param nitrogen_concentration: concentration of nitrogen (microgram/m3)
#' @param annual_precip: annual precipitation in Joshua tree (m/year)
#' @param num_storms: number of storms occuring in a year
#' @param storm_duration: duration of each storm (hours)
#' @param cloud_height: height of clouds (m)
#' @return annual wet deposition of nitrogen to Joshua Tree (microgram/(m2*year))

WetDeposition = function(nitrogen_concentration, annual_precip = 0.13, num_storms = 50, storm_duration = 10, cloud_height = 610) 
{
  #Calculate the rainfall rate per storm (in m/hr)
  rainfall_rate = (annual_precip / num_storms) / storm_duration
  
  #Calculate the washout coefficient, which is the rate at which nitrogen washes out of the atmosphere
  washout = 0.001 * rainfall_rate ^ 0.53
  
  #Concentration of N in the air after each storm
  concentration_N_left = nitrogen * exp(washout * storm_duration * 3600)
  
  #Amount of N washed out and deposited on the watershed per storm
  concentration_N_deposited = nitrogen_concentration - concentration_N_left
  
  #Annual wet deposition of nitrogen in micrograms/(m2*year)
  annual_wet = concentration_N_deposited * num_storms * cloud_height
  
  return(annual_wet)
}