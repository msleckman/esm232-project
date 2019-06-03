#### Description ####

#' @description This function is used to calculate the total wet nitrogen deposition in Joshua Tree 
#' @authors Kym Howo, Alex Irvin, Margaux Sleckman, Caitlin Swalec

#### Parameters ####

#' @param nitrogen_concentration: dataframe of concentration of nitrogen for each year (g/m3)
#' @param annual_precip: annual precipitation in Joshua tree (m/year)
#' @param num_storms: number of storms occuring in a year
#' @param storm_duration: duration of each storm (hours)
#' @param cloud_height: height of clouds (m)
#' @return dataframe of annual wet deposition of nitrogen to Joshua Tree (g/(m2*year))

WetDeposition = function(nitrogen_concentration, annual_precip = 0.13, num_storms = 50, storm_duration = 10, cloud_height = 610) 
{
  #Calculate the rainfall rate per storm (in m/hr)
  rainfall_rate = ((annual_precip * 1000) / num_storms) / storm_duration
  
  #Calculate the washout coefficient, which is the rate at which nitrogen washes out of the atmosphere
  washout = 0.0001 * rainfall_rate ^ 0.53
  
  #Concentration of N in the air after each storm
  concentration_N_left = nitrogen_concentration$city2_air * exp(-1 * washout * storm_duration * 3600)

  #Amount of N washed out and deposited on the watershed per storm
  concentration_N_deposited = nitrogen_concentration$city2_air - concentration_N_left

  #Annual wet deposition of nitrogen in g/(m2*year)
  annual_wet = concentration_N_deposited * num_storms * cloud_height
  
  deposition_per_year = data.frame(time = nitrogen_concentration$time, wet_dep = annual_wet)
  
  return(deposition_per_year)
}