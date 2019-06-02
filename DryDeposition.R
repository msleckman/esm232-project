#### Description ####

#' @description This function is used to calculate the total dry nitrogen deposition in Joshua Tree 
#' @authors Kym Howo, Alex Irvin, Margaux Sleckman, Caitlin Swalec

#### Parameters ####

#' @param nitrogen_concentration: dataframe of concentration of nitrogen for each year (g/m3)
#' @param deposition_velocity: deposition velocity (m/sec)
#' @return dataframe of annual dry deposition of nitrogen to Joshua Tree (g/(m2*year)

DryDeposition = function(nitrogen_concentration, deposition_velocity) 
{
  #Calculate and return per second dry deposition (g/(m2*sec))
  dry_dep = nitrogen_concentration$nitrogen * deposition_velocity
  dry_dep = dry_dep * 60 * 60 * 24 * 365
  
  deposition_per_year = data.frame(nitrogen_concentration$year, dry_dep)
  
  #Return annual dry deposition
  return(dry_dep * 60 * 60 * 24 * 365)
}