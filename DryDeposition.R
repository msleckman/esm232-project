#### Description ####

#' @description This function is used to calculate the total dry nitrogen deposition in Joshua Tree 
#' @authors Kym Howo, Alex Irvin, Margaux Sleckman, Caitlin Swalec

#### Parameters ####

#' @param nitrogen_concentration: concentration of nitrogen (microgram/m3)
#' @param deposition_velocity: deposition velocity (m/sec)
#' @return annual dry deposition of nitrogen to Joshua Tree (microgram/(m2*year)

DryDeposition = function(nitrogen_concentration, deposition_velocity) 
{
  #Calculate and return per second dry deposition (microgram/(m2*sec))
  dry_dep = nitrogen_concentration * deposition_velocity 
  
  #Return annual dry deposition
  return(dry_dep * 60 * 60 * 24 * 365 )
}