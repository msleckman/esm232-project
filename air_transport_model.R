#### Description ####

#' @description This function is used to correlate the level of NOx emissions in LA County to the level of atmospheric nitrogen pollution in Joshua Tree.
#' @authors Kym Howo, Alex Irvin, Margaux Sleckman, Caitlin Swalec

#### Parameters ####

#' @param air_data: annual averages of NOx (kg/year)
#' @param intercept: y-intercept of linear regression correlating LA NOx levels to Joshua Tree NOx levels, default = -8.0148
#' @param slope: slope of linear regression correlating LA NOx levels to Joshua Tree NOx levels, default = 0.6693
#' @param n_mw: molecular weight for nitrogen (g/mol), default = 14 g/mol
#' @param avg_nox_mw: weighted average molecular weight for NOx (g/mol), default = 41.3 with a 30% and 70% mix of NO and NO2
#' @param area: area of LA County, default = 12,310,000,000 m^2
#' @param height: average height of the troposphere over Los Angeles, default = 12,000 m
#' @return annual average nitrogen density for Joshua Tree (kg/m^2/year)

air_transport = function(LA_data, intercept = -8.0148, slope = 0.6693, n_mw = 14, avg_nox_mw = 41.3, area = 12310000000, height = 12000) 
{
  JT_data <- LA_data
  
  #Correlates the NOx level in LA County to NOx level in Joshua Tree, then converts from kg NOx/year to kg N/year using molecular weights of nitrogen and NOx, then converts from kg N/year to kg N/m^3/year using the area of LA County and height of troposphere
  JT_data$JT_air = (intercept+JT_data$LA_air*slope)*(n_mw/avg_nox_mw)/(area*height)
  JT_data <- JT_data %>% 
    select(year, JT_air)
  return(JT_data)
}