#' Air transport
#' 
#' correlates air quality in LA to air quality in Joshua Tree
#' @param air_data data frame including 'year' and 'LA_air' where LA_air is an annual average for NOx level in kg/year
#' @param intercept y-intercept of linear regression for LA_air and JT_air
#' @param slope slope of linear regression for LA_air and JT_air
#' @param n_mw molecular weight for nitrogen (g/mol), default = 14 g/mol
#' @param avg_nox_mw weighted average molecular weight for nox (g/mol), default = 41.3 with a 30% and 70% mix of NO and NO2
#' @param area area of LA County, default = 12,310,000,000 m^2
#' @param height average height of the troposphere over Los Angeles, default = 12,000 m
#' @param return JT_data data frame including 'year' and 'JT_air' where  JT_air is an annual average nitrogen density for air in kg/m^2/year


air_transport = function(LA_data, intercept = -8.0148, slope = 0.6693, n_mw = 14, avg_nox_mw = 41.3, area = 12310000000, height = 12000) {
  JT_data <- LA_data
  JT_data$JT_air = (intercept+JT_data$LA_air*slope)*(n_mw/avg_nox_mw)/(area*height)
  JT_data <- JT_data %>% 
    select(year, JT_air)
  return(JT_data)
}