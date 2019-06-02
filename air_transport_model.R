#' Air transport
#' 
#' correlates air quality in LA to air quality in Joshua Tree
#' @param LA_air NOx level in LA (annual average)
#' @return JT_air NOx level in Joshua Tree (annual average)
#' @param data data frame including year and LA_air where LA_air is an annual average for NOx level

air_transport = function(LA_air) {
  JT_air = -8.0148 + LA_air*0.6693
  resturn(JT_air)
}

air_transport = function(data) {
  data$JT_air = -8.0148+data$LA_air*0.6693
}