#### Description ####

#' @description This function is used to correlate the level of NOx emissions in LA County to the level of atmospheric nitrogen pollution in Joshua Tree.
#' @authors Kym Howo, Alex Irvin, Margaux Sleckman, Caitlin Swalec

#### Parameters ####

#' @param historic_data: historic annual averages of NOx for the two cities of interest (kg/year)
#' @param city1_data: annual averages of NOx for City 1(kg/year)
#' @param n_mw: molecular weight for nitrogen (g/mol), default = 14 g/mol
#' @param avg_nox_mw: weighted average molecular weight for NOx (g/mol), default = 41.3 with a 30% and 70% mix of NO and NO2
#' @param area: area of City 1, defaulted to LA = 12,310,000,000 m^2
#' @param height: average height of the troposphere over City 1, defaulted to LA = 12,000 m
#' @return annual average nitrogen density for City 2 (kg/m^2/year)

air_transport = function(historic_data, city1_data, n_mw = 14, avg_nox_mw = 41.3, area = 12310000000, height = 12000) 
{
  #Testing historic data for major outliers in historic annual averages of NOx for the two cities of interest
  par(mfrow=c(2, 2))
  boxplot_city1 <- boxplot(historic_data$city1_nox, main="City 1 NOx", sub=paste("Outlier rows: ", boxplot.stats(historic_data$city1_nox)$out)) 
  boxplot_city2 <- boxplot(historic_data$city2_nox, main="City 2 NOx", sub=paste("Outlier rows: ", boxplot.stats(historic_data$city2_nox)$out))
  
  #Create density plots for historic data of annual averages of NOx for the two cities of interest to view viability of linear regression
  library(e1071)
  density_city1 <- plot(density(historic_data$city1_nox), main="Density Plot: City 1 NOx", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(historic_data$city1_nox), 2)))
  polygon(density(historic_data$city1_nox), col="red")
  density_city2 <- plot(density(historic_data$city2_nox), main="Density Plot: City 2 NOx", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(historic_data$city2_nox), 2)))
  polygon(density(historic_data$city2_nox), col="red")
  
  #Calculate correlatlion between NOx in City 1 and NOx in City 2
  correlation <- cor(historic_data$city1_nox, historic_data$city2_nox)
  
  #Build linear model for the two cities of interest and plot results to visual fit of model
  linear_mod <- lm(city2_nox ~ city1_nox, data=historic_data) 
  model_data <- historic_data %>% 
    mutate(city2_nox_predict = linear_mod$coefficients[1] + city1_nox*linear_mod$coefficients[2])
  lm_plot <- ggplot(model_data, aes(city1_nox, city2_nox)) +
    geom_point()+
    geom_line(aes(x=city1_nox, city2_nox_predict)) +
    ggtitle("Linear Model")
  
  #Calculate statistics on fit of linear model 
  model_summary <- summary(linear_mod)
  model_coeffs <- model_summary$coefficients
  beta.estimate <- model_coeffs["city1_nox", "Estimate"]
  std.error <- model_coeffs["city1_nox", "Std. Error"]
  t_value <- beta.estimate/std.error
  p_value <- 2*pt(-abs(t_value), df=nrow(historic_data)-ncol(historic_data))
  
  #Correlates the NOx level in LA County to NOx level in Joshua Tree, then converts from kg NOx/year to kg N/year using molecular weights of nitrogen and NOx, then converts from kg N/year to kg N/m^3/year using the area of LA County and height of troposphere
  city2_data <- city1_data
  city2_data$city2_air = (linear_mod$coefficients[1]+city2_data$nitrogen*linear_mod$coefficients[2])*(n_mw/avg_nox_mw)/(area*height)
  city2_data <- city2_data %>% 
    select(time, city2_air)
  
  #Format statistical outputs into table
  stat <- c("correlation", "std error", "t value", "p value")
  value <- c(correlation, std.error, t_value, p_value)
  stats <- cbind(stat, value)
  
  return(list(city2_data, boxplot_city1, boxplot_city2, density_city1, density_city2, model_summary, lm_plot, stats))
}