#### Description ####

#' @description This function is used to calculate the amount of emissions resulting from automobiles
#' @authors Kym Howo, Alex Irvin, Margaux Sleckman, Caitlin Swalec

#### Parameters ####

#' @param population_df dataframe of population size for by year
#' @param dailyVMT_percar Default = 25. daily mileage per vehicle
#' @param proportion_trucks Default = 0.14
#' @param proportion_autos Default = 0.84
#' @param proportion_motorcycle Default = 0.02
#' @param daily_vehicle_reduction_proportion Default = 0 . Can reduce the daily vehicle miles travelled by a certain percentage (decimal format)
#' @param proportion_diesel proportion of diesel vehicles
#' @param days_driven_peryear the average amount of miles driven per year by each vehicle
#' @param dailyVMT_change optional variable if the user wants to reduce vehicle mileage (e.g. 10% reduction)
#' @references http://www.laalmanac.com/transport/tr01.php (total miles driven)
#' @references https://www.bts.gov/content/estimated-national-average-vehicle-emissions-rates-vehicle-vehicle-type-using-gasoline-and (emissions factors)
#' @references https://www.epa.gov/sites/production/files/2017-04/documents/mobile_combustion_users_guide.pdf (mobile emissions calculation)
#' @references https://www.vitalsigns.mtc.ca.gov/daily-miles-traveled (daily miles travelled per K)
#' @return a dataframe of emissions for each year

transport_emissions_LA = function(population_df, 
                                  vehicle_per_capita = 0.76,
                                  proportion_trucks = 0.14 ,
                                  proportion_autos = 0.84,
                                  proportion_motocycles = 0.02,
                                  dailyVMT_percar = 25,
                                  dailyVMT_change = 0,
                                  proportion_diesel = 0.036,
                                  days_driven_peryear = 365){

#### Set up: #### 
  
  if(dailyVMT_change != 0){
    
    dailyVMT_percar = dailyVMT_percar + (dailyVMT_percar * (dailyVMT_change))

  }
  
###  Estimated National Average Vehicle Emissions Rates per Vehicle by Vehicle Type using Gasoline and Diesel 
  ## (Grams per mile)
    ## Estimates are by calendar year.  Vehicles types are defined as follows: 
    ## Light-duty vehicles (passenger cars)
    ## retrieved from Bureau of transportatin statistics
  
  ## gasoline
    EF_auto_gasoline = 0.289
  ## light-duty trucks (two axle, four tire)
  ## heavy-duty vehicles (trucks with more than two axles or four tires)
  ## light-duty trucks: 0.478; heavy duty trucks: 1.416
    EF_truck_gasoline = mean(c(0.478, 1.416))
  ## Motorcycle (highway only) 
    EF_motorcycles_gasoline = 0.719

  ## Diesel  
    EF_auto_diesel = 0.153
  # light-duty trucks: 0.478; heavy duty trucks: 1.416
    EF_truck_diesel = mean(c(1.321, 5.971)) 
  
  # EF_electricity = 0.4

  ## Note if interested: average emissions per vehicle (gas + diesel) = 0.929
  

###  Number of cars  
  all_vehicles = vehicle_per_capita * population_df[,2]
  ## by gasoline, by diesel
  all_vehicles_gasoline = all_vehicles  * (1 - proportion_diesel)
  all_vehicles_diesel = all_vehicles * proportion_diesel
  
  
#### Calculation of emissions per vehicle type: ####
 
  # NOx Emissions per mile   
  emissions_gasoline_per_mile = ((all_vehicles_gasoline*proportion_trucks) * EF_truck_gasoline) + 
  ((all_vehicles_gasoline * proportion_autos) * EF_auto_gasoline) +
  ((all_vehicles_gasoline * proportion_motocycles) * EF_motorcycles_gasoline)
  
  emissions_diesel_per_mile = ((all_vehicles_diesel*proportion_trucks) * EF_truck_diesel) + 
  ((all_vehicles_diesel * proportion_autos) * EF_auto_diesel)
  
  emissions_transport_per_mile = emissions_gasoline_per_mile + emissions_diesel_per_mile  
  
  ## Calculate full grams of Nox emissions from vehicles per day
  emissions_transport_daily = emissions_transport_per_mile * dailyVMT_percar

  ## NOx emissions from all vehicles per year - g   
  annual_transport_emissions_g = emissions_transport_daily*days_driven_peryear

  ## Unit conversion of NOx emissions from all vehicles per year - kg
  annual_transport_emissions_kg = annual_transport_emissions_g/1000

  ## create DF
  transport_df = data.frame(time=population_df[,1], pop=population_df[,2],annual_transport_emissions_kg)
  
  ## return
  return(transport_df)
  
  }
