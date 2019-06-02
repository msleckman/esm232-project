#' submodel1 - emissions from econ. activity in LA county
#' @param dailyVMT_percar daily mileage per vehicle 
#' @param percent_trucks
#' @param percent_autos
#' @param percent_motorcycle
#' @param daily_vehicle_reduction_percent
#' @param prop_diesel
#' 

######


7762454/10162507

# population_cars_growth_rate
  
transport_emissions_LA = function(population_df, 
                                  vehicle_per_capita = 0.76,
                                  percent_trucks = 14 , percent_autos = 84, percent_motocycles = 2,
                                  dailyVMT_percar = 25,
                                  daily_vehicle_reduction_percent = 0,
                                  prop_diesel = 0.036,
                                  days_driven_peryear = 365){

## Set up: 
  
  #  http://www.laalmanac.com/transport/tr01.php
  if(daily_vehicle_reduction_percent != 0){
    
    dailyVMT_percar = dailyVMT_percar - (dailyVMT_percar * (daily_vehicle_reduction_percent/100))

  }
  
  ###  Estimated National Average Vehicle Emissions Rates per Vehicle by Vehicle Type using Gasoline and Diesel 
  
  ## (Grams per mile)
    ## Estimates are by calendar year.  Vehicles types are defined as follows: 
    ## Light-duty vehicles (passenger cars)
  
  EF_auto_gasoline = 0.289
  ## light-duty trucks (two axle, four tire)
  ## heavy-duty vehicles (trucks with more than two axles or four tires)
  ## light-duty trucks: 0.478; heavy duty trucks: 1.416
  EF_truck_gasoline = mean(c(0.478, 1.416))
  ## Motorcycle (highway only) 
  EF_motorcycles_gasoline = 0.719

  EF_auto_diesel = 0.153
  # light-duty trucks: 0.478; heavy duty trucks: 1.416
  EF_truck_diesel = mean(c(1.321, 5.971)) 
  
  EF_electricity = 0.4
  
  prop_truck = (percent_trucks/100)
  prop_autos = (percent_autos/100)
  prop_motorcycles = (percent_motocycles/100)
  
  
  ## Note if interested: average emissions per vehicle (gas + diesel) = 0.929
  

###  Number of cars  
  all_vehicles = vehicle_per_capita * population_df[,2]
  ## by gasoline, by diesel
  all_vehicles_gasoline = all_vehicles  * (1 - prop_diesel)
  all_vehicles_diesel = all_vehicles * prop_diesel
  
  
# emissions per vehicle type  
  
  emissions_gasoline_per_mile = ((all_vehicles_gasoline*prop_truck) * EF_truck_gasoline) + 
  ((all_vehicles_gasoline * prop_autos) * EF_auto_gasoline) +
  ((all_vehicles_gasoline * prop_motorcycles) * EF_motorcycles_gasoline)
  
  emissions_diesel_per_mile = ((all_vehicles_diesel*prop_truck) * EF_truck_diesel) + 
  ((all_vehicles_diesel * prop_autos) * EF_auto_diesel)
  
  emissions_transport_per_mile = emissions_gasoline_per_mile + emissions_diesel_per_mile  
  
### Calculate full grams of Nox emissions from vehicles
  emissions_transport_daily = emissions_transport_per_mile * dailyVMT_percar
 
  annual_transport_emissions = emissions_transport_daily*days_driven_peryear

  transport_df = data.frame(population_df[,1], population_df[,2],annual_transport_emissions)   
  
  return(transport_df)
  }

# Fuel combustion: Electric utilities, petrol eum refining, manufacturing and industrial, service and commercial,

# Tons per day from stationary sources = 33 tons/day of Nox in LA County
# https://www.arb.ca.gov/app/emsinv/fcemssumcat/cepam_emssumcat_query_v5.php


# Note: TOTAL ON-ROAD MOTOR VEHICLES 	106.816