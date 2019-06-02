#' submodel1 - emissions from econ. activity in LA county
#' @param dailyVMT_percar daily mileage per vehicle 
#' @param percent_trucks
#' @param percent_autos
#' @param percent_motorcycle
#' @param daily_vehicle_reduction_percent
#' @param prop_diesel
#' 

# **Agriculture:** Nitrous oxide can result from various agricultural soil management activities, such as synthetic and organic fertilizer application and other cropping practices, the management of manure, or burning of agricultural residues. Agricultural soil management is the largest source of N2O emissions in the United States, accounting for about 73.9 percent of total U.S. N2O emissions in 2017.
# 
# Fuel Combustion : Nitrous oxide is emitted when fuels are burned. The amount of N2O emitted from burning fuels depends on the type of fuel and combustion technology, maintenance, and operating practices.
# https://www.epa.gov/sites/production/files/2016-03/documents/mobileemissions_3_2016.pdf
# 
# Industry: Nitrous oxide is generated as a byproduct during the production of chemicals such as nitric acid, which is used to make synthetic commercial fertilizer, and in the production of adipic acid, which is used to make fibers, like nylon, and other synthetic products.
# 
# https://www.epa.gov/ghgemissions/overview-greenhouse-gases#nitrous-oxide


##########

# population_cars_growth_rate
  
transport_emissions_LA = function(percent_trucks = 14 , percent_autos = 84, percent_motocycles = 2,
                        dailyVMT_percar = 25, 
                        daily_vehicle_reduction_percent = 0,
                        prop_diesel = 0.036
                        ){
 
## 221.8 million miles are travelled daily in Los angeles county  (mi)
#  http://www.laalmanac.com/transport/tr01.php
  if(daily_vehicle_reduction_percent != 0){
    
    dailyVMT_percar = dailyVMT_percar - (dailyVMT_percar * (daily_vehicle_reduction_percent/100))

  }
  
  ###  Estimated National Average Vehicle Emissions Rates per Vehicle by Vehicle Type using Gasoline and Diesel 
  
  # (Grams per mile)
  # Estimates are by calendar year.  Vehicles types are defined as follows: 
  # light-duty vehicles (passenger cars)
  # light-duty trucks (two axle, four tire)
  # heavy-duty vehicles (trucks with more than two axles or four tires)
  # motorcycle (highway only) 
  
  EF_auto_gasoline = 0.289
  # light-duty trucks: 0.478; heavy duty trucks: 1.416
  EF_truck_gasoline = mean(c(0.478, 1.416))
  EF_motorcycles_gasoline = 0.719

  EF_auto_diesel = 0.153
  # light-duty trucks: 0.478; heavy duty trucks: 1.416
  EF_truck_diesel = mean(c(1.321, 5.971)) 
  
  EF_electricity = 0.4
  
  ## Note: average emissions per vehicle (gas + diesel) = 0.929
  
  # n_trucks = 14
  # n_autos = 84
  # n_motocycles = 2
  # dailyVMT = 221800000 
  # daily_vehicle_reduction_percent = 0
  # prop_diesel = 0.036
  # 
  

###  Number of cars  
  
  all_vehicles = 7762453
  
  all_vehicles_gasoline = all_vehicles  * (1 - prop_diesel)
  all_vehicles_diesel = all_vehicles * prop_diesel
  
  all_vehicles_diesel
  all_vehicles_gasoline
  
  prop_truck = (percent_trucks/100)
  prop_autos = (percent_autos/100)
  prop_motorcycles = (percent_motocycles/100)
  
# emissions per vehicle type  
  
  emissions_gasoline_per_mile = ((all_vehicles_gasoline*prop_truck) * EF_truck_gasoline) + 
    ((all_vehicles_gasoline * prop_autos) * EF_auto_gasoline) +
    ((all_vehicles_gasoline * prop_motorcycles) * EF_motorcycles_gasoline)
  
  emissions_diesel_per_mile = ((all_vehicles_diesel*prop_truck) * EF_truck_diesel) + 
    ((all_vehicles_diesel * prop_autos) * EF_auto_diesel)
  emissions_diesel_per_mile
  
  emissions_transport_per_mile = emissions_gasoline_per_mile + emissions_diesel_per_mile  
  emissions_transport_per_mile

### Calculate full grams of Nox emissions from vehicles
  emissions_transport_daily = emissions_transport_per_mile * dailyVMT_percar
 
  return(emissions_transport_daily)
   
}

# Fuel combustion: Electric utilities, petroleum refining, manufacturing and industrial, service and commercial,

# Tons per day from stationary sources = 33 tons/day of Nox in LA County
# https://www.arb.ca.gov/app/emsinv/fcemssumcat/cepam_emssumcat_query_v5.php

## Industry - incompletely   
fuel_combustion_emissions_LA <- function(n_industry, production_growth_rate){
  
  Production = n_industry * production_growth_rate
  EF_industry = 
  industry_emissions = EF * Production   
  return(industry_emissions)

  }


transport_emissions_LA(n_trucks = 14, n_autos = 84, n_motocycles = 2)

# Note: TOTAL ON-ROAD MOTOR VEHICLES 	106.816