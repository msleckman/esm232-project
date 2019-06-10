#### Description ####

#' @description This ODE function is used to calculate human population growth 
#' @authors Kym Howo, Alex Irvin, Margaux Sleckman, Caitlin Swalec

#### Parameters ####

#' @param time time since start
#' @param P population
#' @param parms - list with two values, r, K
#' @param r intrinsic growth rate
#' @param K carrying capacity
#' @return derivative of population with time

HumanPopGrowth = function(Time, P, parms) {
  dP = parms$r * P * (1- P/parms$K)
  return(list(dP))
}
