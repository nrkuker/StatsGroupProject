# Confidence Interval Case Study ####

# Load Packages & Data ####
library(tidyverse)
library(readxl)

NPD <- read_xlsx("data/NPDC Case study Data.xlsx", sheet = "Sheet1")
head(NPD)
str(NPD)

n <- nrow(NPD)
sigma <- 1.75
alpha <- 0.05

# Test BG 10FEB ####

# ROI ####

# Givens:
  # 100k dentists in US treat cavities = 100000
  # 10% of dentists will use Caridex in 1st yr of production = 10000
  # Caridex = dispensing unit + dissolving solution
  # Dispensing unit = $200 each, sold at cost
  # Solution = $0.50 per cavity, sold at $2.50 per cavity
  # Fixed annual costs = $4 million = 4000000
  # Sample is n = 400 dentists who "planned to use Caridex", part of the 10000 number from above
  # Each obs is a single dentist and estimated num cavities per week

dentists_using <- 10000
unit_costs <- 200*dentists_using
unit_profits <- 0*dentists_using

num_cavities_perdent_perwk <- NA
solution_costs <- 0.5*num_cavities_perdent_perwk*dentists_using*52
solution_profits <- 2*num_cavities_perdent_perwk*dentists_using*52

fixed_costs <- 4000000

total_costs <- unit_costs + solution_costs
total_profit <- (unit_profits + solution_profits) - fixed_costs
ROI <- total_profit/total_costs * 100
