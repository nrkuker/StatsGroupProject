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



# Confidence Interval Calculations ####
# x_bar given
x_bar <- mean(NPD$cavities, na.rm = T)

# calculate Margin of Error
t_alpha <- qt(1-alpha/2, df = n-1)
SEM <- sigma/sqrt(n)
MoE <- t_alpha * SEM

# calculate lower & upper bounds of CI
lower <- round(x_bar - MoE, 4)
upper <- round(x_bar + MoE, 4)

lower
upper
# 95% CI for number of cavities treated per dentist per week:
# 3.843-4.187

mean_CI <- function(x_bar, sigma, n, alpha){
  if(n >= 50) {
    SEM <- sigma/sqrt(n)
    t_alpha <- qt(1-alpha/2, df = n-1)
    MoE <- t_alpha*SEM
    cat("Lower limit of mean CI is", toString(round(x_bar - MoE, 2)), "\n")
    cat("Upper limit of mean CI is", toString(round(x_bar + MoE, 2)), "\n")
    cat(toString(x_bar), "+/-", toString(round(MoE, 2)), "\n")
  } else {
    print("Sample size not large enough to assume Normality")
  }
}

mean_CI(x_bar, sigma, n, alpha)



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
fixed_costs <- 4000000

# Unit costs & profits not dependent on number of cavities filled per year
unit_costs <- 200*dentists_using
unit_sales <- 200*dentists_using

# Solution costs & profits for lower bound of CI
solution_costs_lower <- 0.5*dentists_using*52*lower
solution_sales_lower <- 2.5*dentists_using*52*lower

# Solution costs & profits for upper bound of CI
solution_costs_upper <- 0.5*dentists_using*52*upper
solution_sales_upper <- 2.5*dentists_using*52*upper


# ROI for lower bound of CI
total_costs_lower <- fixed_costs + unit_costs + solution_costs_lower
total_sales_lower <- unit_sales + solution_sales_lower
ROI_lower <- (total_sales_lower - total_costs_lower)/total_costs_lower * 100

# ROI for upper bound of CI
total_costs_upper <- fixed_costs + unit_costs + solution_costs_upper
total_sales_upper <- unit_sales + solution_sales_upper
ROI_upper <- (total_sales_upper - total_costs_upper)/total_costs_upper * 100


round(ROI_lower, 2)
round(ROI_upper, 2)
# 95% CI for ROI: -0.05% - 5.00%


# necessary x_bar to break even
x_even <- round(fixed_costs / (2*dentists_using*52), 3)
# each dentist using Caridex needs to treat mean of 3.846 cavities per week for NPD to break even


# what level of confidence gets us above break even point?
desired_margin <- x_bar - x_even
SEM <- sigma/sqrt(n)
desired_t_alpha <- desired_margin / SEM

t_alpha <- qt(1-alpha/2, df = n-1)

alpha_breakeven <- (1 - pt(desired_t_alpha, df = n-1)) * 2
confidence_even <- 1-alpha_breakeven
# at least 94.59% confident that mean cavities per week per dentist will be above breakeven point
# technically, 94.59% confident mean cavities will be [3.846, 4.184]

x_bar - desired_margin
x_bar + desired_margin
