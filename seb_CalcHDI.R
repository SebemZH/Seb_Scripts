# What this script does:
  # The User gives parameters of a country. Namely:
  # - Life Expectancy
  # - Expected and mean years of schooling
  # - GNI per capita
  # It will then calculate and return the Human Development Index (HDI)

# start function and ask for parameters
  calculate_HDI <- function(life_expectancy,
                            expected_schooling_years, mean_schooling_years,
                            GNI_per_capita) {
    
  # Calc Index for country's health
    I_health <-  (life_expectancy - 20) / 65 
  
  # Calc Index for country's education
    I_education <- ((mean_schooling_years / 15 ) + (expected_schooling_years / 18)) / 2
  
  # Calc Index for country's income
    I_income <- (log(GNI_per_capita) - log(100)) / log(750)
    
    
  # Lastly, Calc HDI and return
    HDI <- (I_health^(1/3)) * (I_education^(1/3)) * (I_income^(1/3))
    
  return(HDI)
}

# Author: Seb. Meier, Zurich 06.04.2025