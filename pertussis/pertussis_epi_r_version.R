# Imports
pacman::p_load(rio,          # File import
               here,         # File locator
               tsibble,      # handle time series datasets
               slider,       # for calculating moving averages
               imputeTS,     # for filling in missing values
               feasts,       # for time series decomposition and autocorrelation
               forecast,     # fit sin and cosin terms to data (note: must load after feasts)
               trending,     # fit and assess models 
               tmaptools,    # for getting geocoordinates (lon/lat) based on place names
               ecmwfr,       # for interacting with copernicus sateliate CDS API
               stars,        # for reading in .nc (climate data) files
               units,        # for defining units of measurement (climate data)
               yardstick,    # for looking at model accuracy
               surveillance,  # for aberration detection
               tidyverse     # data management + ggplot2 graphics
)
# import the counts into R
counts <- rio::import("Pertussis_Line_List.csv")
#counts <- rio::import("campylobacter_germany.xlsx")
## ensure the date column is in the appropriate format
counts$datetime <- as.Date(counts$datetime)

# Step 3: Resample the data to monthly totals
counts <- counts %>%
  mutate(datetime = floor_date(datetime, "week")) %>%  # Floor to the start of each month
  group_by(datetime) %>%
  summarize(cases = sum(cases, na.rm = TRUE))

## create a calendar week variable 
## fitting ISO definitons of weeks starting on a monday
counts <- counts %>% 
  mutate(epiweek = yearweek(datetime, week_start = 1))

## define time series object 
counts <- tsibble(counts, index = epiweek)

## plot a line graph of cases by week
ggplot(counts, aes(x = epiweek, y = cases)) + 
  geom_line()

## get a vector of TRUE/FALSE whether rows are duplicates
are_duplicated(counts, index = epiweek) 

## get a data frame of any duplicated rows 
duplicates(counts, index = epiweek) 


## create a variable with missings instead of weeks with reporting issues
counts <- counts %>% 
  mutate(case_miss = if_else(
    ## if epiweek contains 52, 53, 1 or 2
    str_detect(epiweek, "W51|W52|W53|W01|W02"), 
    ## then set to missing 
    NA_real_, 
    ## otherwise keep the value in case
    cases
  ))

## alternatively interpolate missings by linear trend 
## between two nearest adjacent points
counts <- counts %>% 
  mutate(case_int = imputeTS::na_interpolation(case_miss)
  )

## to check what values have been imputed compared to the original
ggplot_na_imputations(counts$case_miss, counts$case_int) + 
  ## make a traditional plot (with black axes and white background)
  theme_classic()


## create a moving average variable (deals with missings)
counts <- counts %>% 
  ## create the ma_4w variable 
  ## slide over each row of the case variable
  mutate(ma_4wk = slider::slide_dbl(cases, 
                                    ## for each row calculate the name
                                    ~ mean(.x, na.rm = TRUE),
                                    ## use the four previous weeks
                                    .before = 4))

## make a quick visualisation of the difference 
ggplot(counts, aes(x = epiweek)) + 
  geom_line(aes(y = cases)) + 
  geom_line(aes(y = ma_4wk), colour = "red")


## Function arguments
#####################
## x is a dataset
## counts is variable with count data or rates within x 
## start_week is the first week in your dataset
## period is how many units in a year 
## output is whether you want return spectral periodogram or the peak weeks
## "periodogram" or "weeks"

# Define function
periodogram <- function(x, 
                        counts, 
                        start_week = c(2002, 1), 
                        period = 52, 
                        output = "weeks") {
  
  
  ## make sure is not a tsibble, filter to project and only keep columns of interest
  prepare_data <- dplyr::as_tibble(x)
  
  # prepare_data <- prepare_data[prepare_data[[strata]] == j, ]
  prepare_data <- dplyr::select(prepare_data, {{counts}})
  
  ## create an intermediate "zoo" time series to be able to use with spec.pgram
  zoo_cases <- zoo::zooreg(prepare_data, 
                           start = start_week, frequency = period)
  
  ## get a spectral periodogram not using fast fourier transform 
  periodo <- spec.pgram(zoo_cases, fast = FALSE, plot = FALSE)
  
  ## return the peak weeks 
  periodo_weeks <- 1 / periodo$freq[order(-periodo$spec)] * period
  
  if (output == "weeks") {
    periodo_weeks
  } else {
    periodo
  }
  
}

## get spectral periodogram for extracting weeks with the highest frequencies 
## (checking of seasonality) 
periodo <- periodogram(counts, 
                       case_int, 
                       start_week = c(2002, 1),
                       output = "periodogram")

## pull spectrum and frequence in to a dataframe for plotting
periodo <- data.frame(periodo$freq, periodo$spec)

## plot a periodogram showing the most frequently occuring periodicity 
ggplot(data = periodo, 
       aes(x = 1/(periodo.freq/52),  y = log(periodo.spec))) + 
  geom_line() + 
  labs(x = "Period (Weeks)", y = "Log(density)")

## decompose the counts dataset 
counts %>% 
  # using an additive classical decomposition model
  model(classical_decomposition(case_int, type = "additive")) %>% 
  ## extract the important information from the model
  components() %>% 
  ## generate a plot 
  autoplot()
## add in fourier terms using the epiweek and case_int variabless
counts$fourier <- select(counts, epiweek, case_int) %>% 
  fourier(K = 1)
## define the model you want to fit (negative binomial) 
model <- glm_nb_model(
  ## set number of cases as outcome of interest
  case_int ~
    ## use epiweek to account for the trend
    epiweek +
    ## use the fourier terms to account for seasonality
    fourier)

## fit your model using the counts dataset
fitted_model <- trending::fit(model, data.frame(counts))

## calculate confidence intervals and prediction intervals 
observed <- predict(fitted_model, simulate_pi = FALSE)

estimate_res <- data.frame(observed$result)

## plot your regression 
ggplot(data = estimate_res, aes(x = epiweek)) + 
  ## add in a line for the model estimate
  geom_line(aes(y = estimate),
            col = "Red") + 
  ## add in a band for the prediction intervals 
  geom_ribbon(aes(ymin = lower_pi, 
                  ymax = upper_pi), 
              alpha = 0.25) + 
  ## add in a line for your observed case counts
  geom_line(aes(y = case_int), 
            col = "black") + 
  ## make a traditional plot (with black axes and white background)
  theme_classic()


## calculate the residuals 
estimate_res <- estimate_res %>% 
  mutate(resid = fitted_model$result[[1]]$residuals)

## are the residuals fairly constant over time (if not: outbreaks? change in practice?)
estimate_res %>%
  ggplot(aes(x = epiweek, y = resid)) +
  geom_line() +
  geom_point() + 
  labs(x = "epiweek", y = "Residuals")
