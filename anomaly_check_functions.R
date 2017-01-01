####################
# Functions
####################

# This script doesn't do anything on its own. It is a series of code chunks that
# get called from the anomaly-oriented scripts using read_chunk() in the .Rmd files.

# ---- assess-anomalies ----

# Function that requires 'data_anomaly

# Filter the data provided down to just the data for the metric of
# interest, does an assessment of which data points are anomalies, and returns a
# list with a filtered and augmented data frame, as well as a count of the number
# of good anomalies, bad anomalies, and net good (good - bad) anomalies.
#   - metric = the metric as it comes out of the Adobe API
#   - data = data for the anomaly period with forecast data included

assess_anomalies <- function(metric, data){
  
  # Select just the datetime and data for the metric
  data_anomaly_period <- select(data, matches(paste0("datetime|",metric)))
  
  # Rename columns to make them legend-friendly
  colnames(data_anomaly_period) <- gsub("forecast.*","Forecast",colnames(data_anomaly_period)) %>%
    gsub("upperBound.*","Upper",.) %>%
    gsub("lowerBound.*","Lower",.) %>%
    gsub(metric,"Actual",.)
  
  # Identify the anomalies by adding a column that puts an NA if, during the anomaly assessment
  # period (with or without weekends, as specified in the configuration), the 
  # actual value is NOT outside the upper/lower bounds and puts the actual value if
  # it is. These are then used with geom_point() later to plot the anomalies
  # on the chart.
  data_anomaly_period$Anomalies_Good <- 
    #    ifelse(data_anomaly_period$Actual > data_anomaly_period$Forecast,
    ifelse(data_anomaly_period$Actual > data_anomaly_period$Upper,
           ifelse(as.POSIXlt(data_anomaly_period$datetime)$wday %in% include_days,
                  data_anomaly_period$Actual, NA), NA)
  
  data_anomaly_period$Anomalies_Bad <- 
    #    ifelse(data_anomaly_period$Actual < data_anomaly_period$Forecast,
    ifelse(data_anomaly_period$Actual < data_anomaly_period$Lower,
           ifelse(as.POSIXlt(data_anomaly_period$datetime)$wday %in% include_days,
                  data_anomaly_period$Actual, NA), NA)
  
  # Get number of anomalies in the anomaly period
  anomaly_count_good <- sum(!is.na(data_anomaly_period$Anomalies_Good))
  anomaly_count_bad <- sum(!is.na(data_anomaly_period$Anomalies_Bad))
  anomaly_count_net_good <- anomaly_count_good - anomaly_count_bad
  
  # Convert the table to a tidy structure
  data_anomaly_period <- gather(data_anomaly_period, metric_name, value, -datetime)
  
  # Return a list with the anomaly counts and the prepped data frame
  return_results <- list(data = data_anomaly_period,
                         good_anomalies = anomaly_count_good,
                         bad_anomalies = anomaly_count_bad,
                         net_good_anomalies = anomaly_count_net_good)
  
}

# ---- plot-with-anomalies ----

# Function to get the pre-anomaly actuals for the plot.
#   - metric = the metric as it comes out of the Adobe API
#   - data = daily data for the pre-anomaly period (actuals only)

get_pre_anomaly_actuals <- function(metric, data){
  
  # Select just the datetime and data for the metric
  data_actuals_trend <- select(data, matches(paste0("datetime|",metric)))
  
  # Rename columns to make them legend-friendly. This could be done
  # just by index, but this seems more robust and a similar approach is used
  # later with a more involved set of renaming.
  colnames(data_actuals_trend) <- gsub(metric, "Actual", colnames(data_actuals_trend))
  
  # Add NA for Upper, Lower, and Forecast. These values don't exist for the
  # pre-anomaly period data, but we want to have the right number of rows when
  # we merge this with the anomaly data and plot it.
  data_actuals_trend$Forecast <- NA
  data_actuals_trend$Upper <- NA
  data_actuals_trend$Lower <- NA
  data_actuals_trend$Anomalies_Good <- NA
  data_actuals_trend$Anomalies_Bad <- NA
  
  # Convert the table to a tidy structure
  data_actuals_trend <- gather(data_actuals_trend, metric_name, value, -datetime)
  
}

# Function to actually build a plot that trends actuals for the pre-anomaly period,
# then trends actuals, forecast, confidence interval, and highlights for actuals
# following outside the forecast confidence interval. Returns the plot as well as the
# counts of anomalies.
#     - metric = the metric as it comes out of the Adobe API
#     - data_pre_anomaly_period = daily data from the Adobe API for the pre-anomaly period
#     - data_anomaly_period = daily data w/ forecast from the Adobe API for the anomaly period

build_plot <- function(metric, data_pre_anomaly_period, data_anomaly_period){
  
  # Get metric label
  metric_label <- filter(metrics_list, metric_id == metric) %>% 
    select(metric_name) %>% as.character()
  
  # Get metric format (for y-axis of plot)
  metric_format <- filter(metrics_list, metric_id == metric) %>% 
    select(metric_format) %>% as.character()
  
  # Get pre-anomaly period data
  data_actuals_trend <- get_pre_anomaly_actuals(metric, data_pre_anomaly_period)
  
  # Get anomaly period data (including anomalies assessment)
  anomalies_assessment <- assess_anomalies(metric, data_anomaly_period)
  data_anomaly_period <- anomalies_assessment$data
  
  # Combine the two sets of data so that they're ready to plot and then sort
  # the data frame by datetime and then by metric
  data_plot <- rbind(data_actuals_trend, data_anomaly_period) %>%
    arrange(metric_name, datetime)
  
  # Create a plot of the data that shows the actual, forecast, and anomalies.
  full_plot <- ggplot(data = data_plot,
                      aes(x = datetime)) +
    
    # Upper/lower band
    geom_ribbon(aes(ymin = filter(data_plot,metric_name=="Lower") %>% select(value),
                    ymax = filter(data_plot,metric_name=="Upper") %>% select(value)),
                fill = "gray95", colour = NA) +
    
    # Forecast line
    geom_line(aes(y = filter(data_plot,metric_name=="Forecast") %>% select(value)),
              linetype="dotted", color="gray50") +
    
    # Actuals line
    geom_line(aes(y = filter(data_plot,metric_name=="Actual") %>% select(value))) +
    
    # "Good" Anomalies
    geom_point(aes(y = filter(data_plot,metric_name=="Anomalies_Good") %>% select(value)),
               size = 1, color= "#169E2F") +
    
    # "Bad" Anomalies
    geom_point(aes(y = filter(data_plot,metric_name=="Anomalies_Bad") %>% select(value)),
               size = 1, color= "#D31717") +
    
    # A bit of additional cleanup
    default_theme +
    expand_limits(y=0) +
    scale_y_continuous(expand = c(0, 0), labels = get(metric_format)) 
  
  # Build a list to return the plot as well as the anomaly counts.
  return_results <- list(plot = full_plot,
                         good_anomalies = anomalies_assessment$good_anomalies,
                         bad_anomalies = anomalies_assessment$bad_anomalies,
                         net_good_anomalies = anomalies_assessment$net_good_anomalies)
}
