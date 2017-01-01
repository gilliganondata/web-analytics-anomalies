####################
# Configuration
####################

# This script doesn't do anything on its own. It is a series of code chunks that
# get called from other scripts using read_chunk() in the .Rmd files.

# ---- timeframes ----
# Timeframe. Adjust these as you see fit (including hardcoding). They do
# need to be strings in YYYY-MM-DD format. 'date_end' is used by all of the scripts.
# You may want it to be through the most recent day, or you may want to have it
# be through the most recent week or month.

date_end <- as.character(Sys.Date() - as.POSIXlt(Sys.Date())$wday - 1)  # Most recent Saturday
# date_end <- as.character(as.Date(format(Sys.Date(), "%Y-%m-01")) -1)  # End of most recent month
# date_end <- as.character(Sys.Date() - 1)                              # Yesterday
# date_end <- "2016-12-31"                                              # Manually specified

# The dates below are used by different .Rmd files as noted. They *could* just
# be included there, but it seemed cleaner to have them here and then just delete
# or disregard ones that aren't being used.

# This one is just used by the "heatmap" Rmarkdown. It, along with date_end, define
# the range over which data will be aggregated.
date_start_heatmap <- as.character(as.Date(date_end)-7*6+1)     # Full 6 weeks before date_end
# date_start_heatmap <- "2016-12-04"                            # Manually specified

# These two dates are used with the "anomaly" Rmarkdown files. The "long_trend" is the
# start date for the full range that will be plotted in trendlines. The "anomaly_period" one
# is the start of the period for which anomalies will be detected/assessed. The forecast
# is based on the 35 days leading up to this date.
date_start_long_trend <- as.character(as.Date(date_end)-7*8+1)     # Full 8 weeks before date_end
date_start_anomaly_period <- as.character(as.Date(date_end)-6)     # Full 1 week leading to date_end
# date_start_long_trend <- "2016-06-30"                            # Manually specified
# date_start_anomaly_period <- "2016-08-28"                        # Manually specified

# Include weekends in weekly anomaly count? Set to "Yes" or "No." This is NOT used for the
# key_metrics_heatmaps.Rmd script(s), so you can skip this if that's all you're working with.
include_weekends <- "No"

# This operation just sets up the range of wday values based on whether include_weekends
# is set to include weekends or not.
include_days <- if(include_weekends == "No"){
  c(1:5)
} else {
  c(0:6) 
}

# ---- metrics-list ----
# Metrics to assess. This is a little clunky, but should be a one-time config. You can
# use GetMetrics() to get all available metrics. Then, you need four values for each metric,
# so just put them in the same location in each of four vectors:
#  - metric_id - the Adobe Analytics API value for the metric. Calculated metrics will
#    have a really long alphanumeric string for them
#  - metric_name - the label that will get used
#  - metric_format - what the number format should be (the available values are 
#     "comma" for numbers, "dollar" for numbers that should include a dollar sign in 
#     front of them, and "percent" for values that should have a % after it)
#  - metric_decimals is the number of places after the decimal to include
metrics_list <- data.frame(
  metric_id = c("visits", "pageviews", "orders", "revenue", "cm300000270_557f63ffe4b0d23197c9e852"),
  metric_name = c("Visits", "Page Views", "Orders", "Revenue", "Conversion Rate"),
  metric_format = c("comma", "comma", "comma", "dollar", "percent"), 
  metric_decimals = c(0,0,0,0,1), 
  stringsAsFactors = FALSE)

# ---- main-segment ----
# Segment(s) to apply to ALL results. This can be a single string (or "" for nothing)
# or a vector with multiple segment IDs. The example below will not work...so this needs
# to be changed or changed to "".
segments_all <- "537ce3d0e4b083ab6f5eccbd"

# ---- drilldown-segments ----
# Provide a list of segment IDs that will be drilled down on for each of the metrics.
# This list will be processed along with the next list. The order here is the order
# that the values will appear -- the rows -- in heatmaps that cross-reference with
# segment_drilldown_2.
segment_drilldown_1 <- list(
  list(name = "Segment #1 Name", seg_id = "s300000270_58571c3be4b0165fc1715030"),
  list(name = "Segment #2 Name", seg_id = "s300000270_58571c47e4b0032693cd96df"),
  list(name = "Segment #3 Name", seg_id = "s300000270_58571c53e4b0374f4d0c19d0"),
  list(name = "Segment #4 Name", seg_id = "s300000270_58571c50e4b0da3c63a627d8")
)

# Provide a list of segment IDs that will be drilled down on for each of the metrics.
# This list will be processed along with the previous list.
segment_drilldown_2 <- list(
  list(name = "Segment A Name", seg_id = "s300000270_58572dfae4b09ee39960d763"),
  list(name = "Segment B Name", seg_id = "s300000270_58572e08e4b084a9fd1490f5"),
  list(name = "Segment C Name", seg_id = "s300000270_58572e18e4b0a79c8eae83d9"),
  list(name = "Segment D Name", seg_id = "s300000270_58572e16e4b07db520adbcb8"),
  list(name = "Segment E Name", seg_id = "s300000270_58572e13e4b0012144125917")
)

# ---- default-theme ----
####################
# Define base theme. You have an option of tweaking settings
# here OR in the actual functions/output that use default_theme.
####################

default_theme <-   theme_bw() +
  theme(axis.text = element_text(face = "bold", size = 10, colour = "grey50"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_line(colour = "grey30"),
        axis.line.y = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor = element_blank()
  )
