# ibuttoncharge
Simple R function to estimate the ibutton battery consumption during a mission. 
The function is directly derived and improves the "Gas Gauge Spreadsheet" (https://www.maximintegrated.com/en/images/appnotes/3761/3761Gas_Gauge.zip) provided by Maxim Integrated.

## Options
temp: numeric vector or data.frame/matrix with numeric data in the first column.

sampling_interval: Sampling interval in minutes.

prev_charge: Battery charge in maH before the beginning of the current mission. The factory fresh battery charge is 48 maH.

humidity: if TRUE then humidity was sampled together with temperature.

res: numeric resolution of the converted temperature, "8bit" or "11bit".

plotting: if TRUE the cumulative used charge is plotted against each sample

printing: if TRUE ibutton settings and battery consumption metrics are printed on the screen

## References:
https://www.maximintegrated.com/en/app-notes/index.mvp/id/3761

## Example
### Set working directory
setwd("~/GitHub/ibuttoncharge/")

### Load the function
source("ibuttoncharge.r")

### Load the data
data <- read.csv("example_data.csv")

### Or generate a synusoidal time series
data_syn <- rep(sapply(seq(-10,20,0.1), function(i)
      c(ceiling(50 * 0.5 * (1 + exp(-3*i/50) * sin(10*i/30))))),10)

### Run the function with different settings
lowcons <- ibuttoncharge(temp=data,sampling_interval=30,prev_charge=48,humidity=FALSE,res="8bit",plotting=FALSE)

highcons <- ibuttoncharge(temp=data,sampling_interval=30,prev_charge=48,humidity=TRUE,res="11bit")

### Calculate the difference in battery charge between the two ibuttons settings
highcons$perc_remaining_charge - lowcons$perc_remaining_charge
