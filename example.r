# Set working directory
setwd("~/GitHub/ibuttoncharge.git/")

#Load the function
source("ibuttoncharge.r")

# Load the data
data <- read.csv("example_data.csv")

# Or generate a synusoidal time series
data_syn <- rep(sapply(seq(-10,20,0.1), function(i)
      c(ceiling(50 * 0.5 * (1 + exp(-3*i/50) * sin(10*i/30))))),10)

# Run the function with different settings
lowcons <- ibuttoncharge(temp=data,sampling_interval=30,prev_charge=48,humidity=FALSE,res="8bit",plotting=FALSE)

highcons <- ibuttoncharge(temp=data,sampling_interval=30,prev_charge=48,humidity=TRUE,res="11bit")

# Calculate the difference in battery charge between the two ibuttons settings
highcons$perc_remaining_charge - lowcons$perc_remaining_charge
