ibuttoncharge <- function(temp, sampling_interval,prev_charge=48, humidity=FALSE, res="8bit", plotting=FALSE, printing=TRUE) {

# Empirical measured energy consumption at different temperatures
    ec <- matrix(c(-40,-39,-38,-37,-36,-35,-34,-33,-32,-31,-30,-29,-28,-27,-26,-25,-24,-23,-22,-21,-20,-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,1.137,1.122,1.109,1.095,1.081,1.068,1.054,1.041,1.028,1.016,1.003,0.991,0.979,0.967,0.955,0.943,0.932,0.921,0.910,0.900,0.889,0.880,0.870,0.861,0.852,0.843,0.834,0.826,0.818,0.810,0.802,0.795,0.788,0.781,0.774,0.768,0.762,0.757,0.751,0.746,0.740,0.736,0.732,0.728,0.724,0.720,0.717,0.714,0.711,0.708,0.706,0.704,0.703,0.702,0.701,0.700,0.700,0.701,0.701,0.702,0.702,0.705,0.707,0.709,0.712,0.714,0.718,0.723,0.727,0.732,0.736,0.744,0.752,0.760,0.768,0.776,0.789,0.802,0.816,0.829,0.843,0.862,0.883,0.903,0.924,0.945,0.975,1.005,1.036,1.069,1.102,1.146,1.191,1.239,1.288,1.340,1.404,1.472,1.543,1.618,1.696,1.792,1.894,2.000,2.113,2.233,2.374,2.524,2.683,2.853,3.033,3.238,3.458,3.692,3.942,4.210,4.511,4.835,5.181,5.553,5.951,6.389,6.859,7.364,7.906,8.488,127.6,128.0,128.4,128.8,129.3,129.7,130.1,130.6,131.0,131.4,131.8,132.3,132.7,133.1,133.6,134.0,134.4,134.8,135.3,135.7,136.1,136.6,137.0,137.4,137.9,138.3,138.7,139.1,139.6,140.0,140.4,140.9,141.3,141.7,142.2,142.6,143.0,143.5,143.9,144.3,144.7,145.2,145.6,146.0,146.5,146.9,147.3,147.8,148.2,148.6,149.1,149.5,149.9,150.4,150.8,151.2,151.7,152.1,152.5,153.0,153.4,153.8,154.3,154.7,155.1,155.6,156.0,156.4,156.9,157.3,157.7,158.2,158.6,159.0,159.5,159.9,160.3,160.8,161.2,161.6,162.1,162.5,162.9,163.4,163.8,164.2,164.7,165.1,165.5,166.0,166.4,166.9,167.3,167.7,168.2,168.6,169.0,169.5,169.9,170.3,170.8,171.2,171.7,172.1,172.5,173.0,173.4,173.8,174.3,174.7,175.2,175.6,176.0,176.5,176.9,177.3,177.8,178.2,178.6,179.1,179.5,180.0,180.4,180.8,181.3,181.7),ncol=3)

# If temp is not a numeric vector, make it be a numeric vector
    if( !is(temp,"numeric") ) {
        x <- as.numeric(temp[,1])
    } else{x<-temp}

# If still is not a numeric vector, break the function
    if( !is(x,"numeric") ) {
        break("Non numerical vector as input data")
    }

# Check if there are NA's and warn on output inconsistency
    if( any(is.na(x)) ) {
        print("NA's in temperature vector, the cumulative sums of discharge will not be available")
    }

# Prepare data
    xr <- round(x)
    xl <- length(x)
    tm <- c()

# Calculate the consumption per each sample
    for (i in 1:xl) {
        rw <- which(ec[,1] %in% xr[i])
        tm[i] <- sampling_interval * 60 * ec[rw,2] + ifelse(res=="8bit",1,8) * ec[rw,3] + ifelse(humidity==TRUE,1,0) * 40
    }

# Derive metrics
    c_mas <- sum(tm,na.rm=T)
    c_mah <- c_mas/3600000
    r_mah <- prev_charge - c_mah
    r_mah_sample <- prev_charge - cumsum(tm / 3600000)
    c_per <- 100 - (c_mah / prev_charge * 100)
    m_length <- length(x) / (60 * 24 /sampling_interval)

# Plotting?
    if(plotting==TRUE) {
        plot(r_mah_sample~seq(1,length(r_mah_sample)),xlab="Sample",ylab="Remaining Charge mAh)")
    }

# Printing?
    if(printing==TRUE) {
        cat(paste('\n',"Mission length (days):  ",round(m_length,2),'\n',"Humidity:               ",humidity,'\n', "Temperature resolution: ",res,'\n',"Consumed charge (mAh):  ", round(c_mah,3),'\n',"Remaining charge (mAh): ", round(r_mah,3),'\n', "Remaining charge (%):   ", round(c_per,3),'\n \n'))
    }
    return(list(mission_duration_days=m_length,consumed_charge_mas=c_mas,consumed_charge_mah=c_mah,remaining_charge_mah=r_mah,cum_consumed_charge=r_mah_sample, perc_remaining_charge=c_per))
}
