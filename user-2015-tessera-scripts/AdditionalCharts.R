require(datadr)
require(parallel)
require(ggplot2)
require(gridExtra)
require(hexbin)
## Create a datadr backend with a 4 core cluster
options(defaultLocalDiskControl = localDiskControl(makeCluster(4)))

## Create a plot of the fare vs. distance with 
## tip amount removed. 
## We need to add a transform.
no_toll <- function(x){
  x$no_toll <- x$total_amount - x$tolls_amount
  x
}

# test it on a subset
head(no_toll(taxi[[1]]$value))

# add the transformation
taxiNoToll  <- addTransform(taxi, no_toll)

# look at the result
taxiNoToll

# look at the name of the result
names(taxiNoToll)

## Look at the relationship between fare without tolls and distance traveled
trns <- function(x) log2(x + 1)
amount_vs_dist <- drHexbin("trip_distance", "no_toll", data = taxiNoToll, 
                           xbins = 150, shape = 1, xTransFn = trns, yTransFn = trns,
                           xRange = c(0, 100), yRange = c(0, 650))

plot(amount_vs_dist, trans = log, inv = exp, style = "colorscale", colramp = LinOCS, xlab = "Distance (log2 miles)", ylab = "Amount Paid Less Tolls (log2 dollars)")


## Look at just the fare without tolls or tip
amount_vs_dist <- drHexbin("trip_distance", "fare_amount", data = taxiNoToll, 
                           xbins = 150, shape = 1, xTransFn = trns, yTransFn = trns,
                           xRange = c(0, 100), yRange = c(0, 650))

plot(amount_vs_dist, trans = log, inv = exp, style = "colorscale", colramp = LinOCS, xlab = "Distance (log2 miles)", ylab = "Fare paid, no tip or tolls (log2 dollars)")

## Zooming in on Manhattan
xRange <- c(-74.03, -73.92)
yRange <- c(40.7, 40.82)
tmp <- addTransform(taxi, function(x) {
  subset(x, pickup_longitude < -73.92 & pickup_longitude > -74.03 &
           pickup_latitude > 40.7 & pickup_latitude < 40.82)
})

## Look at the relationship between fare without tolls and distance traveled
trns <- function(x) log2(x + 1)
amount_vs_dist <- drHexbin("trip_distance", "no_toll", data = tmp, 
                           xbins = 150, shape = 1, xTransFn = trns, yTransFn = trns,
                           xRange = c(0, 100), yRange = c(0, 650))

plot(amount_vs_dist, trans = log, inv = exp, style = "colorscale", colramp = LinOCS, 
     xlab = "Distance (log2 miles)", ylab = "Manhattan Amount Paid Less Tolls (log2 dollars)")
