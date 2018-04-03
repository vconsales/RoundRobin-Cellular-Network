slideFunct <- function(data, window, step){
  total <- length(data)
  spots <- seq(from=1, to=(total-window), by=step)
  result <- vector(length = length(spots))
  for(i in 1:length(spots)){
    result[i] <- mean(data[spots[i]:(spots[i]+window)])
  }
  return(result)
}

plotPoints <- function(targetvec, mainlabel, xlabel, ylabel, yformat='g') {
	# plot data
	plot(targetvec$x, targetvec$y, yaxt = 'n', pch='.', main=mainlabel, xlab=xlabel, ylab=ylabel)

	## use decimal notation for y axis
	pointplotTicks = axTicks(2)
	axis(2, at = pointplotTicks, labels = formatC(pointplotTicks, format = yformat))

	# mean constant line
	abline(mean(targetvec$y),0, col="red")
}

plotSlideWinAvg <- function(targetvec, window_size, mainlabel, xlabel, ylabel, yformat='g') {
	targetvecy_sldwinavg=slideFunct(targetvec$y, window_size, 1)
	# plot data
	plot(targetvec$x[1:(length(targetvec$x)-window_size)], targetvecy_sldwinavg,
		yaxt = 'n', pch='.', main=mainlabel, xlab=xlabel, ylab=ylabel)

	## use decimal notation for y axis
	winavgTicks = axTicks(2)
	axis(2, at = winavgTicks, labels = formatC(winavgTicks, format = yformat))

	# mean constant line
	abline(mean(targetvec$y),0, col="red")
}

require(omnetpp)

setwd("results/")
db <- loadDataset("BinomialCQI-usertraffic=1.6-#0.vec")
rawvec <- loadVectors(db, NULL)
veclist <- split(rawvec$vectordata, rawvec$vectordata$resultkey)

# window properties
X11(width=14, height=7)
par(mfrow=c(2,2))

# target User
targetvec=veclist[[1]]

#################### Points Plot ####################
plotPoints(targetvec, mainlabel="User1 Throughput", xlabel="Time (ms)", ylabel="Throughput (bit/s)", yformat='d')
#main="User1 Throughput", xlab="Time (ms)", ylab="Throughput (bit/s)"
#####################################################

#################### Winsldavg Plot ####################
plotSlideWinAvg(targetvec, 50, mainlabel="User1 Slide Window AVG Throughput", xlabel="Time (ms)", ylabel="Throughput (bit/s)", yformat='d')
########################################################

# target User
targetvec=veclist[[11]]

#################### Points Plot ####################
plotPoints(targetvec, mainlabel="User1 Response Time", xlabel="Time (ms)", ylabel="Time (ms)")
#####################################################

#################### Winsldavg Plot ####################
plotSlideWinAvg(targetvec, 50, mainlabel="User1 Slide Window AVG Response Time", xlabel="Time (ms)", ylabel="Time (ms)")
#######################################################


X11(width=14, height=7)
scalar_db <- loadDataset("BinomialCQI-*.sca")

# filter all scalars except for throughputBits:last
allThroughputBits <- scalar_db$scalars[scalar_db$scalars$name == "throughputBits:last",]
allThroughputBits$file <- NULL
allThroughputBits$name <- NULL

# take all repetition rows from runattrs table
repetitions <- scalar_db$runattrs[scalar_db$runattr$attrname=="repetition",]
repetitions$attrname <- NULL

# take usertraffic value
usertraffics <- scalar_db$params[scalar_db$params$paramname=="**.webServer[*].lambda",]
usertraffics$paramname <- NULL

# merge scalar table with repetitions and usertraffics
partialMergedScalars <- merge(allThroughputBits, repetitions)
mergedScalars <- merge(partialMergedScalars, usertraffics)

# rename column to match values (otherwise they will be too generic)
colnames(mergedScalars)=c("runid","resultkey","module","value","repetition","usertraffic")

agg_clients <- aggregate(list(throughputbits=mergedScalars$value), by = list(module=mergedScalars$module, usertraffic=mergedScalars$usertraffic), mean)

targetmodule = agg_clients[agg_clients$module == "CellularNetwork.users[0]",]
plot(targetmodule$usertraffic,targetmodule$throughputbits, pch='.')

# compute global antenna traffic summing all client traffics
agg_globaltraffic <- aggregate(list(throughputbits=agg_clients$throughputbits), by = list(usertraffic=agg_clients$usertraffic), sum)
agg_globaltraffic$usertraffic <- factor(agg_globaltraffic$usertraffic)

plot(agg_globaltraffic$usertraffic, agg_globaltraffic$throughputbits, type='n', yaxt = 'n')
lines(agg_globaltraffic$usertraffic, agg_globaltraffic$throughputbits, yaxt = 'n')
abline(max(as.numeric(agg_globaltraffic$throughputbits)),0, col="red")

## use decimal notation for y axis
globaltrafficTicks = axTicks(2)
axis(2, at = globaltrafficTicks, labels = formatC(globaltrafficTicks, format = 'd'))

## wait for user input (do not close the window after script end)
b <- scan("stdin", character(), n=1)