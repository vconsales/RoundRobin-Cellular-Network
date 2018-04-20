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

plotUserThroughput <- function(xdata, ydata, mainlabel, xlabel, ylabel) {
	plot.default(xdata, ydata, type='n', yaxt = 'n', main=mainlabel, xlab=xlabel, ylab=ylabel)
	lines(xdata, ydata, yaxt = 'n')

	globaltrafficTicks = axTicks(2)
	axis(2, at = globaltrafficTicks, labels = formatC(globaltrafficTicks, format = 'd'))

	# mean constant line
	abline(max(as.numeric(ydata)),0, col="red")
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


######################## PART2 ########################
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
agg_clients$usertraffic <- factor(agg_clients$usertraffic)

X11(width=14, height=7)
par(mfrow=c(1,2))

#plotUserThroughput(targetmodule$usertraffic, targetmodule$throughputbits,
#		mainlabel=sprintf("user1 Throughput %d",clientindex), xlabel="Rate", ylabel="Throughput")
for (clientindex in 0:9){
	targetmodule = agg_clients[agg_clients$module == sprintf("CellularNetwork.users[%d]",clientindex),]

	if(clientindex==0)
	{
		plot.default(targetmodule$usertraffic, targetmodule$throughputbits, type='n', yaxt = 'n', ylim=c(0,1500000),
			mainlabel="Users Throughput", xlabel="Rate", ylabel="Throughput")
		legend("topleft", inset=.05, cex = 1, title="Legend", c("User0","User1","User2","User3","User4","User5","User6","User7","User8","User9"), 
			lty=c(1,1), lwd=c(2,2), pch=1:10, col=1:10, bg="grey96")
	}

	lines(targetmodule$usertraffic, targetmodule$throughputbits, yaxt = 'n', col=clientindex+1);
	points(targetmodule$usertraffic, targetmodule$throughputbits, yaxt = 'n', pch=clientindex+1, col=clientindex+1);
}

globaltrafficTicks = axTicks(2)
axis(2, at = globaltrafficTicks, labels = formatC(globaltrafficTicks, format = 'd'))

# compute global antenna traffic summing all client traffics
agg_globaltraffic <- aggregate(list(throughputbits=agg_clients$throughputbits), by = list(usertraffic=agg_clients$usertraffic), sum)
agg_globaltraffic$usertraffic <- factor(agg_globaltraffic$usertraffic)

plotUserThroughput(agg_globaltraffic$usertraffic, agg_globaltraffic$throughputbits,
	mainlabel="Global Antenna Throughput", xlabel="Rate", ylabel="Throughput")

## wait for user input (do not close the window after script end)
b <- scan("stdin", character(), n=1)