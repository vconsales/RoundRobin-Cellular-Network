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
db <- loadDataset("*.vec")
rawvec <- loadVectors(db, NULL)
veclist <- split(rawvec$vectordata, rawvec$vectordata$resultkey)

# window properties
X11(width=14, height=7)
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



b <- scan("stdin", character(), n=1)