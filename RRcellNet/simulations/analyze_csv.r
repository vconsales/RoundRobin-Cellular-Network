confidence <- function(mean, stdev, n) {
	error <- qt(0.975,df=n-1)*stdev/sqrt(n)
	down <- mean-error
	up <- mean+error
	return(c("confmin"=down, "confmax"=up))
}

aggregateMeasures <- function(csvfile) {
	csvData <- read.csv(file=csvfile, header=TRUE, sep=",")

	AllThroughputBits <- csvData[csvData$type=="scalar" & csvData$name=="throughputBits:last", c("run", "module", "value")]
	colnames(AllThroughputBits)[3] <- "throughput"
	AllThroughputBits$throughput <- as.numeric(as.character(AllThroughputBits$throughput))

	AllResponseTimes <- csvData[csvData$type=="scalar" & csvData$name=="responseTime:mean", c("run", "module", "value")]
	colnames(AllResponseTimes)[3] <- "responsetime"
	AllResponseTimes$responsetime <- as.numeric(as.character(AllResponseTimes$responsetime))

	AllRepetitions <- csvData[csvData$type=="runattr" & csvData$attrname=="repetition", c("run", "attrvalue")]
	colnames(AllRepetitions)[2] <- "repetition"
	AllRepetitions$repetition <- as.numeric(as.character(AllRepetitions$repetition))

	AllUserLambdas <- csvData[csvData$type=="itervar" & csvData$attrname=="usertraffic", c("run", "attrvalue")]
	colnames(AllUserLambdas)[2] <- "usertraffic"
	AllUserLambdas$usertraffic <- as.numeric(as.character(AllUserLambdas$usertraffic))

	temp_merge1 <- merge(AllUserLambdas, AllRepetitions)

	## Throughput (on usertraffic variation)
	throughput_mergedScalars <- merge(temp_merge1, AllThroughputBits)

	# group by lambda and client. Compute mean and stdev for throughput values
	throughput_agg_clients <- aggregate(list(values=throughput_mergedScalars$throughput),
		by = list(module=throughput_mergedScalars$module, usertraffic=throughput_mergedScalars$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(throughput_agg_clients)[3] <- "throughput"

	# trasform confidence(...) vectors into columns
	throughput_agg_clients <- do.call(data.frame, throughput_agg_clients)

	## Response time (on usertraffic variation)
	responsetime_mergedScalars <- merge(temp_merge1, AllResponseTimes)

	# group by lambda and client. Compute mean and stdev for responsetime values
	responsetime_agg_clients <- aggregate(list(values=responsetime_mergedScalars$responsetime),
		by = list(module=responsetime_mergedScalars$module, usertraffic=responsetime_mergedScalars$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(responsetime_agg_clients)[3] <- "responsetime"

	# trasform confidence(...) vectors into columns
	responsetime_agg_clients <- do.call(data.frame, responsetime_agg_clients)

	# returning the merged dataframe
	allStats <- merge(throughput_agg_clients, responsetime_agg_clients)
	return(allStats)
}

computeAntennaMeasures <- function(clientsdata) {
	# compute global antenna traffic summing all client traffics
	agg_globaltraffic <- aggregate(list(antennathroughput=clientsdata$throughput.mean), by = list(usertraffic=clientsdata$usertraffic), sum)

	return(agg_globaltraffic)
}

plotAllModulesThroughput <- function(plotdata) {
	for (clientindex in 0:9){
		targetmodule = plotdata[plotdata$module == sprintf("CellularNetwork.users[%d]",clientindex),]
		#print(targetmodule)
		if(clientindex==0)
		{
			plot(x=targetmodule$usertraffic, y=targetmodule$throughput.mean, type='n', yaxt = 'n', ylim=c(0,2600000),
				mainlabel="Users Throughput", xlabel="Rate", ylabel="Throughput")
			legend("topleft", inset=.05, cex = 1, title="Legend", c("User0","User1","User2","User3","User4","User5","User6","User7","User8","User9"), 
				lty=c(1,1), lwd=c(2,2), pch=1:10, col=1:10, bg="grey96")
		}

		lines(targetmodule$usertraffic, targetmodule$throughput.mean, yaxt = 'n', col=clientindex+1);
		points(targetmodule$usertraffic, targetmodule$throughput.mean, yaxt = 'n', pch=clientindex+1, col=clientindex+1);
	}

	globaltrafficTicks = axTicks(2)
	axis(2, at = globaltrafficTicks, labels = formatC(globaltrafficTicks, format = 'd'))
}

plotAllModulesResponseTime <- function(plotdata) {
	for (clientindex in 0:9){
		targetmodule = plotdata[plotdata$module == sprintf("CellularNetwork.users[%d]",clientindex),]
		#print(targetmodule)
		if(clientindex==0)
		{
			plot(x=targetmodule$usertraffic, y=targetmodule$responsetime.mean, type='n', ylim=c(0,0.035),
				mainlabel="Users Response Time", xlabel="Rate", ylabel="Response Time")
			legend("topleft", inset=.05, cex = 1, title="Legend", c("User0","User1","User2","User3","User4","User5","User6","User7","User8","User9"), 
				lty=c(1,1), lwd=c(2,2), pch=1:10, col=1:10, bg="grey96")
		}

		lines(targetmodule$usertraffic, targetmodule$responsetime.mean, yaxt = 'n', col=clientindex+1);
		points(targetmodule$usertraffic, targetmodule$responsetime.mean, yaxt = 'n', pch=clientindex+1, col=clientindex+1);
	}
}

plotModuleComparision <- function(plotdata1, moduleindex1, plotdata2, moduleindex2) {
	targetmodule1 = plotdata1[plotdata1$module == sprintf("CellularNetwork.users[%d]",moduleindex1),]
	targetmodule2 = plotdata2[plotdata2$module == sprintf("CellularNetwork.users[%d]",moduleindex2),]

	legendvec <- c(
		sprintf("%s User%d", deparse(substitute(plotdata1)), moduleindex1),
		sprintf("%s User%d", deparse(substitute(plotdata2)), moduleindex2)
		)

	## Throughput
	plot(x=targetmodule1$usertraffic, y=targetmodule1$throughput.mean, type='n', ylim=c(0,1800000),
		mainlabel="Throughput Comparision", xlab="Rate", ylab="Throughput (bit/s)")
	legend("topleft", inset=.05, cex = 1, title="Legend", legendvec,
		lty=c(1,1), lwd=c(2,2), pch=1:10, col=1:10, bg="grey96")

	lines(targetmodule1$usertraffic, targetmodule1$throughput.mean, yaxt = 'n', col=1);
	points(targetmodule1$usertraffic, targetmodule1$throughput.mean, yaxt = 'n', pch=1, col=1);
	#points(targetmodule1$usertraffic, targetmodule1$throughput.confmin, yaxt = 'n', pch=1, col=1);
	#points(targetmodule1$usertraffic, targetmodule1$throughput.confmax, yaxt = 'n', pch=1, col=1);

	lines(targetmodule2$usertraffic, targetmodule2$throughput.mean, yaxt = 'n', col=2);
	points(targetmodule2$usertraffic, targetmodule2$throughput.mean, yaxt = 'n', pch=2, col=2);
	#points(targetmodule2$usertraffic, targetmodule2$throughput.confmin, yaxt = 'n', pch=2, col=2);
	#points(targetmodule2$usertraffic, targetmodule2$throughput.confmax, yaxt = 'n', pch=2, col=2);

	## Resp Time
	plot(x=targetmodule1$usertraffic, y=targetmodule1$responsetime.mean, type='n', ylim=c(0,0.035),
		mainlabel="Response Time Comparision", xlab="Rate", ylab="Response Time")
	legend("topleft", inset=.05, cex = 1, title="Legend", legendvec,
		lty=c(1,1), lwd=c(2,2), pch=1:10, col=1:10, bg="grey96")

	lines(targetmodule1$usertraffic, targetmodule1$responsetime.mean, yaxt = 'n', col=1);
	points(targetmodule1$usertraffic, targetmodule1$responsetime.mean, yaxt = 'n', pch=1, col=1);

	lines(targetmodule2$usertraffic, targetmodule2$responsetime.mean, yaxt = 'n', col=2);
	points(targetmodule2$usertraffic, targetmodule2$responsetime.mean, yaxt = 'n', pch=2, col=2);
}

# disable scientific notation
options(scipen = 999)

# load all experiments data from CSVs
uniformData <- aggregateMeasures("data_uni.csv")
uniformBestCQIData <- aggregateMeasures("data_uni_bestcqi.csv")
binomialData <- aggregateMeasures("data_binom.csv")
binomialBestCQIData <- aggregateMeasures("data_binom_bestcqi.csv")

# open a new window with 1 row x 2 column graphs
X11(width=14, height=7)
par(mfrow=c(1,2))

## Throughput (on usertraffic variation)
plotAllModulesThroughput(binomialBestCQIData);

## Response time (on usertraffic variation)
plotAllModulesResponseTime(binomialBestCQIData);

# open a new window with 2 row x 2 column graphs
X11(width=10, height=8)
par(mfrow=c(4,2))

plotModuleComparision(uniformData, 0, uniformBestCQIData, 0)
plotModuleComparision(uniformData, 1, uniformBestCQIData, 1)
plotModuleComparision(uniformData, 2, uniformBestCQIData, 2)
plotModuleComparision(uniformData, 3, uniformBestCQIData, 3)

X11(width=10, height=8)
par(mfrow=c(3,2))

plotModuleComparision(binomialData, 0, binomialBestCQIData, 0)
plotModuleComparision(binomialData, 1, binomialBestCQIData, 1)
plotModuleComparision(binomialData, 2, binomialBestCQIData, 2)

X11(width=10, height=8)
par(mfrow=c(3,2))

plotModuleComparision(binomialData, 3, binomialBestCQIData, 3)
plotModuleComparision(binomialData, 4, binomialBestCQIData, 4)
plotModuleComparision(binomialData, 5, binomialBestCQIData, 5)

X11(width=10, height=8)
par(mfrow=c(3,2))

plotModuleComparision(binomialData, 6, binomialBestCQIData, 6)
plotModuleComparision(binomialData, 7, binomialBestCQIData, 7)
#globaltrafficTicks = axTicks(2)
#axis(2, at = globaltrafficTicks, labels = formatC(globaltrafficTicks, format = 'd'))


antennaUniform <- computeAntennaMeasures(uniformData)
antennaUniformBestCQI <- computeAntennaMeasures(uniformBestCQIData)
antennaBinomial <- computeAntennaMeasures(binomialData)
antennaBinomialBestCQI <- computeAntennaMeasures(binomialBestCQIData)

X11()
par(mfrow=c(1,1))
plot(antennaUniform$usertraffic, antennaUniform$antennathroughput, type='n', xlim=c(0,9), ylim=c(0,9500000))
points(antennaUniform$usertraffic, antennaUniform$antennathroughput, col=2)
lines(antennaUniform$usertraffic, antennaUniform$antennathroughput, col=2)
text(4.1, max(antennaUniform), round(max(antennaUniform)), col=2, pos=3, cex= 0.7)

lines(antennaUniformBestCQI$usertraffic, antennaUniformBestCQI$antennathroughput, col=3)
points(antennaUniformBestCQI$usertraffic, antennaUniformBestCQI$antennathroughput, col=3)
text(4.1, max(antennaUniformBestCQI), round(max(antennaUniformBestCQI)), col=3, pos=3, cex= 0.7)

points(antennaBinomial$usertraffic, antennaBinomial$antennathroughput, col=4)
lines(antennaBinomial$usertraffic, antennaBinomial$antennathroughput, col=4)
text(8.1, max(antennaBinomial), round(max(antennaBinomial)), col=4, pos=3, cex= 0.7)

lines(antennaBinomialBestCQI$usertraffic, antennaBinomialBestCQI$antennathroughput, col=5)
points(antennaBinomialBestCQI$usertraffic, antennaBinomialBestCQI$antennathroughput, col=5)
text(8.1, max(antennaBinomialBestCQI), round(max(antennaBinomialBestCQI)), col=5, pos=3, cex= 0.7)

legend("topleft", inset=.05, cex = 1, title="Legend", c("unif1","unif2", "binom1", "binom2"),
		lty=c(1,1), lwd=c(2,2), pch=1:4, col=2:5, bg="grey96")
#plotUserThroughput(agg_globaltraffic$usertraffic, agg_globaltraffic$throughputbits,
#	mainlabel="Global Antenna Throughput", xlabel="Rate", ylabel="Throughput")

b <- scan("stdin", character(), n=1)