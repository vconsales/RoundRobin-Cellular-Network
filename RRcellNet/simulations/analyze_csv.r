confidence <- function(mean, stdev, n) {
	error <- qt(0.975,df=n-1)*stdev/sqrt(n)
	down <- mean-error
	up <- mean+error
	return(c("confmin"=down, "confmax"=up))
}

plotUserThroughput <- function(xdata, ydata, mainlabel, xlabel, ylabel) {
	plot.default(xdata, ydata, type='n', yaxt = 'n', main=mainlabel, xlab=xlabel, ylab=ylabel)
	lines(xdata, ydata, yaxt = 'n')

	globaltrafficTicks = axTicks(2)
	axis(2, at = globaltrafficTicks, labels = formatC(globaltrafficTicks, format = 'd'))

	# mean constant line
	abline(max(as.numeric(ydata)),0, col="red")
}

BinomialCQI_csv <- read.csv(file="data.csv", header=TRUE, sep=",")
AllThroughputBits <- BinomialCQI_csv[BinomialCQI_csv$type=="scalar" & BinomialCQI_csv$name=="throughputBits:last", c("run", "module", "value")]
colnames(AllThroughputBits)[3] <- "throughput"
AllThroughputBits$throughput <- as.numeric(as.character(AllThroughputBits$throughput))

AllRepetitions <- BinomialCQI_csv[BinomialCQI_csv$type=="runattr" & BinomialCQI_csv$attrname=="repetition", c("run", "attrvalue")]
colnames(AllRepetitions)[2] <- "repetition"
AllRepetitions$repetition <- as.numeric(as.character(AllRepetitions$repetition))

AllUserLambdas <- BinomialCQI_csv[BinomialCQI_csv$type=="itervar" & BinomialCQI_csv$attrname=="usertraffic", c("run", "attrvalue")]
colnames(AllUserLambdas)[2] <- "usertraffic"
AllUserLambdas$usertraffic <- as.numeric(as.character(AllUserLambdas$usertraffic))

temp_merge1 <- merge(AllThroughputBits, AllRepetitions)
mergedScalars <- merge(temp_merge1, AllUserLambdas)

# group by lambda and client. Compute mean and stdev for throughput values
agg_clients <- do.call(data.frame, aggregate(list(values=mergedScalars$throughput),
	by = list(module=mergedScalars$module, usertraffic=mergedScalars$usertraffic),
	function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x)))))

X11(width=14, height=7)
par(mfrow=c(1,2))

for (clientindex in 0:9){
	targetmodule = agg_clients[agg_clients$module == sprintf("CellularNetwork.users[%d]",clientindex),]
	if(clientindex==0)
	{
		plot(x=targetmodule$usertraffic, y=targetmodule$values.mean, type='n', yaxt = 'n', ylim=c(0,1500000),
			mainlabel="Users Throughput", xlabel="Rate", ylabel="Throughput")
		legend("topleft", inset=.05, cex = 1, title="Legend", c("User0","User1","User2","User3","User4","User5","User6","User7","User8","User9"), 
			lty=c(1,1), lwd=c(2,2), pch=1:10, col=1:10, bg="grey96")
	}

	lines(targetmodule$usertraffic, targetmodule$values.mean, yaxt = 'n', col=clientindex+1);
	points(targetmodule$usertraffic, targetmodule$values.mean, yaxt = 'n', pch=clientindex+1, col=clientindex+1);
}

globaltrafficTicks = axTicks(2)
axis(2, at = globaltrafficTicks, labels = formatC(globaltrafficTicks, format = 'd'))

# compute global antenna traffic summing all client traffics
agg_globaltraffic <- aggregate(list(throughputbits=agg_clients$values.mean), by = list(usertraffic=agg_clients$usertraffic), sum)

plotUserThroughput(agg_globaltraffic$usertraffic, agg_globaltraffic$throughputbits,
	mainlabel="Global Antenna Throughput", xlabel="Rate", ylabel="Throughput")

b <- scan("stdin", character(), n=1)