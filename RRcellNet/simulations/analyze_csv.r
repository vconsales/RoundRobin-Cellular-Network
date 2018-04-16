library(ggplot2)
library(gridExtra)
library(grid)

# == Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ==
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	library(grid)
	plots <- c(list(...), plotlist)
	numPlots = length(plots)

	if (is.null(layout)) {
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
		ncol = cols, nrow = ceiling(numPlots/cols))
	}

	if (numPlots==1) {
		print(plots[[1]])
	} else {
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
		for (i in 1:numPlots) {
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
			layout.pos.col = matchidx$col))
		}
	}
}

# Source (Edited): https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots/28594060
plotdouble_singlelegend <- function(p1, p2) {
	g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs
	legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

	grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
								p2 + theme(legend.position="none"),
								nrow=1),
				legend, nrow=2,heights=c(10, 1))
}
# =====================================================================================

waitForClick <- function() {
	invisible(grid::grid.locator())
}

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

	AllConfigNames <- csvData[csvData$type=="runattr" & csvData$attrname=="configname", c("run", "attrvalue")]
	colnames(AllConfigNames)[2] <- "scenario"

	temp_merge1 <- merge(AllUserLambdas, AllRepetitions)
	temp_merge2 <- merge(temp_merge1, AllConfigNames)

	## Throughput (on usertraffic variation)
	throughput_mergedScalars <- merge(temp_merge2, AllThroughputBits)

	# group by lambda and client. Compute mean and stdev for throughput values
	throughput_agg_clients <- aggregate(list(values=throughput_mergedScalars$throughput),
		by = list(module=throughput_mergedScalars$module, scenario=throughput_mergedScalars$scenario, usertraffic=throughput_mergedScalars$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(throughput_agg_clients)[4] <- "throughput"

	# trasform confidence(...) vectors into columns
	throughput_agg_clients <- do.call(data.frame, throughput_agg_clients)

	## Response time (on usertraffic variation)
	responsetime_mergedScalars <- merge(temp_merge2, AllResponseTimes)

	# group by lambda and client. Compute mean and stdev for responsetime values
	responsetime_agg_clients <- aggregate(list(values=responsetime_mergedScalars$responsetime),
		by = list(module=responsetime_mergedScalars$module, scenario=throughput_mergedScalars$scenario, usertraffic=responsetime_mergedScalars$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(responsetime_agg_clients)[4] <- "responsetime"

	# trasform confidence(...) vectors into columns
	responsetime_agg_clients <- do.call(data.frame, responsetime_agg_clients)

	# merging throughput and responsetime data frames
	allStats <- merge(throughput_agg_clients, responsetime_agg_clients)

	# returning the merged dataframe
	return(allStats)
}

computeAntennaMeasures <- function(clientsdata) {
	# compute global antenna traffic summing all client traffics
	agg_globaltraffic <- aggregate(list(antennathroughput=clientsdata$throughput.mean),
							by = list(usertraffic=clientsdata$usertraffic, scenario=clientsdata$scenario), sum)

	return(agg_globaltraffic)
}

plotSingleModuleThroughput <- function(plotdata, clientindex) {
	targetmodule = plotdata[plotdata$module == sprintf("CellularNetwork.users[%d]",clientindex),]
	targetmodule = targetmodule[order(targetmodule$usertraffic),]
	#print(targetmodule)

	res <- ggplot(targetmodule, aes(x=usertraffic, y=throughput.mean)) +
	geom_line() +
	geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1))

	return(res)
}

plotSingleModuleResponseTime <- function(plotdata, clientindex) {
	targetmodule = plotdata[plotdata$module == sprintf("CellularNetwork.users[%d]",clientindex),]
	targetmodule = targetmodule[order(targetmodule$usertraffic),]
	#print(targetmodule)

	res <- ggplot(targetmodule, aes(x=usertraffic, y=responsetime.mean)) +
	geom_line() +
	geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1))

	return(res)
}

plotAllModulesStatistics <- function(plotdata) {
	plot_th <- ggplot(plotdata, aes(x=usertraffic, y=throughput.mean, colour=module, group=module)) +
	geom_line() +
	geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1)) +
	theme(legend.position="bottom")

	plot_rt <- ggplot(plotdata, aes(x=usertraffic, y=responsetime.mean, colour=module, group=module)) +
	geom_line() +
	geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1))

	plotdouble_singlelegend(plot_th, plot_rt);
}

plotModuleComparision <- function(plotdata1, moduleindex1, plotdata2, moduleindex2) {
	targetmodule1 = plotdata1[plotdata1$module == sprintf("CellularNetwork.users[%d]",moduleindex1),]
	targetmodule2 = plotdata2[plotdata2$module == sprintf("CellularNetwork.users[%d]",moduleindex2),]

	plotdata <- rbind(targetmodule1, targetmodule2)
	#print(plotdata)

	plot_th <- ggplot(plotdata, aes(x=usertraffic, y=throughput.mean, colour=interaction(module, scenario), group=interaction(module, scenario))) +
		geom_line() +
		geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1))

	plot_rt <- ggplot(plotdata, aes(x=usertraffic, y=responsetime.mean, colour=interaction(module, scenario), group=interaction(module, scenario))) +
		geom_line() +
		geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1))

	plotdouble_singlelegend(plot_th, plot_rt)
}

# disable scientific notation
options(scipen = 999)

# open a new window with 1 row x 2 column graphs
X11(width=14, height=7)


## REGRESSION TEST

regressionTestData <- aggregateMeasures("data_regr.csv")

## Throughput (on usertraffic variation)
plotAllModulesStatistics(regressionTestData)
waitForClick()

## USERS STATISTICS

# load all experiments data from CSVs
uniformData <- aggregateMeasures("data_uni.csv")
uniformBestCQIData <- aggregateMeasures("data_uni_bestcqi.csv")
binomialData <- aggregateMeasures("data_binom.csv")
binomialBestCQIData <- aggregateMeasures("data_binom_bestcqi.csv")

# open a new window with 1 row x 2 column graphs

## Throughput (on usertraffic variation)
plotAllModulesStatistics(uniformData)
waitForClick()
plotAllModulesStatistics(uniformBestCQIData)
waitForClick()
plotAllModulesStatistics(binomialData)
waitForClick()
plotAllModulesStatistics(binomialBestCQIData)
waitForClick()

# plot user statistics for uniform and uniform bestcqi scenario
for(clientindex in 0:9)
{
	plotModuleComparision(uniformData, clientindex, uniformBestCQIData, clientindex)
	waitForClick()
}

# plot user statistics for binomial and binomial bestcqi scenario
for(clientindex in 0:9)
{
	plotModuleComparision(binomialData, clientindex, binomialBestCQIData, clientindex)
	waitForClick()
}

# ANTENNA STATISTICS

antennaUniform <- computeAntennaMeasures(uniformData)
antennaUniformBestCQI <- computeAntennaMeasures(uniformBestCQIData)
antennaBinomial <- computeAntennaMeasures(binomialData)
antennaBinomialBestCQI <- computeAntennaMeasures(binomialBestCQIData)

antennaAll <- rbind(antennaUniform, antennaUniformBestCQI, antennaBinomial, antennaBinomialBestCQI)

antenna_graph <- ggplot(antennaAll, aes(x=usertraffic, y=antennathroughput, colour=scenario, group=scenario)) +
	geom_point() +
	geom_line() +
	geom_text(aes(label=ifelse(scenario=="UniformCQI" & antennathroughput==max(antennaUniform$antennathroughput),
		floor(max(antennaUniform$antennathroughput)), '')), hjust=0.5, vjust=-0.5) +
	geom_text(aes(label=ifelse(scenario=="UniformCQI_bestCQIScheduler" & antennathroughput==max(antennaUniformBestCQI$antennathroughput),
		floor(max(antennaUniformBestCQI$antennathroughput)), '')), hjust=0.5, vjust=-0.5) +
	geom_text(aes(label=ifelse(scenario=="BinomialCQI" & antennathroughput==max(antennaBinomial$antennathroughput),
		floor(max(antennaBinomial$antennathroughput)), '')), hjust=0.5, vjust=-0.5) +
	geom_text(aes(label=ifelse(scenario=="BinomialCQI_bestCQIScheduler" & antennathroughput==max(antennaBinomialBestCQI$antennathroughput),
		floor(max(antennaBinomialBestCQI$antennathroughput)), '')), hjust=0.5, vjust=-0.5)

multiplot(antenna_graph)
waitForClick()

invisible(dev.off())