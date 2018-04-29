library(ineq)
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

prepareMeasures <- function(csvfile) {
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
	temp_merge3 <- merge(temp_merge2, AllThroughputBits)
	merged_result <- merge(temp_merge3, AllResponseTimes)

	return(merged_result)
}

aggregateClientMeasures <- function(measures) {
	## Throughput (on usertraffic variation)

	# group by lambda and client. Compute mean and stdev for throughput values
	throughput_agg_clients <- aggregate(list(values=measures$throughput),
		by = list(module=measures$module, scenario=measures$scenario, usertraffic=measures$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(throughput_agg_clients)[4] <- "throughput"

	# trasform confidence(...) vectors into columns
	throughput_agg_clients <- do.call(data.frame, throughput_agg_clients)

	## Response time (on usertraffic variation)

	# group by lambda and client. Compute mean and stdev for responsetime values
	responsetime_agg_clients <- aggregate(list(values=measures$responsetime),
		by = list(module=measures$module, scenario=measures$scenario, usertraffic=measures$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(responsetime_agg_clients)[4] <- "responsetime"

	# trasform confidence(...) vectors into columns
	responsetime_agg_clients <- do.call(data.frame, responsetime_agg_clients)

	# merging throughput and responsetime data frames
	allStats <- merge(throughput_agg_clients, responsetime_agg_clients)

	# returning the merged dataframe
	return(allStats)
}

aggregateAntennaMeasures <- function(measures) {
	throughput_agg_antenna <- aggregate(list(values=measures$throughput),
		by = list(scenario=measures$scenario, usertraffic=measures$usertraffic, repetition=measures$repetition),
		sum)
	colnames(throughput_agg_antenna)[4] <- "antennathroughput"

	# group by lambda and client. Compute mean and stdev for throughput values
	throughput_agg <- aggregate(list(values=throughput_agg_antenna$antennathroughput),
		by = list(scenario=throughput_agg_antenna$scenario, usertraffic=throughput_agg_antenna$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(throughput_agg)[3] <- "antennathroughput"

	# trasform confidence(...) vectors into columns
	throughput_agg <- do.call(data.frame, throughput_agg)

	# returning the merged dataframe
	return(throughput_agg)
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

plotLorentzCurvePerRate <- function(plotdata, clientratemin, clientratemax, clientratestep) {
	cumulativedf <- data.frame();
	for(rate in seq(clientratemin, clientratemax, by=clientratestep))
	{
		targetrate <- plotdata[plotdata$usertraffic==rate,]
		lcdata <- Lc(targetrate$throughput.mean)
		lcdataframe <- data.frame(lcdata[1], lcdata[2], clientrate=rep(rate, length(lcdata$p)))
		# if first iteration
		if(rate == clientratemin)
			cumulativedf <- lcdataframe
		else
			cumulativedf <- rbind(cumulativedf, lcdataframe)
	}

	resplot <- ggplot(cumulativedf, aes(x=p, y=L, color=clientrate, group=clientrate)) +
		geom_abline() +
		geom_line() + scale_color_gradient(low="#3fc601", high="#ff0f0f") +
		ggtitle(sprintf("Lorentz Curve Per Rate Throughputs (%s)", deparse(substitute(plotdata)))) +
		theme(plot.title = element_text(hjust = 0.5))

	multiplot(resplot)
}

plotLorentzCurvePerRateResponseTimes <- function(plotdata, clientratemin, clientratemax, clientratestep) {
	cumulativedf <- data.frame();
	for(rate in seq(clientratemin, clientratemax, by=clientratestep))
	{
		targetrate <- plotdata[plotdata$usertraffic==rate,]
		lcdata <- Lc(targetrate$responsetime.mean)
		lcdataframe <- data.frame(lcdata[1], lcdata[2], clientrate=rep(rate, length(lcdata$p)))
		# if first iteration
		if(rate == clientratemin)
			cumulativedf <- lcdataframe
		else
			cumulativedf <- rbind(cumulativedf, lcdataframe)
	}

	resplot <- ggplot(cumulativedf, aes(x=p, y=L, color=clientrate, group=clientrate)) +
		geom_abline() +
		geom_line() + scale_color_gradient(low="#3fc601", high="#ff0f0f") +
		ggtitle(sprintf("Lorentz Curve Per Rate Response Times (%s)", deparse(substitute(plotdata)))) +
		theme(plot.title = element_text(hjust = 0.5))


	multiplot(resplot)
}

plotLorentzCurveComparision <- function(plotdata1, plotdata2, clientrate) {
	targetrate1 <- plotdata1[plotdata1$usertraffic==clientrate,]
	lcdata1 <- Lc(targetrate1$throughput.mean)

	targetrate2 <- plotdata2[plotdata2$usertraffic==clientrate,]
	lcdata2 <- Lc(targetrate2$throughput.mean)

	lcplotdata <- rbind(data.frame(lcdata1[1], lcdata1[2], dataset=rep(deparse(substitute(plotdata1)), length(lcdata1$p))),
						data.frame(lcdata2[1], lcdata2[2], dataset=rep(deparse(substitute(plotdata2)), length(lcdata2$p))))

	resplot <- ggplot(lcplotdata, aes(x=p, y=L, color=dataset, group=dataset)) +
		geom_abline() +
		geom_line() +
		ggtitle(sprintf("Lorentz Curve Throughput Comparision (Rate = %s)", clientrate)) +
		theme(plot.title = element_text(hjust = 0.5))

	multiplot(resplot)
}

plotThroughputEcdfComparision <- function(prepdata1, prepdata2, clientrate, moduleindex) {
	targetdata1 <- prepdata1[prepdata1$usertraffic==clientrate & prepdata1$module==sprintf("CellularNetwork.users[%d]",moduleindex),]
	targetdata2 <- prepdata2[prepdata2$usertraffic==clientrate & prepdata2$module==sprintf("CellularNetwork.users[%d]",moduleindex),]
	targetdata <- rbind(targetdata1, targetdata2)

	resplot <- ggplot(targetdata, aes(x=throughput)) +
				stat_ecdf(aes(group=scenario, color=scenario))

	multiplot(resplot)
}

plotBoxplotThroughputComparision <- function(prepdata1, prepdata2, clientrate, moduleindex) {
	targetdata1 <- prepdata1[prepdata1$usertraffic==clientrate & prepdata1$module==sprintf("CellularNetwork.users[%d]",moduleindex),]
	targetdata2 <- prepdata2[prepdata2$usertraffic==clientrate & prepdata2$module==sprintf("CellularNetwork.users[%d]",moduleindex),]
	targetdata <- rbind(targetdata1, targetdata2)

	resplot <- ggplot(targetdata, aes(x=usertraffic, y=throughput)) +
				geom_boxplot(aes(group=interaction(usertraffic,scenario), fill=scenario))

	multiplot(resplot)
}

# disable scientific notation
options(scipen = 999)

# open a new window with 1 row x 2 column graphs
X11(width=14, height=7)


## REGRESSION TEST

regressionTestData <- aggregateClientMeasures(prepareMeasures("data_regr.csv"))

## Throughput (on usertraffic variation)
plotAllModulesStatistics(regressionTestData)
waitForClick()

## USERS STATISTICS

# load all experiments data from CSVs
preparedUniformData <- prepareMeasures("data_uni.csv")
preparedUniformBestCQIData <- prepareMeasures("data_uni_bestcqi.csv")
preparedBinomialData <- prepareMeasures("data_binom.csv")
preparedBinomialBestCQIData <- prepareMeasures("data_binom_bestcqi.csv")

# compute confidence intervals and means for each user (and for each scenario)
uniformData <- aggregateClientMeasures(preparedUniformData)
uniformBestCQIData <- aggregateClientMeasures(preparedUniformBestCQIData)
binomialData <- aggregateClientMeasures(preparedBinomialData)
binomialBestCQIData <- aggregateClientMeasures(preparedBinomialBestCQIData)

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

plotLorentzCurvePerRate(uniformData, 0.1, 4.1, 0.5)
waitForClick()
plotLorentzCurvePerRate(uniformBestCQIData, 0.1, 4.1, 0.5)
waitForClick()
plotLorentzCurvePerRateResponseTimes(uniformData, 0.1, 4.1, 0.5)
waitForClick()
plotLorentzCurvePerRateResponseTimes(uniformBestCQIData, 0.1, 4.1, 0.5)
waitForClick()

plotLorentzCurvePerRate(binomialData, 0.1, 8.1, 0.5)
waitForClick()
plotLorentzCurvePerRate(binomialBestCQIData, 0.1, 8.1, 0.5)
waitForClick()
plotLorentzCurvePerRateResponseTimes(binomialData, 0.1, 8.1, 0.5)
waitForClick()
plotLorentzCurvePerRateResponseTimes(binomialBestCQIData, 0.1, 8.1, 0.5)
waitForClick()

# Lorentz curve comparision per rate (binomial, binomialbestcqi)
for(rate in seq(0.6, 4.1, by=0.5))
{
	plotLorentzCurveComparision(uniformData, uniformBestCQIData, rate)
	waitForClick()
}

# Lorentz curve comparision per rate (uniform, uniformbestcqi)
for(rate in seq(0.6, 8.1, by=0.5))
{
	plotLorentzCurveComparision(binomialData, binomialBestCQIData, rate)
	waitForClick()
}

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

antennaUniform <- aggregateAntennaMeasures(preparedUniformData)
antennaUniformBestCQI <- aggregateAntennaMeasures(preparedUniformBestCQIData)
antennaBinomial <- aggregateAntennaMeasures(preparedBinomialData)
antennaBinomialBestCQI <- aggregateAntennaMeasures(preparedBinomialBestCQIData)

antennaAll <- rbind(antennaUniform, antennaUniformBestCQI, antennaBinomial, antennaBinomialBestCQI)

antenna_graph <- ggplot(antennaAll, aes(x=usertraffic, y=antennathroughput.mean, colour=scenario, group=scenario)) +
	geom_line() +
	geom_errorbar(aes(ymin=antennathroughput.confmin, ymax=antennathroughput.confmax, width=.1)) +
	geom_text(aes(label=ifelse(scenario=="UniformCQI" & antennathroughput.mean==max(antennaUniform$antennathroughput.mean),
		floor(max(antennaUniform$antennathroughput.mean)), '')), hjust=0.5, vjust=-0.5) +
	geom_text(aes(label=ifelse(scenario=="UniformCQI_bestCQIScheduler" & antennathroughput.mean==max(antennaUniformBestCQI$antennathroughput.mean),
		floor(max(antennaUniformBestCQI$antennathroughput.mean)), '')), hjust=0.5, vjust=-0.5) +
	geom_text(aes(label=ifelse(scenario=="BinomialCQI" & antennathroughput.mean==max(antennaBinomial$antennathroughput.mean),
		floor(max(antennaBinomial$antennathroughput.mean)), '')), hjust=0.5, vjust=-0.5) +
	geom_text(aes(label=ifelse(scenario=="BinomialCQI_bestCQIScheduler" & antennathroughput.mean==max(antennaBinomialBestCQI$antennathroughput.mean),
		floor(max(antennaBinomialBestCQI$antennathroughput.mean)), '')), hjust=0.5, vjust=-0.5)

multiplot(antenna_graph)
waitForClick()

invisible(dev.off())