library(ineq)
library(ggplot2)
library(gridExtra)
library(grid)
library(tikzDevice)

basedir <- "./csv_results/"
THROUGHPUT_MARGIN <- 40000

# == Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ==
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	library(grid)
	plots <- c(list(...), plotlist)
	numPlots = length(plots)

	#ggsave("plot.png", dpi=320)
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

plotdown <- function(p1) {
	p1 <- p1 + theme(legend.position="bottom")
	multiplot(p1)
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

	AllRbCounts <- csvData[csvData$type=="scalar" & csvData$name=="rbCount:mean", c("run", "module", "value")]
	colnames(AllRbCounts)[3] <- "rbcount"
	AllRbCounts$rbcount <- as.numeric(as.character(AllRbCounts$rbcount))

	AllPacketCounts <- csvData[csvData$type=="scalar" & csvData$name=="packetCount:mean", c("run", "module", "value")]
	colnames(AllPacketCounts)[3] <- "packetcount"
	AllPacketCounts$packetcount <- as.numeric(as.character(AllPacketCounts$packetcount))
	AllPacketCounts$module <- gsub("\\CellularNetwork.antenna.queue", "CellularNetwork.users", AllPacketCounts$module)

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
	temp_merge4 <- merge(temp_merge3, AllRbCounts)
	temp_merge5 <- merge(temp_merge4, AllPacketCounts)
	merged_result <- merge(temp_merge5, AllResponseTimes)

	# add inputthroughput universal column, depends on usertraffic
	merged_result$inputthroughput = (312*10^3)*merged_result$usertraffic

	return(merged_result)
}

prepareSchedulerMeasures <- function(csvfile)
{
	csvData <- read.csv(file=csvfile, header=TRUE, sep=",")

	AllFramefilledbCounts <- csvData[csvData$type=="scalar" & csvData$module == "CellularNetwork.antenna.scheduler"
		& csvData$name == "framefilledRbCount:mean", c("run", "value")]
	colnames(AllFramefilledbCounts)[2] <- "framefilledrbcount"
	AllFramefilledbCounts$framefilledrbcount <- as.numeric(as.character(AllFramefilledbCounts$framefilledrbcount))

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
	merged_result <- merge(temp_merge2, AllFramefilledbCounts)

	return(merged_result)
}

aggregateSchedulerMeasures <- function(measures) {
	## FrameFilledRbCount (on usertraffic variation)

	# group by lambda and client. Compute mean and stdev for throughput values
	framefilledrbcount_agg_clients <- aggregate(list(values=measures$framefilledrbcount),
		by = list(scenario=measures$scenario, usertraffic=measures$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(framefilledrbcount_agg_clients)[3] <- "framefilledrbcount"

	# trasform confidence(...) vectors into columns
	framefilledrbcount_agg_clients <- do.call(data.frame, framefilledrbcount_agg_clients)

	# returning the merged dataframe
	return(framefilledrbcount_agg_clients)
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

	## ResourceBlock count (on usertraffic variation)

	# group by lambda and client. Compute mean and stdev for rbcount values
	rbcount_agg_clients <- aggregate(list(values=measures$rbcount),
		by = list(module=measures$module, scenario=measures$scenario, usertraffic=measures$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(rbcount_agg_clients)[4] <- "rbcount"

	# trasform confidence(...) vectors into columns
	rbcount_agg_clients <- do.call(data.frame, rbcount_agg_clients)

	## ResourceBlock count (on usertraffic variation)

	# group by lambda and client. Compute mean and stdev for packetcount values
	packetcount_agg_clients <- aggregate(list(values=measures$packetcount),
		by = list(module=measures$module, scenario=measures$scenario, usertraffic=measures$usertraffic),
		function(x) c(mean=mean(x), stdev=sd(x), samples=length(x), confidence(mean(x), sd(x), length(x))))
	colnames(packetcount_agg_clients)[4] <- "packetcount"

	# trasform confidence(...) vectors into columns
	packetcount_agg_clients <- do.call(data.frame, packetcount_agg_clients)



	# merging throughput, responsetime and rbcount data frames
	partial_merge1 <- merge(throughput_agg_clients, responsetime_agg_clients)
	partial_merge2 <- merge(partial_merge1, rbcount_agg_clients)
	allStats <- merge(partial_merge2, packetcount_agg_clients)

	# add inputthroughput universal column, depends on usertraffic
	allStats$inputthroughput = (312*10^3)*allStats$usertraffic

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

getPlotTicks <- function(plotdata) {
	x_max <- max(plotdata$usertraffic)
	y_max_th <- max(plotdata$throughput.mean)
	y_max_rt <- max(plotdata$responsetime.mean)

	if(x_max < 10)
		x_tick <- 0.5
	else
		x_tick <- round(x_max/14, digits=1)

	y_tick_th <- round(y_max_th/32, digits=0)
	y_tick_rt <- round(y_max_rt/32, digits=3)

	return(list( "th" = list("x_max" = x_max, "x_tick" = x_tick, "y_max" = y_max_th, "y_tick" = y_tick_th),
				 "rt" = list("x_max" = x_max, "x_tick" = x_tick, "y_max" = y_max_rt, "y_tick" = y_tick_rt)))
}

plotAllModulesStatistics <- function(plotdata) {
	pt <- getPlotTicks(plotdata)

	plot_th <- ggplot(plotdata, aes(x=usertraffic, y=throughput.mean, colour=module, group=module)) +
	geom_line() + scale_y_continuous(breaks=seq(0,pt$th$y_max,pt$th$y_tick)) + scale_x_continuous(breaks=seq(0.1,pt$th$x_max,pt$th$x_tick)) +
	geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1)) +
	theme(legend.position="bottom")

	y_max <- round(max(plotdata$responsetime.mean)/(1), digits=0)
	plot_rt <- ggplot(plotdata, aes(x=usertraffic, y=responsetime.mean, colour=module, group=module)) +
	geom_line() + scale_y_continuous(breaks=seq(0,pt$rt$y_max,pt$rt$y_tick)) + scale_x_continuous(breaks=seq(0.1,pt$rt$x_max,pt$rt$x_tick)) +
	geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1))

	#zoommato
	#nonsaturared_data <- plotdata[abs(plotdata$inputthroughput - plotdata$throughput.mean) < THROUGHPUT_MARGIN,]
	#plot_rt <- ggplot(nonsaturared_data, aes(x=usertraffic, y=responsetime.mean, colour=module, group=module)) +
	#geom_line() + scale_x_continuous(breaks=seq(0,x_max,0.5)) +
	#coord_cartesian(ylim = c(0, 0.02)) +
	#geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1))

	plotdouble_singlelegend(plot_th, plot_rt);
}

plotAllModulesStatisticsZoom <- function(plotdata) {
	pt <- getPlotTicks(plotdata)

	plot_th <- ggplot(plotdata, aes(x=usertraffic, y=throughput.mean, colour=module, group=module)) +
	geom_line() + scale_y_continuous(breaks=seq(0,pt$th$y_max,pt$th$y_tick)) + scale_x_continuous(breaks=seq(0.1,pt$th$x_max,pt$th$x_tick)) +
	geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1)) +
	theme(legend.position="bottom")

	y_max = 0.05
	plot_rt <- ggplot(plotdata, aes(x=usertraffic, y=responsetime.mean, colour=module, group=module)) +
	geom_line() + scale_y_continuous(breaks=seq(0,pt$rt$y_max,round(y_max/20, digits=4))) + scale_x_continuous(breaks=seq(0.1,pt$rt$x_max,pt$rt$x_tick)) +
	coord_cartesian(ylim = c(0, y_max)) +
	geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1))

	plotdouble_singlelegend(plot_th, plot_rt);
}

plotAllModulesThroughput <- function(plotdata) {
	pt <- getPlotTicks(plotdata)

	plot_th <- ggplot(plotdata, aes(x=usertraffic, y=throughput.mean, colour=module, group=module)) +
	geom_line() + scale_y_continuous(breaks=seq(0,pt$th$y_max,pt$th$y_tick)) + scale_x_continuous(breaks=seq(0.1,pt$th$x_max,pt$th$x_tick)) +
	geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1))

	plotdown(plot_th);
}

plotAllModulesResponseTimes <- function(plotdata) {
	#nonsaturared_data <- plotdata[abs(plotdata$inputthroughput - plotdata$throughput.mean) < THROUGHPUT_MARGIN,]
	pt <- getPlotTicks(plotdata)
	y_max = 0.05

	plot_rt <- ggplot(plotdata, aes(x=usertraffic, y=responsetime.mean, colour=module, group=module)) +
	geom_line() + scale_x_continuous(breaks=seq(0,pt$rt$x_max,pt$rt$x_tick)) + scale_y_continuous(breaks=seq(0,pt$rt$y_max,round(y_max/20, digits=4))) +
	coord_cartesian(ylim = c(0, y_max)) +
	geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1))

	plotdown(plot_rt);
}

plotAllModulesRBcounts <- function(plotdata) {
	plot_th <- ggplot(plotdata, aes(x=usertraffic, y=throughput.mean, colour=module, group=module)) +
	geom_line() +
	geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1)) +
	theme(legend.position="bottom")

	plot_rb <- ggplot(plotdata, aes(x=usertraffic, y=rbcount.mean, colour=module, group=module)) +
	geom_line() +
	geom_errorbar(aes(ymin=rbcount.confmin, ymax=rbcount.confmax, width=.1))

	plotdouble_singlelegend(plot_th, plot_rb);
}

plotAllModulesRBcountsByTrafficComparision <- function(plotdata1, plotdata2, clientrate) {
	targetrate1 <- plotdata1[plotdata1$usertraffic==clientrate,]
	targetrate2 <- plotdata2[plotdata2$usertraffic==clientrate,]
	plotdata <- rbind(targetrate1, targetrate2)

	# user name is too long to plot on x axis
	plotdata$module <- gsub("\\CellularNetwork.users", "user", plotdata$module)

	plot_rb <- ggplot(plotdata, aes(x=module, fill=scenario, y=rbcount.mean)) +
		geom_bar(stat="identity", position="dodge") +
		geom_errorbar(aes(ymin=rbcount.confmin, ymax=rbcount.confmax, width=.1), position=position_dodge(.9))

	plotdown(plot_rb);
}

plotAllModulesPacketCounts <- function(plotdata) {
	plot_packetcount <- ggplot(plotdata, aes(x=usertraffic, y=packetcount.mean, colour=module, group=module)) +
	geom_line() +
	geom_errorbar(aes(ymin=packetcount.confmin, ymax=packetcount.confmax, width=.1)) +
	theme(legend.position="bottom")

	plotdown(plot_packetcount);
}

plotAllLittle <- function(plotdata) {
	computeddata <- plotdata[abs(plotdata$inputthroughput - plotdata$throughput.mean) < THROUGHPUT_MARGIN, ]
	computeddata$littleresponsetime <- (computeddata$packetcount.mean/computeddata$usertraffic)/1000

	plot_rs <- ggplot(computeddata, aes(x=usertraffic, y=responsetime.mean, colour=module, group=module)) +
	geom_line() + coord_cartesian(ylim = c(0, 0.02)) +
	geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1)) +
	theme(legend.position="bottom")

	plot_little <- ggplot(computeddata, aes(x=usertraffic, y=littleresponsetime, colour=module, group=module)) +
	geom_line() + coord_cartesian(ylim = c(0, 0.02))

	plotdouble_singlelegend(plot_rs, plot_little);
}

plotLittleRegression <- function(plotdata, clientindex) {
	filtereddata <- plotdata[abs(plotdata$inputthroughput - plotdata$throughput) < THROUGHPUT_MARGIN & plotdata$module == sprintf("CellularNetwork.users[%d]",clientindex), ]
	#print(filtereddata)

	m <- lm(responsetime ~ I((packetcount/usertraffic)/1000), filtereddata)

	eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
         list(a = format(coef(m)[1], digits = 2),
              b = format(coef(m)[2], digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3)))

	plot_lr <- ggplot(filtereddata, aes(x=responsetime, y=((packetcount/usertraffic)/1000), colour=module, group=module)) +
	geom_smooth(method="lm") + geom_point() +
	annotate(geom = 'text', label = as.character(as.expression(eq)), x = -Inf, y = Inf, hjust = 0, vjust = 1, parse = TRUE)
	#geom_text(aes(x = 0, y = 0, label = as.character(as.expression(eq))), parse = TRUE)

	multiplot(plot_lr)
}

plotThroughputSaturationLines <- function(plotdata, clientindex) {
	filtereddata <- plotdata[plotdata$module == sprintf("CellularNetwork.users[%d]",clientindex), ]
	x_max <- max(filtereddata$usertraffic)
	y_max <- round(max(filtereddata$throughput.mean)/(1), digits=0)

	plot_sl <- ggplot(filtereddata, aes(x=usertraffic, y=throughput.mean, colour=module, group=module)) + geom_line() +
	geom_line(aes(x=usertraffic, y=inputthroughput, colour=module, group=module)) +
	scale_x_continuous(breaks=seq(0,x_max,0.5)) +
	geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1)) +
	theme(legend.position="bottom")

	multiplot(plot_sl);
}

plotModuleComparision <- function(plotdata1, moduleindex1, plotdata2, moduleindex2) {
	targetmodule1 = plotdata1[plotdata1$module == sprintf("CellularNetwork.users[%d]",moduleindex1),]
	targetmodule2 = plotdata2[plotdata2$module == sprintf("CellularNetwork.users[%d]",moduleindex2),]

	plotdata <- rbind(targetmodule1, targetmodule2)
	#print(plotdata)

	y_max <- round(max(plotdata$throughput.mean)/(1), digits=0)
	x_max <- max(plotdata$usertraffic)

	plot_th <- ggplot(plotdata, aes(x=usertraffic, y=throughput.mean, colour=interaction(module, scenario), group=interaction(module, scenario))) +
		geom_line() + scale_y_continuous(breaks=seq(0,y_max,y_max/32)) + scale_x_continuous(breaks=seq(0,x_max,1)) +
		geom_errorbar(aes(ymin=throughput.confmin, ymax=throughput.confmax, width=.1))
	#ggsave("plotth.png", dpi=320)	

	y_max <- round(max(plotdata$responsetime.mean)/(1), digits=0)	
	plot_rt <- ggplot(plotdata, aes(x=usertraffic, y=responsetime.mean, colour=interaction(module, scenario), group=interaction(module, scenario))) +
		geom_line() + scale_y_continuous(breaks=seq(0,y_max,y_max/32)) + scale_x_continuous(breaks=seq(0,x_max,1)) +
		geom_errorbar(aes(ymin=responsetime.confmin, ymax=responsetime.confmax, width=.1))

	plotdouble_singlelegend(plot_th, plot_rt)

	#ggsave("plotresp.png", dpi=320)
}

plotModuleRBComparision <- function(plotdata1, moduleindex1, plotdata2, moduleindex2) {
	targetmodule1 = plotdata1[plotdata1$module == sprintf("CellularNetwork.users[%d]",moduleindex1),]
	targetmodule2 = plotdata2[plotdata2$module == sprintf("CellularNetwork.users[%d]",moduleindex2),]

	plotdata <- rbind(targetmodule1, targetmodule2)
	#print(plotdata)

	plot_th <- ggplot(plotdata, aes(x=usertraffic, y=rbcount.mean, colour=interaction(module, scenario), group=interaction(module, scenario))) +
		geom_line() + scale_y_continuous(breaks=seq(0,25,1)) +
		geom_errorbar(aes(ymin=rbcount.confmin, ymax=rbcount.confmax, width=.1))

	plotdown(plot_th)
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

plotLorentzCurvePerRateRBcount <- function(plotdata, clientratemin, clientratemax, clientratestep) {
	cumulativedf <- data.frame();
	for(rate in seq(clientratemin, clientratemax, by=clientratestep))
	{
		targetrate <- plotdata[plotdata$usertraffic==rate,]
		lcdata <- Lc(targetrate$rbcount.mean)
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
		ggtitle(sprintf("Lorentz Curve Per Rate Resource Block counts (%s)", deparse(substitute(plotdata)))) +
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

	plotdown(resplot)
}

plotLorentzCurveRBcountComparision <- function(plotdata1, plotdata2, clientrate) {
	targetrate1 <- plotdata1[plotdata1$usertraffic==clientrate,]
	lcdata1 <- Lc(targetrate1$rbcount.mean)

	targetrate2 <- plotdata2[plotdata2$usertraffic==clientrate,]
	lcdata2 <- Lc(targetrate2$rbcount.mean)

	lcplotdata <- rbind(data.frame(lcdata1[1], lcdata1[2], dataset=rep(deparse(substitute(plotdata1)), length(lcdata1$p))),
						data.frame(lcdata2[1], lcdata2[2], dataset=rep(deparse(substitute(plotdata2)), length(lcdata2$p))))

	resplot <- ggplot(lcplotdata, aes(x=p, y=L, color=dataset, group=dataset)) +
		geom_abline() +
		geom_line() +
		ggtitle(sprintf("Lorentz Curve Throughput Comparision (Rate = %s)", clientrate)) +
		theme(plot.title = element_text(hjust = 0.5))

	plotdown(resplot)
}

plotThroughputEcdfComparision <- function(prepdata1, prepdata2, clientrate, moduleindex) {
	targetdata1 <- prepdata1[prepdata1$usertraffic==clientrate & prepdata1$module==sprintf("CellularNetwork.users[%d]",moduleindex),]
	targetdata2 <- prepdata2[prepdata2$usertraffic==clientrate & prepdata2$module==sprintf("CellularNetwork.users[%d]",moduleindex),]
	targetdata <- rbind(targetdata1, targetdata2)

	resplot <- ggplot(targetdata, aes(x=throughput)) +
				stat_ecdf(aes(group=scenario, color=scenario))

	plotdown(resplot)
}

plotBoxplotThroughputComparision <- function(prepdata1, prepdata2, clientrate, moduleindex) {
	targetdata1 <- prepdata1[prepdata1$usertraffic==clientrate & prepdata1$module==sprintf("CellularNetwork.users[%d]",moduleindex),]
	targetdata2 <- prepdata2[prepdata2$usertraffic==clientrate & prepdata2$module==sprintf("CellularNetwork.users[%d]",moduleindex),]
	targetdata <- rbind(targetdata1, targetdata2)

	resplot <- ggplot(targetdata, aes(x=usertraffic, y=throughput)) +
				geom_boxplot(aes(group=interaction(usertraffic,scenario), fill=scenario))

	plotdown(resplot)
}

plotSchedulerFrameFillRBcount <- function(plotdata) {
	resplot <- ggplot(plotdata, aes(x=usertraffic, y=framefilledrbcount.mean)) +
		geom_line() + scale_y_continuous(breaks=seq(0,25,1)) +
		geom_errorbar(aes(ymin=framefilledrbcount.confmin, ymax=framefilledrbcount.confmax, width=.1)) +
		geom_text(aes(label=ifelse(
						usertraffic==max(plotdata$usertraffic),
						plotdata[plotdata$usertraffic == max(plotdata$usertraffic), ]$framefilledrbcount.mean,
						'')), hjust=0.5, vjust=-0.5)

	multiplot(resplot)
}

plotThantenna <- function(scenariodatalist) {
	targetdata <- do.call("rbind", scenariodatalist)

	x_max <- max(targetdata$usertraffic)
	y_max <- round(max(targetdata$antennathroughput.mean)/(1), digits=0)

	resplot <- ggplot(targetdata, aes(x=usertraffic, y=antennathroughput.mean, colour=scenario, group=scenario)) +
		geom_line() + scale_y_continuous(breaks=seq(0,y_max,y_max/32)) + scale_x_continuous(breaks=seq(0,x_max,1)) +
		geom_errorbar(aes(ymin=antennathroughput.confmin, ymax=antennathroughput.confmax, width=.1))
		#+
		#geom_text(aes(label=ifelse(scenario=="UniformCQI" & antennathroughput.mean==max(antennaUniform$antennathroughput.mean),
		#	paste(round(max(antennaUniform$antennathroughput.mean)/(1), digits=0), 'bps'), '')), hjust=0.5, vjust=-0.5) +
		#geom_text(aes(label=ifelse(scenario=="UniformCQI_bestCQIScheduler" & antennathroughput.mean==max(antennaUniformBestCQI$antennathroughput.mean),
		#	paste(round(max(antennaUniformBestCQI$antennathroughput.mean)/(1), digits=0), 'bps'), '')), hjust=0.5, vjust=-0.5) +
		#geom_text(aes(label=ifelse(scenario=="BinomialCQI" & antennathroughput.mean==max(antennaBinomial$antennathroughput.mean),
		#	paste(round(max(antennaBinomial$antennathroughput.mean)/(1), digits=0), 'bps'), '')), hjust=0.5, vjust=-0.5) +
		#geom_text(aes(label=ifelse(scenario=="BinomialCQI_bestCQIScheduler" & antennathroughput.mean==max(antennaBinomialBestCQI$antennathroughput.mean),
		#	paste(round(max(antennaBinomialBestCQI$antennathroughput.mean)/(1), digits=0), 'bps'), '')), hjust=0.5, vjust=-0.5) +
		#geom_text(aes(label=ifelse(scenario=="NoFramingTest" & usertraffic==max(antennaNoFraming$usertraffic),
		#	paste(round(max(antennaNoFraming$antennathroughput.mean)/(1), digits=0), 'bps'), '')), hjust=0.5, vjust=-0.5)

	plotdown(resplot)
}

plotThantennaMax <- function(scenariodatalist) {
	antennadata <- do.call("rbind", scenariodatalist)

	targetdata <- aggregate(
		list(anthmean.max=antennadata$antennathroughput.mean,
			anthmean.confmin=antennadata$antennathroughput.confmin,
			anthmean.confmax=antennadata$antennathroughput.confmax),
		by = list(scenario=antennadata$scenario), max)

	resplot <- ggplot(targetdata, aes(x=scenario, y=anthmean.max, color=scenario, fill=scenario)) +
		geom_bar(stat="identity", width=.5) +
		geom_errorbar(aes(ymin=anthmean.confmin, ymax=anthmean.confmax), color="black", width=.2, position=position_dodge(.9)) +
		theme(legend.position="none") +
		geom_text(aes(label=round(anthmean.max)), position=position_dodge(width=0.9), vjust=-1)

	multiplot(resplot)
}

printRates <- function(plotdata)
{
	rates = sort(unique(plotdata$usertraffic));
	cat(paste(rates));
	cat("\n");
}

outputmode <- "window";
switchOutput <- function(mode)
{
	if(mode == "window")
		library(ggplot2)
	else if(mode == "png")
	{}
	else if(mode == "tikz")
	{
		library(tikzDevice)
		#options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
	}
	else if(mode == "plotly")
		library(plotly)
	else {
		cat("Invalid output mode!\n");
		return()
	}
	outputmode <<- mode;
}

startDevice <- function(cparams)
{
	filename <- paste(cparams, collapse = '-')

	if(outputmode == "window")
		X11(width=14, height=7)
	else if(outputmode == "tikz") {
		filename <- paste(filename, ".tex", collapse='', sep='')
		tikz(file = filename, sanitize=TRUE, width = 5, height = 5)
		cat(paste("exported to ", filename, "\n", collapse='', sep=''))
	}
	else if(outputmode == "png") {
		filename <- paste(filename, ".png", collapse='', sep='')
		png(filename, 4400, 2300, units = "px", res=400)
		cat(paste("exported to ", filename, "\n", collapse='', sep=''))
	}
	else if(outputmode == "plotly")
		cat("plotly is not yet supported.\n")
}

getPath <- function(filename) {
	return(paste(basedir,filename,sep=""))
}

# disable scientific notation
options(scipen = 999)

## REGRESSION TEST
preparedRegressionData <- prepareMeasures(getPath("data_regr.csv"))
regressionTestData <- aggregateClientMeasures(preparedRegressionData)

## USERS STATISTICS

# load all experiments data from CSVs
preparedValidation1Data <- prepareMeasures(getPath("data_validation_1.csv"))
preparedValidation2Data <- prepareMeasures(getPath("data_validation_2.csv"))
preparedNoFramingData <- prepareMeasures(getPath("data_noframing.csv"))
preparedUniformData <- prepareMeasures(getPath("data_uni.csv"))
preparedUniformBestCQIData <- prepareMeasures(getPath("data_uni_bestcqi.csv"))
preparedBinomialData <- prepareMeasures(getPath("data_binom.csv"))
preparedBinomialBestCQIData <- prepareMeasures(getPath("data_binom_bestcqi.csv"))

# compute confidence intervals and means for each user (and for each scenario)
validation1Data <- aggregateClientMeasures(preparedValidation1Data)
validation2Data <- aggregateClientMeasures(preparedValidation2Data)
noFramingData <- aggregateClientMeasures(preparedNoFramingData)
uniformData <- aggregateClientMeasures(preparedUniformData)
uniformBestCQIData <- aggregateClientMeasures(preparedUniformBestCQIData)
binomialData <- aggregateClientMeasures(preparedBinomialData)
binomialBestCQIData <- aggregateClientMeasures(preparedBinomialBestCQIData)

## SCHEDULER STATISTICS
schedulerPreparedValidation1Data <- prepareSchedulerMeasures(getPath("data_validation_1.csv"))
schedulerPreparedValidation2Data <- prepareSchedulerMeasures(getPath("data_validation_2.csv"))
schedulerPreparedNoFramingData <- prepareSchedulerMeasures(getPath("data_noframing.csv"))
schedulerPreparedUniformData <- prepareSchedulerMeasures(getPath("data_uni.csv"))
schedulerPreparedUniformBestCQIData <- prepareSchedulerMeasures(getPath("data_uni_bestcqi.csv"))
schedulerPreparedBinomialData <- prepareSchedulerMeasures(getPath("data_binom.csv"))
schedulerPreparedBinomialBestCQIData <- prepareSchedulerMeasures(getPath("data_binom_bestcqi.csv"))

schedulerValidation1Data <- aggregateSchedulerMeasures(schedulerPreparedValidation1Data)
schedulerValidation2Data <- aggregateSchedulerMeasures(schedulerPreparedValidation2Data)
schedulerNoFramingData <- aggregateSchedulerMeasures(schedulerPreparedNoFramingData)
schedulerUniformData <- aggregateSchedulerMeasures(schedulerPreparedUniformData)
schedulerUniformBestCQIData <- aggregateSchedulerMeasures(schedulerPreparedUniformBestCQIData)
schedulerBinomialData <- aggregateSchedulerMeasures(schedulerPreparedBinomialData)
schedulerBinomialBestCQIData <- aggregateSchedulerMeasures(schedulerPreparedBinomialBestCQIData)

## ANTENNA STATISTICS

antennaValidation1 <- aggregateAntennaMeasures(preparedValidation1Data)
antennaValidation2 <- aggregateAntennaMeasures(preparedValidation2Data)
antennaNoFraming <- aggregateAntennaMeasures(preparedNoFramingData)
antennaUniform <- aggregateAntennaMeasures(preparedUniformData)
antennaUniformBestCQI <- aggregateAntennaMeasures(preparedUniformBestCQIData)
antennaBinomial <- aggregateAntennaMeasures(preparedBinomialData)
antennaBinomialBestCQI <- aggregateAntennaMeasures(preparedBinomialBestCQIData)

antennaAll <- rbind(antennaValidation1, antennaValidation2, antennaNoFraming, antennaUniform, antennaUniformBestCQI, antennaBinomial, antennaBinomialBestCQI)


# scenario string parsing
parsescenario_prep <- list("regr" = preparedRegressionData,
							"nofram"  = preparedNoFramingData,
							"unif" = preparedUniformData,
							"unifbest" = preparedUniformBestCQIData,
							"binom" = preparedBinomialData,
							"binombest" = preparedBinomialBestCQIData,
							"val1" = preparedValidation1Data,
							"val2" = preparedValidation2Data)
parsescenario_data <- list("regr" = regressionTestData,
							"nofram" = noFramingData,
							"unif" = uniformData,
							"unifbest" = uniformBestCQIData,
							"binom" = binomialData,
							"binombest" = binomialBestCQIData,
							"val1" = validation1Data,
							"val2" = validation2Data)
parsescenario_scheddata <- list("regr" = preparedRegressionData,
							"nofram" = schedulerNoFramingData,
							"unif" = schedulerUniformData,
							"unifbest" = schedulerUniformBestCQIData,
							"binom" = schedulerBinomialData,
							"binombest" = schedulerBinomialBestCQIData,
						 	"val1" = schedulerValidation1Data,
							"val2" = schedulerValidation2Data)


cat("Plot commands:\n");
cat("\trates,\n");
cat("\tall, allzoom, allth, allrt, allrb, allrbbars, allpacketcount, alllittle\n");
cat("\tlorallth, lorallrt, lorallrb\n");
cat("\tlittleregr, thsat\n")
cat("\tth, rb, lorth, lorrb, ecdf, boxplot\n");
cat("\tfillrb\n");
cat("\tthantenna, thantennamax\n");
cat("\tthusrate\n")
cat("\tclose, exit\n");
cat("Valid scenarios:\n\t");
cat(paste(names(parsescenario_data), collapse = ' '));
cat("\n");

f <- file("stdin")
open(f)

while(1) {
	cat("$ ");
	cmdstr <- readLines(f,n=1,skipNul=TRUE);
	params <- unlist(strsplit(cmdstr, " "));

	switch(params[1],
		exit={
			quit();
		},
		close={
			dev.off();
		},
		mode={
			if(length(params) != 2)
				cat("mode usage: mode <modename>\n")
			else {
				switchOutput(params[2])
			}
		},
		ggsave={
			if(length(params) > 2)
				cat("mode usage: ggsave [<filename>]\n")
			else if(length(params) == 2) {
				ggsave(params[2], dpi=320)
			}
			else if(length(params) == 1){
				ggsave("plot.png", dpi=320)
			}
		},
		all={
			if(length(params) != 2)
				cat("all usage: all <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotAllModulesStatistics(data1)
				}
			}
		},
		allzoom={
			if(length(params) != 2)
				cat("allzoom usage: allzoom <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotAllModulesStatisticsZoom(data1)
				}
			}
		},
		allth={
			if(length(params) != 2)
				cat("allth usage: allth <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotAllModulesThroughput(data1)
				}
			}
		},
		allrt={
			if(length(params) != 2)
				cat("allrt usage: allrt <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotAllModulesResponseTimes(data1)
				}
			}
		},
		allrb={
			if(length(params) != 2)
				cat("allrb usage: allrb <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotAllModulesRBcounts(data1)
				}
			}
		},
		allrbbars={
			if(length(params) != 4)
				cat("allrbbars usage: allrbbars <scenario1> <scenario2> <clientrate>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]
				data2=parsescenario_data[[ params[3] ]]

				if(is.null(data1) || is.null(data2))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotAllModulesRBcountsByTrafficComparision(data1, data2, as.numeric(params[4]))
				}
			}
		},
		allpacketcount={
			if(length(params) != 2)
				cat("allpacketcount usage: allpacketcount <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotAllModulesPacketCounts(data1)
				}
			}
		},
		alllittle={
			if(length(params) != 2)
				cat("alllittle usage: alllittle <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotAllLittle(data1)
				}
			}
		},
		littleregr={
			if(length(params) != 3)
				cat("littleregr usage: littleregr <scenario> <clientindex>\n")
			else {
				data1=parsescenario_prep[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotLittleRegression(data1, as.numeric(params[3]))
				}
			}
		},
		thsat={
			if(length(params) != 3)
				cat("thsat usage: thsat <scenario> <clientindex>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotThroughputSaturationLines(data1, as.numeric(params[3]))
				}
			}
		},
		rates={
			if(length(params) != 2)
				cat("rates usage: rates <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]
				if(is.null(data1))
					cat("invalid scenario\n")
				else
					printRates(data1)
			}
		},
		th={
			if(length(params) != 5)
				cat("th usage: th <scenario1> <scenario2> <clientindex1> <clientindex2>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]
				data2=parsescenario_data[[ params[3] ]]

				if(is.null(data1) || is.null(data2))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotModuleComparision(data1, as.numeric(params[4]), data2, as.numeric(params[5]))
				}
			}
		},
		rb={
			if(length(params) != 5)
				cat("rb usage: rb <scenario1> <scenario2> <clientindex1> <clientindex2>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]
				data2=parsescenario_data[[ params[3] ]]

				if(is.null(data1) || is.null(data2))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotModuleRBComparision(data1, as.numeric(params[4]), data2, as.numeric(params[5]))
				}
			}
		},
		lorallth={
			if(length(params) != 2)
				cat("lorallth usage: lorallth <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotLorentzCurvePerRate(data1, 0.1, 8.1, 0.5)
				}
			}
		},
		lorallrt={
			if(length(params) != 2)
				cat("lorallrt usage: lorallrt <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotLorentzCurvePerRateResponseTimes(data1, 0.1, 8.1, 0.5)
				}
			}
		},
		lorallrb={
			if(length(params) != 2)
				cat("lorallrb usage: lorallrb <scenario>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotLorentzCurvePerRateRBcount(data1, 0.1, 8.1, 0.5)
				}
			}
		},
		lorth={
			if(length(params) != 4)
				cat("lorth usage: lorth <scenario1> <scenario2> <clientrate>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]
				data2=parsescenario_data[[ params[3] ]]

				if(is.null(data1) || is.null(data2))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotLorentzCurveComparision(data1, data2, as.numeric(params[4]))
				}
			}
		},
		lorrb={
			if(length(params) != 4)
				cat("lorrb usage: lorrb <scenario1> <scenario2> <clientrate>\n")
			else {
				data1=parsescenario_data[[ params[2] ]]
				data2=parsescenario_data[[ params[3] ]]

				if(is.null(data1) || is.null(data2))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotLorentzCurveRBcountComparision(data1, data2, as.numeric(params[4]))
				}
			}
		},
		ecdf={
			if(length(params) != 5)
				cat("ecdf usage: ecdf <scenario1> <scenario2> <clientrate> <clientindex>\n")
			else {
				prepdata1=parsescenario_prep[[ params[2] ]]
				prepdata2=parsescenario_prep[[ params[3] ]]

				if(is.null(prepdata1) || is.null(prepdata2))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotThroughputEcdfComparision(prepdata1, prepdata2, as.numeric(params[4]), as.numeric(params[5]));
				}
			}
		},
		boxplot={
			if(length(params) != 5)
				cat("boxplot usage: boxplot <scenario1> <scenario2> <clientrate> <clientindex>\n")
			else {
				prepdata1=parsescenario_prep[[ params[2] ]]
				prepdata2=parsescenario_prep[[ params[3] ]]

				if(is.null(prepdata1) || is.null(prepdata2))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotBoxplotThroughputComparision(prepdata1, prepdata2, as.numeric(params[4]), as.numeric(params[5]))
				}
			}
		},
		fillrb={
			if(length(params) != 2)
				cat("fillrb usage: fillrb <scenario>\n")
			else {
				data1=parsescenario_scheddata[[ params[2] ]]

				if(is.null(data1))
					cat("invalid scenario\n")
				else {
					startDevice(params)
					plotSchedulerFrameFillRBcount(data1)
				}
			}
		},
		thantenna={
			if(length(params) != 2)
			{
				cat("thantenna usage: thantenna <group>\n")
				cat("\tgroup can be: 0 (noframing), 1 (uniforms), 2 (binomials), 3 (validations)\n")
			}
			else {
				startDevice(params)

				if(params[2] == 0)
					plotThantenna(list(antennaNoFraming))
				else if(params[2] == 1)
					plotThantenna(list(antennaUniform, antennaUniformBestCQI))
				else if(params[2] == 2)
					plotThantenna(list(antennaBinomial, antennaBinomialBestCQI))
				else if(params[2] == 3)
					plotThantenna(list(antennaValidation1, antennaValidation2))
				else
					cat("invalid group!\n")
			}
		},
		thantennamax={
			if(length(params) != 2)
			{
				cat("thantennamax usage: thantennamax <group>\n")
				cat("\tgroup can be: 0 (all), 1 (noframing + uniforms), 2 (binomials), 3 (validations), 4 (uniforms + noframing + binomial)\n")
			}
			else {
				startDevice(params)

				if(params[2] == 0)
					plotThantennaMax(list(antennaValidation1, antennaValidation2, antennaUniform, antennaUniformBestCQI, antennaBinomial, antennaBinomialBestCQI))
				else if(params[2] == 1)
					plotThantennaMax(list(antennaNoFraming, antennaUniform, antennaUniformBestCQI))
				else if(params[2] == 2)
					plotThantennaMax(list(antennaBinomial, antennaBinomialBestCQI))
				else if(params[2] == 3)
					plotThantennaMax(list(antennaValidation1, antennaValidation2))
				else if(params[2] == 4)
					plotThantennaMax(list(antennaNoFraming, antennaUniform, antennaUniformBestCQI, antennaBinomial, antennaBinomialBestCQI))
				else
					cat("invalid group!\n")
			}
		},
		thusrate={
			if(length(params) != 4)
			{
				cat("thusrate usage: thusrate <scenario> <user> <rate>\n")
			} else {
				data1=parsescenario_data[[ params[2] ]]
				#cat(data1[data1$usertraffic == params[4] & data1$module == sprintf("CellularNetwork.users[%s]",params[3]),])
				th_confmin = data1[data1$usertraffic == as.numeric(params[4]) & data1$module==sprintf("CellularNetwork.users[%s]",params[3]),]$throughput.confmin
				th_confmax = data1[data1$usertraffic == as.numeric(params[4]) & data1$module==sprintf("CellularNetwork.users[%s]",params[3]),]$throughput.confmax
				th_mean = data1[data1$usertraffic == as.numeric(params[4]) & data1$module==sprintf("CellularNetwork.users[%s]",params[3]),]$throughput.mean
				cat("th_confmin", th_confmin)
				cat("\nth_mean: ",th_mean)
				cat("\nth_confmax", th_confmax,"\n")
			}
		},
		{
			cat("Not recognized command\n")
		}
	)
}