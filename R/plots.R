
#Creates additional spacing around levels in the legend
legend.f<- function(f) {
	if(!is.factor(f)) f = as.factor(f)
	f.n = nlevels(f)
	levels(f)[1:(f.n-1)] = paste(levels(f)[1:(f.n-1)], "   ")

	return(f)
}


# Calculates measures of central tendency (Mean, Median, and Mode) and plots a histogram
genHist<- function(x, binwidth=NULL, x.breaks = NULL, y.breaks = NULL) {
	x.name = deparse(substitute(x))
	x.tbl = tibble(x = as.numeric(x[!is.na(x)]))
	x.c = x.tbl %>% summarize(Mean = avg(x), Median = med(x), Mode = mde(x))
	x.lines = x.c %>% gather(StatName, StatVal)

	if(is.null(x.breaks)) {
		x.range = range(x, na.rm=TRUE)
		x.breaks = seq(floor(x.range[1]), ceiling(x.range[2]), length.out=20)
	}

	#if(is.null(binwidth)) binwidth = sd_na(x)/20
	if(is.null(binwidth)) binwidth = diff(x.breaks[1:2])

	x.p =	ggplot(data=x.tbl, aes(x=x)) +
				geom_histogram(binwidth=binwidth, fill='white', color='black') +
				scale_x_continuous(breaks=x.breaks) + labs(x = x.name, y = 'Count') +
				geom_vline(data=x.lines, aes(xintercept=StatVal, color=StatName), size=1, show.legend=TRUE) +
				scale_color_brewer(palette='Dark2')
	if(!is.null(y.breaks)) x.p = x.p + scale_y_continuous(breaks=y.breaks)
	print(x.p)
	print(x.c)
	invisible(x.c)
}


