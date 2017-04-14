# Returns the upper and lower quantiles for a given confidence level
getCI.q<- function(conf.level = 0.95) { c(CI.lo = (1-conf.level)/2, CI.hi = 1-((1-conf.level)/2)) }

# Calculates confidence intervals around the mean based on the normal (z) or Student's t distribution
#' x = rnorm(100)
#' ci(x)
#' ci(x, use='t')
ci<- function(x, conf.level=0.95, use='z', df=NULL) {
	if(use == 't') {
		if(is.null(df)) df = len(x)-1
		x.ci = avg(x)+(qt(p=getCI.q(conf.level), df=df)*se(x))
	} else {
		x.ci = avg(x)+(qnorm(p=getCI.q(conf.level))*se(x))
	}
	return(x.ci)
}

# Returns just the lower or upper CI
ci.lo<- function(x, ...) { ci(x, ...)[1] }
ci.hi<- function(x, ...) { ci(x, ...)[2] }

# Returns a vector of the mean +/- the number of standard deviations (default: 1)
mean_sd<- function(x, mult = 1) { avg(x) + sd_na(x)*(mult*c(-1,1)) }

# Returns a vector of the median +/- mult.q quantiles (default: 25, 50, 75)
med_q<- function(x, mult.q = 0.25) {  quantile(x, probs=c(0.5 - mult.q, 0.5, 0.5 + mult.q))  }

sd_range<- function(x, mult = 2, by = mult/4) {  seq(-sd_na(x)*mult, sd_na(x)*mult, by=sd_na(x)*by)  }

# Given a confidence interval around an estimate, calculates the corresponding p value
ci_to_p<- function(Estimate, CIs, conf.level = 0.95) {
	SE = abs(diff(as.numeric(CIs)))/(qnorm(getCI.q(conf.level)[2])*2)
	z.value = Estimate/SE
	p.value = pnorm(z.value, lower.tail=FALSE)

	return(c(z.value = z.value, p.value = p.value))
}

