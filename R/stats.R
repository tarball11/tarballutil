
# Calculates average excluding NAs
avg<- function(x, ...) {  base::mean(x, na.rm=TRUE, ...)  }

# Calculates length excluding NAs
len<- function(x) {  sum(!is.na(x))	 }

# Calculates standard deviation (excluding NAs)
sd_na<- function(x) { sd(x, na.rm=TRUE) }

#' Cumulative standard deviation
cumsd<- function(x) {
	x.sd = rep(NA, length(x))
	for(i in 2:length(x)) x.sd[i] = sd(x[1:i], na.rm=TRUE)
	return(x.sd)
}

# Calculates standard error (excluding NAs)
se<- function(x) {  sd_na(x)/sqrt(len(x))  }

# Calculates Median excluding NAs
med<- function(x) {  median(x, na.rm=TRUE)  }

# Calculates Mode (from various online sources)
mde<- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Skewness statistic
skew<- function(x) {  mean(center(x)^3/var(x, na.rm=TRUE)^(3/2), na.rm=TRUE)	}

# Standard error of the skewness statistic
skew.se<- function(x, n=len(x)) sqrt((6*n*(n-1)) / ((n-2)*(n+1)*(n+3)))
# Skewness test:
skew.test<- function(x, digits=6, print=TRUE) {
	z.val = skew(x)/skew.se(x)
	p.val = pnorm(abs(z.val), lower.tail=FALSE)*2

	return.val = c(Z = z.val, p = p.val)
	if(!is.null(digits)) return.val = round(return.val, digits)

	if(print) {
		eq = ifelse(p.val < .001, '', ' = ')
		cat('z = ', format.t(z.val), ', p ', eq, format.p(p.val), '\n', sep='')
		invisible(return.val)
	} else {
		return(return.val)
	}
}
