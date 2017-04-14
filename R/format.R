#' Format p values
#'
#' Formats a vector of p values with sensible (APA style) defaults
#'
#' @param p A numeric vector of p values.
#' @param eps Numeric value (epsilon) below which precision does not matter (e.g. for eps = .001, report p < .001 instead of p = .0000042)
#' @param digits Number of digits to report precise p values.
#' @param print.eq Boolean; always print the equals sign (TRUE), or just for values < \code{eps} (FALSE, the default)?
#' @return A vector of formatted p values (character vector)
#' @export
#' @examples
#' p.values = c(0.304563453, 0.04150, 0.02, 0.001, 0.0009)
#' format.p(p.values, eps = .001, digits = 3, print.eq=FALSE)
#' format.p(p.values, eps = .05, digits = 3, print.eq=FALSE)
#' format.p(p.values, eps = .001, digits = 4, print.eq=FALSE)
#' format.p(p.values, eps = .001, digits = 3, print.eq=TRUE)
format.p<- function(p, eps = .001, digits=3, print.eq = FALSE) {
	if(!is.numeric(p)) stop('p must be a numeric vector.')
	if(!is.numeric(eps) | eps > 1) stop('Invalid value for eps.')
	if(!is.numeric(digits) | digits < 0) stop('digits must be numeric and > 0')
	if(!is.logical(print.eq)) stop('Invalid value for print.eq')

	# Round to [3] digits
	p.fmt = ifelse(!is.na(p), sprintf(paste0('%.', digits, 'f'), p), p)
	# Drop leading zero
	p.fmt = gsub('0\\.', '.', p.fmt)

	# Change formatted values to < [eps] (e.g. < .001) using original (pre-rounding) p-values
	p.fmt[which(as.numeric(p) < eps)] = paste('<', gsub('0\\.', '.', eps))

	# Add equals sign if requested
	if(print.eq) p.fmt[which(as.numeric(p) >= eps)] = paste('=', p.fmt[which(as.numeric(p) >= eps)])

	return(p.fmt)
}

#' Format test statistics (e.g. t values)
#'
#' Formats a vector of test statistics (e.g. t values) with sensible (APA style) defaults
#'
#' @param x A numeric vector of test statisics
#' @param digits Number of digits after the decimal to report (default: 2)
#' @param perc Boolean; add a percentage sign after \code{x}
#' @return A vector of formatted test statistics (character vector)
#' @details Intended to format test statistics for output, but can be used to format any vector of numeric values.
#' @export
#' @examples
#' t.values = c(1.2165468, 0.0001, -1.5)
#' format.t(t.values)
#' format.t(t.values, digits=4)
#' format.t(95, digits=0, perc=TRUE)
format.t<- function(x, digits=2, perc=FALSE) {
	if(all(is.na(x))) return(x)
	if(!is.numeric(x)) stop('x must be a numeric vector.')
	if(!is.numeric(digits) | digits < 0) stop('digits must be numeric and > 0')
	if(!is.logical(perc)) stop('Invalid value for perc')

	fmt = ifelse(perc, paste0('%1.', digits, 'f%%'), paste0('%1.', digits, 'f'))
	x.fmt = ifelse(!is.na(x), sprintf(fmt, x), x)
	return(x.fmt)
}

#' Format confidence intervals
#'
#' Formats confidence intervals in APA style
#'
#' @param x A numeric vector of the lower and upper bounds of a confidence interval
#' @param digits Number of digits after the decimal to report (default: 2)
#' @param print.sign Boolean; always print the sign (TRUE), or just for negative values (FALSE, the default)?
#' @return A string with the formatted CI (e.g. "[CI.lo, CI.hi]")
#' @export
#' @examples
#' format.ci(c(1.121234124, 1.44000000))
#' format.ci(c(-1.121234124, 1.4), digits=3)
#' format.ci(c(-1.121234124, 1.4), print.sign=TRUE)
format.ci<- function(x, digits=2, print.sign = FALSE) {
	if(!is.numeric(x)) stop('x must be a numeric vector.')
	if(length(x) != 2) stop('x must be of length 2')
	if(!is.numeric(digits) | digits < 0) stop('digits must be numeric and > 0')
	if(!is.logical(print.sign)) stop('Invalid value for print.sign')

	fmt = ifelse(print.sign, paste0('%+1.', digits, 'f'), paste0('%1.', digits, 'f'))
	x.fmt = paste0('[', paste(sprintf(fmt, x), collapse=', '), ']')
	return(x.fmt)
}

#' Format t statement
print_t<- function(t.value, DF, p.value) {
	if(!all(lengths(list(t.value, DF, p.value)) == length(t.value))) stop('All vectors must be the same length.')
	# Are DFs given as integers (even if they're stored as doubles)?
	DF.f = ifelse(abs(DF - round(DF)) < .Machine$double.eps^0.5, '%i', '%1.2f')

	fmt = paste0('t(', DF.f, ') = %1.2f, p ', format.p(p.value, print.eq=TRUE))
	t.stmt = sprintf(fmt, DF, t.value)
	return(t.stmt)
}

