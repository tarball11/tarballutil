# Search and replace with vectors of patterns and replacements
mult_gsub<- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
	if(length(pattern) != length(replacement)) stop('Pattern and replacement vectors not the same length -- no substitution performed.')

	for(i in 1:length(pattern)) x = gsub(pattern[i], replacement[i], x, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)

	return(x)
}

# Converts NA values to na.str (defaults to empty string)
# Note: Only meant for character vectors; will convert numeric vectors to character!
na_to_str<- function(x, na.str = '') { ifelse(is.na(x), na.str, x) }
