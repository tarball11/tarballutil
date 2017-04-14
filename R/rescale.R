
# Converts a vector to z-scores (returns a vector without additional attributes)
z.score<- function(x) {  as.numeric(base::scale(x, center=TRUE, scale=TRUE))  }

# Centers a vector on its mean (returns a vector without additional attributes)
center<- function(x) {  as.numeric(base::scale(x, center=TRUE, scale=FALSE))  }

# Creates centered (but not scaled) versions of the named columns (or all numeric columns) for the given tibble
# Newly created centered variables will have '_c' appended to the original variable name
center_tbl<- function(x.tbl, x.cols = NULL) {

	if(is.null(x.cols)) {
		x.tbl = x.tbl %>% dplyr::mutate_if(is.numeric, funs(c = center))
	} else {
		if(!all(x.cols %in% names(x.tbl))) stop('Invalid column name provided.')
		x.tbl = x.tbl %>% dplyr::mutate_at(one_of(x.cols), funs(c = center))
	}
	return(x.tbl)
}

# Reverses the actions of scale(), if attributes are available for x
# Optionally, take center and scale values from another variable, or specify directly
unscale<- function(x, y = NULL, y.center = NULL, y.scale = NULL) {
	x.center = dplyr::coalesce(attr(y, 'scaled:center'), y.center, attr(x, 'scaled:center'))
	x.scale = dplyr::coalesce(attr(y, 'scaled:scale'), y.scale, attr(x, 'scaled:scale'))
	if(is.null(x.center)) x.center = 0
	if(is.null(x.scale)) x.scale = 1
	return((x*x.scale)+x.center)
}
