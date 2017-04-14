
# Calculates the nth root of x (adapted from function posted by 'macrakis')
nroot<- function(x, n = 2) if_else(n %% 2 == 1 | x >= 0, sign(x)*(abs(x)^(1/n)), NaN)


back.transform<- function(x, type = 'log') {
	if(!type %in% c('log', 'sqrt')) stop('Invalid type.')
	if(type == 'log') return(exp(x))
	if(type == 'sqrt') return(x^2)
}




# Reload a package
reload<- function(pkg) {
	detach(paste0('package:', pkg), character.only=TRUE)
	library(pkg)
}
