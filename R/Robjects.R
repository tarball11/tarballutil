# Saves the objects (given as ...) as an R object, in the form of a list (lst)
saveRObj.lst<- function(filename = 'Saved_Robjs/RObj.lst', ...) {
	RObj.lst = lst(...)
	write_rds(RObj.lst, paste0(filename, '.gz'), 'gz')
	return(RObj.lst)
}

# Reads in a list saved as an R object and populates the global environment with the list's named variables
getRObj.lst<- function(filename = 'Saved_Robjs/RObj.lst.gz', print.info = TRUE) {
	RObj.lst = read_rds(filename)
	lstToGlobalEnv(Var.lst = RObj.lst, Name.lst = names(RObj.lst), print.info = print.info)
	#for(i in names(RObj.lst)) assign(i, RObj.lst[[i]], envir = .GlobalEnv)
	return(RObj.lst)
}

# Extract a list of variables into the global environment
lstToGlobalEnv<- function(Var.lst, Name.lst = NULL, clobber = TRUE, print.info = TRUE) {
	#Var.lst = Obs_True.lsmeans
	# Use the names of Var.lst if no names are provided.
	if(is.null(Name.lst)) Name.lst = names(Var.lst)
	if(is.null(Name.lst)) stop('Invalid list of variable names provided.')

	if(is.null(names(Name.lst))) names(Name.lst) = names(Var.lst)
	if(!setequal(names(Var.lst), names(Name.lst))) stop('Invalid list of variable names provided.')

	if(print.info) cat('Adding variables to global environment:\n')
	p.str = c()
	for(i in 1:length(Name.lst)) {
		lstVar.name = names(Name.lst)[i]
		newVar.name = Name.lst[i]
		newVar.val = Var.lst[[lstVar.name]]
		if(!clobber & exists(newVar.name, envir=.GlobalEnv)) {
			warn(paste(newVar.name, 'already exists in the global environment; skipping.\n'))
		} else {
			assign(newVar.name, newVar.val, envir = .GlobalEnv)
		}
		p.str = c(p.str, paste(newVar.name, '-->', lstVar.name))
	}

	if(print.info) cat('\t', paste(p.str, collapse=' | '), '\n', sep='')
}
