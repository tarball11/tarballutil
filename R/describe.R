# Standardizes names associated with standard summaries
fix_tbl_names<- function(Mod.tbl) {
	new.names = old.names = names(Mod.tbl)
	names.lst = list('Estimate' = c('estimate', 'fit', 'lsmean'),
									 'SE' = c('Std. Error', 'std.error'),
									 'DF' = c('Df', 'df'),
									 'SumSq' = c('Sum Sq', 'sumsq'),
									 'MeanSq' = c('Mean Sq', 'meansq'),
									 'F.value' = c('F value'),
									 't.value' = c('t value', 't.ratio'),
									 'z.value' = c('z value'),
									 'p.value' = c('Pr(>F)', 'Pr(>|t|)', 'Pr(>z)'),
									 'CI.lo' = c('lower.CL', 'lwr', 'conf.low'),
									 'CI.hi' = c('upper.CL', 'upr', 'conf.high'))

	for(new in names(names.lst)) new.names[which(old.names %in% names.lst[[new]])] = new
	names(Mod.tbl) = new.names
	return(Mod.tbl)
}

add_SE_cols<- function(.) dplyr::mutate(., SE.lo = Estimate - SE, SE.hi = Estimate + SE)

# Returns a summary of the variables
describe_tbl<- function(Data.tbl, ..., key = 'key', conf.level = 0.95) {

	grp.vars = dplyr::groups(Data.tbl) %>% as.character()
	key.vars = pryr::dots(...)
	key_group.f = lazyeval::interp(~x, x=as.name(key)) %>% list() %>% setNames(key)
	key_mutate.f = lazyeval::interp(~factor(x, levels=key.vars), x=as.name(key)) %>% list() %>% setNames(key)
	key_select.f = list(lazyeval::interp(~dplyr::one_of(x), x = key), ~everything())

	Data.tbl =  dplyr::select(Data.tbl, one_of(grp.vars), ...) %>%
		tidyr::gather_(key_col = key, value_col = 'value', gather_cols=key.vars, na.rm=TRUE) %>%
		dplyr::group_by_(.dots=key_group.f, add=TRUE) %>%
		dplyr::summarize_at(vars(value),
												funs(N=sum(!is.na(.)),
														 Avg=mean(., na.rm=TRUE),
														 SD=sd(., na.rm=TRUE),
                             							 SE=SD/sqrt(N),
														 CI.lo=ci.lo(., conf.level),
														 CI.hi=ci.hi(., conf.level))) %>%
		dplyr::mutate_(.dots=key_mutate.f) %>%
		dplyr::select_(.dots=key_select.f) %>%
		dplyr::arrange_(c(key, grp.vars))
	return(Data.tbl)
}

