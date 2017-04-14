
# Convert factor variables in a tibble to character vectors
unfactor_tbl<- function(Data.tbl, f.list = NULL) {
	if(!is_tibble(Data.tbl)) Data.tbl = as_tibble(Data.tbl)
	if(is.null(f.list)) f.list = names(select_if(Data.tbl, is.factor))
	for(f in f.list) {
		Data.tbl[[f]] = parse_character(Data.tbl[[f]])
	}
	return(Data.tbl)
}

# Sets the order of factor levels for given variables in a tibble
relevel_tbl<- function(Mod.tbl, Mod.lvls) {
	require(forcats)
	for(f in intersect(names(Mod.tbl), names(Mod.lvls))) {
		if(!is.factor(Mod.tbl[[f]])) {
			Mod.tbl[[f]] = factor(Mod.tbl[[f]], levels=Mod.lvls[[f]])
		} else {
			Mod.tbl[[f]] = fct_relevel(Mod.tbl[[f]], Mod.lvls[[f]])
		}
	}

	return(Mod.tbl)
}

