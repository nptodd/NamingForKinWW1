identify_join_pairs_uncles_f= function(strat_a, CUTOFF_a){
  
  # strat_a <- strata_uncles[1]  # test
  
  strat_name <- gsub(".*/", "", strat_a)
  strat_name <- gsub(".csv", "", strat_name, fixed = T)
  
  loc_uncles <- fread(strat_a)
  
  loc_fathers <- fread(gsub(strat_a, 
                            pattern = "/oncles/", 
                            replacement = "/peres/", 
                            fixed = T) )
  
  # load id geneanet - id unique indiv. table
  loc_table <- fread(paste0("../data/table_deduplication/table_", strat_name, ".csv" ))
  
  # uncles who are fathers are removed. Sheer duplicates of the Geneanet database
  loc_uncles <- loc_uncles[! (name_father==name_ancestor & 
                                dod_father==dod_ancestor & 
                                type_ancestor=="paternal_u")]
  
  
  loc_uncles[loc_table, on=.(unique_id), merged_id:=merged_id]
  loc_fathers[loc_table, on=.(unique_id), merged_id:=merged_id]
  
  # removal of blatantly identical uncles
  loc_uncles <- unique(x = loc_uncles, 
                       by = c("unique_id", "merged_id", "name_ancestor",
                              "dod_ancestor",  "type_ancestor"))
  
  # creation of soundex versions of uncles' names
  names_for_identification_f(loc_uncles, "name_ancestor", soundex_a = T)
  
  loc_uncles[,unique_id_uncle:=paste0(merged_id, "_", substr(type_ancestor, 1,1),
                                      "_", name_ancestor_sdx_c1)]
  
  
  selected_col <- c("dod_ancestor_y", "dod_ancestor_m", "dod_ancestor_d",
                    "name_ancestor_sdx_c2", 
                    "name_ancestor_c1", "name_ancestor_c2", "name_ancestor_c3")
  
  res <- loc_uncles[, lapply(.SD, getmode_f), 
                    .SDcols=selected_col,
                    by=.(unique_id_uncle, merged_id,
                         type_ancestor, name_ancestor_sdx_c1)]

  return(res)
}
