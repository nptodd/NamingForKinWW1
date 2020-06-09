
identify_pairs_f  = function(strat_a){
  
  # test
  # strat_a = strata_fathers[1]
  
  loc = fread(strat_a)
  
  # frequent "le xxx" or "xxx le" cases in Brittany : all become "lexxx"
  loc[grepl("(^le )|( le$)", family_name) & 
        stringr::str_count(family_name, " ")==1 & 
        dep2 %in% paste0("F", c(22, 29, 35, 44, 56)), 
      family_name:=paste0("le", gsub("(^le )|( le$)", "", family_name)) ]
  
  
  # creation of soundex versions of names' components
  names_for_identification_f(loc, "name", soundex_a = T)
  names_for_identification_f(loc, "name_ancestor", soundex_a = T)
  names_for_identification_f(loc, "family_name", soundex_a = T, type_a="familyname")
  
  # check
  # View(loc[1:100, grep("name", names(loc)), with=F])
  
  # identification of long family names : at least one space in the family name 
  loc[, long_family_name := unique_id]
  loc[stringr::str_count(family_name, " ")>0, long_family_name:="LONG"]
  
  
  # check 
  # View(loc[long_name=="LONG", .(family_name)])
  
  # imputed longitudes and latitudes shouldn't be included in the deduplication process
  # NA will give a 0 in the pattern
  loc[cas_geoloc %in% c("cas2b", "cas3"), `:=`(lat2=NA, lon2=NA)]
  
  # check
  # loc[, .(name, name_sdx_c1, name_sdx_c2, name_sdx_c3)]
  # soundexFR("andre")
  
  # variables used in deduplication
  # nb : by definition, same stratum sets same sex and year of birth 
  # as blocking fields
  var_dedup <- c("family_name_sdx_c1", "long_family_name",
                 "name_sdx_c1", "name_sdx_c2", # "name_sdx_c3", # soundex of names 
                 "name_ancestor_sdx_c1", "name_ancestor_sdx_c2", # "name_ancestor_sdx_c3", # soundex of names of the father
                 "dep2", "lat2", "lon2", # geographical adta
                 "dob_m", "dob_d",  # date of birth
                 "dod_y", "dod_m", "dod_d",  # date of death
                 "dob_f_y", "dod_ancestor_y" # yob and yod of the father
  )
  
  
  
  pairs <- RLBigDataDedup(
    loc, 
    blockfld = 
      list(c("dep2", "family_name_sdx_c1"), 
           c(        "family_name_sdx_c1", "name_sdx_c1", "dob_m", "dob_d"), # handle errors on departement
           c("dep2", "long_family_name",   "name_sdx_c1", "dob_m", "dob_d"), # handle long names
           c(        "long_family_name",   "name_sdx_c1", "dob_m", "dob_d", "name_ancestor_sdx_c1") # long names + error on departement
      ), 
    exclude = names(loc)[! names(loc) %in% var_dedup])
  # summary(pairs)
  
  pairs <- emWeights(pairs)
  
  
  # Save results
  path_to_results  = paste0("../data/pairs/", gsub(".*/", "", strat_a), ".RData")
  path_to_results = gsub(".csv", "", path_to_results, fixed = T)
  saveRLObject(pairs, path_to_results)
  
  return(0)
}

