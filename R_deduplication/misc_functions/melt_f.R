melt_f=function(strat_a){ 
  
  # argument : a path to a stratum 
  # saves 2 data.table in long format:
  #      - one with fathers 
  #      - one with uncles
  # returns 0
  
  # NB : sex is imputed. Aunts are removed 
  
  # test
  # strat_a = strata[4]
  
  loc = fread(strat_a)
  
  loc[, side:="father"] #  necessary for dcast
  loc[, name_father:=name_f]    # necessary to find uncles that are indeed fathers
  loc[, dod_father:= dod_f]     # necessary to find uncles that are indeed fathers
  
  
  dates_vec = grep("^dob|^dod", names(loc), value = T) 
  
  # convert all dates to integer type, to save some memory
  #  and avoid warnings on type conversions
  loc[, c(dates_vec) := lapply(.SD, as.integer), .SDcols=dates_vec] 
  
  N_oncles_loc = length(grep("name_u", names(loc)))
  
  colA = c("dod_f", paste0("dod_u", 1:N_oncles_loc))
  
  colB = c("side", paste0("side_u", 1:N_oncles_loc))
  
  colC = c("name_f", paste0("name_u", 1:N_oncles_loc))
  
  loc_m = melt(loc, variable.name = "ancestor",
               measure = list(colA, colB, colC),
               value.name = c("dod_ancestor", "type_ancestor", "name_ancestor"))
  
  if( loc_m[type_ancestor=="father", .N] !=   loc[, .N] ){stop("Error during melting")}
  
  # check
  # loc_m[,table(type_ancestor, useNA = "always")]
  
  # change values of type_ancestor to avoid confusions with sex
  # see below for the removal of aunts and of ancestors of unknown sex
  loc_m[type_ancestor=="m", type_ancestor:="maternal_u"]
  loc_m[type_ancestor=="p", type_ancestor:="paternal_u"]
  
  
  # removal of lines type_ancestor=="" (empty uncle columns in the original dataset)
  loc_m <- loc_m[type_ancestor!=""]
  
  # creation of independent year, month and day variables for each date
  dates_v = grep("dob|dod", names(loc_m), value = T)
  dates_v = dates_v[dates_v!="dob_y"] # yob has already been created to define strata
  
  loc_m[, c(paste0(dates_v, "_y")) := lapply(.SD, substr_mod_f, 1, 4), .SDcols= dates_v ]
  loc_m[, c(paste0(dates_v, "_m")) := lapply(.SD, substr_mod_f, 5, 6), .SDcols= dates_v ]
  loc_m[, c(paste0(dates_v, "_d")) := lapply(.SD, substr_mod_f, 7, 8), .SDcols= dates_v ]
  
  
  # curation of days : set to NA if not in 1:31
  loc_m[, c(paste0(dates_v, "_d")) := lapply(.SD, correct_date_f, type_a="d"), 
        .SDcols=paste0(dates_v, "_d")]
  # curation of months
  loc_m[, c(paste0(dates_v, "_m")) := lapply(.SD, correct_date_f, type_a="m"), 
        .SDcols=paste0(dates_v, "_m")]
  # curation of years  
  loc_m[, `:=`(dod_y  = correct_date_f(dod_y,  type_a="y", years_a=1905:2017),
               dob_f_y= correct_date_f(dob_f_y, type_a="y", years_a=1830:1915))]
  
  loc_m[type_ancestor=="father", 
        `:=`(dod_ancestor_y=correct_date_f(dod_ancestor_y, type_a="y", years_a=1904:2017))]
  
  # absurd paternal dates of birth and death set to NA 
  loc_m[dob_y < (dob_f_y+12), 
        `:=`(dob_f_d=NA, dob_f_m=NA, dob_f_y=NA)]
  loc_m[type_ancestor=="father" & (1+dod_ancestor_y)<dob_y, 
        `:=`(dod_ancestor_d=NA, dod_ancestor_m=NA, dod_ancestor_y=NA)]

  
  ### imputation of the ancestor's sex

  # add separate names
  names_for_identification_f(data_a = loc_m, names_a = "name_ancestor", soundex_a = F)

  # check
  # loc_m[, grep("^name", names(loc_m), value = T), with=F]
                               
  setnames(fn_insee, old = "preusuel", new = "name_ancestor_c1")
  loc_m[fn_insee, on=.(name_ancestor_c1), proba_m1:=proba_m]
  setnames(fn_insee, old = "name_ancestor_c1", new = "name_ancestor_c2")
  loc_m[fn_insee, on=.(name_ancestor_c2), proba_m2:=proba_m]
  # back to initial column name
  setnames(fn_insee, old = "name_ancestor_c2", new = "preusuel")
   

  # loc_m[abs(proba_m1-0.5)<0.4,
  #       c(grep("^name_ancestor", names(loc_m), value = T), "proba_m1", "proba_m2"), with=F]
 
  
  loc_m[,sex_ancestor:= "unknown"]
  
  loc_m[grep("garcon", name_ancestor), sex_ancestor:= "m"]
  loc_m[grep("fille", name_ancestor), sex_ancestor:= "f"]
  
  # if the two scores are concordant, no problem
  loc_m[proba_m1>0.90 & proba_m2>0.35, sex_ancestor:= "m"]
  loc_m[proba_m2>0.90 & proba_m1>0.35, sex_ancestor:= "m"]
  loc_m[proba_m1<0.10 & proba_m2<0.65, sex_ancestor:= "f"]
  loc_m[proba_m2<0.10 & proba_m1<0.65, sex_ancestor:= "f"]

  # if only one score if available, we follow it
  loc_m[is.na(proba_m2) & proba_m1>0.5, sex_ancestor:="m"]
  loc_m[is.na(proba_m1) & proba_m2>0.5, sex_ancestor:="m"]
  loc_m[is.na(proba_m2) & proba_m1<0.5, sex_ancestor:="f"]
  loc_m[is.na(proba_m1) & proba_m2<0.5, sex_ancestor:="f"]
  
  # special case : Marie
  loc_m[(name_ancestor_c1=="marie" | name_ancestor_c2=="marie") &
        (proba_m1>0.70 | proba_m2>0.70),
        sex_ancestor:= "m"]
  # special case : Camille
  loc_m[(name_ancestor_c1=="camille" | name_ancestor_c2=="camille") &
          (proba_m1>0.70 | proba_m2>0.70),
        sex_ancestor:= "m"]
  
  # we make sure the father will be considered a male...
  loc_m[type_ancestor=="father", sex_ancestor:= "m"]
  
  # checks
  loc_m[name_ancestor!="", summary(as.factor(sex_ancestor))]
  loc_m[name_ancestor!="" &  sex_ancestor=="unknown", .(name_ancestor, proba_m1, proba_m2)]

  # removal of aunts and of ancestor of unknown sex
  loc_m = loc_m[sex_ancestor=="m"]
  
  # removal of unnecessary variables
  loc_m[, `:=`(proba_m1=NULL, 
               proba_m2=NULL)]
  
  data_father <- loc_m[type_ancestor=="father"]
  data_father[, `:=`(name_father=NULL,
                     dod_father=NULL)] # unnecessary in fathers' dataset

  # save data
  
  # fathers
  fwrite(data_father, 
         paste0("../data/stratified_melted/peres/", loc_m[, unique(stratum)], ".csv")
  )
  
  # uncles
  fwrite(loc_m[type_ancestor!="father"], 
         paste0("../data/stratified_melted/oncles/", loc_m[, unique(stratum)], ".csv")
  ) 
  
  return(0)
} # melt_f

