# !diagnostics off

# Geneanet dataset : all individuals born 1905-25 in France. Variables:
#       - names, sex, commune, département of birth
#       - date of birth, date of death, name of the father
#       - for uncles/aunts : date of death, name and maternal/paternal side

load_data_f = function(type_a = "full", 
                       n_sample=5e4, 
                       rndm_sample_a=F, 
                       verbose_a=F, 
                       years_sample_a=1905:1925){
  
  if (! type_a %in% c("full", "sample")){stop("Argument type_a incorrect")}
  
  if(file.exists("../exclusions.log")){ file.remove("../exclusions.log")  }

  # read to a tibble. Tried with fread, but stumbled across the variable number of columns
  if(type_a=="full"){
    
    dtb = readr::read_csv2("../data/export-1418.csv", 
                           locale=readr::locale(encoding="latin1"),
                           col_names=paste0("X",1:72))
    
  } else if (type_a=="sample" & rndm_sample_a==F){
    
    dtb = readr::read_csv2("../data/export-1418.csv", 
                           locale=readr::locale(encoding="latin1"),
                           col_names=paste0("X",1:72), n_max=n_sample)
    
  } else if (type_a=="sample" & rndm_sample_a==T){
    
    dtb = readr::read_csv2("../data/export-1418.csv", 
                           locale=readr::locale(encoding="latin1"),
                           col_names=paste0("X",1:72))
    
    set.seed(55)
    SAMP <- sample(nrow(dtb), size=n_sample)
    
    dtb <-  dtb[SAMP,]
  }
  
  setDT(dtb)
  
  ######################################################################################
  ##################################  Columns' names  ##################################
  
  # names of first variables (infants and fathers) :
  # name : name of the infant
  # dob : date of birth of the infant
  # dod : date of death of the infant
  # name_f : name of the father
  # dob_f : date of birth of the father
  # dod_f : date of death of the father
  
  col_names1 = c("idtree", "family_name",
                 "name", "sex", "dob", "dod",
                 "place", "dep", "region", "country", "lat", "lon",
                 "name_f", "dob_f", "dod_f")
  setnames(dtb, 
           old = paste0("X",1:length(col_names1)), 
           new = col_names1)
  
  # removal of lines lignes idtree == "sourcename" (including the 1st one)
  dtb = dtb[idtree != "sourcename"]
  
  # variables pertaining to uncles
  N_uncles = (ncol(dtb)-length(col_names1))/3
  col_names2 = expand.grid(var=c("side", "name", "dod"), suffixe="_u", numero = 1:N_uncles)
  col_names2 = paste0(col_names2$var, 
                      col_names2$suffixe, 
                      col_names2$numero)
  setnames(dtb, 
           old = paste0("X",(length(col_names1)+1):ncol(dtb)), 
           new = col_names2)
  
  # removal of country (useless)
  dtb[,country:=NULL]
  
  # creation of unique identifier
  dtb[, unique_id:= paste0("i", 1:dtb[,.N]) ]
  
  setcolorder(dtb, c("idtree", "unique_id"))
  
  
  ######################################################################################
  ######################################## Dates #######################################
  
  # Available dates : 
  # - date of birth and date of death, 
  # - date of birth and date of death of the father, 
  # - date of death of the uncles
  
  dates_v = c("dob", "dob_f", grep("^dod", names(dtb),  value = T))
  
  if(verbose_a){
    cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\nBefore correction :\n")
    dtb[, lapply(.SD,  nchar_f), .SDcols = dates_v]
  }
  
  # Rares exceptions left aside, available dates have either 1 or 8 figures
  # If 1, the date is evidently unknown. We thus set to NA dates with != 8 characters 
  
  dtb[, c(dates_v) := lapply(.SD, 
                             function(x_a){ 
                               ifelse(!is.na(x_a) & nchar(x_a)==8, x_a, NA) }),
      .SDcols = dates_v]
  
  if(verbose_a){
    cat("\n\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\nAfter correction :")
    dtb[, lapply(.SD,  nchar_f), .SDcols = dates_v]
  }
  
  
  ######################################################################################
  ##################################### Year of birth ##################################
  
  # creation of the year of birth, necessary to construct strata
  dtb[,c("dob_y") := substr_mod_f(dob, 1, 4)]
  
  # restriction to years in years_sample_a
  dtb = dtb[dob_y %in% years_sample_a] 


  ######################################################################################
  ################################## Names and places ##################################
  
  # removal of accents etc. to avoid encoding problems with fread and for 
  # standardization before comparison during the deduplication step
  
  # see the code of name_correction_f
  
  col_names = grep("^name", names(dtb), value=T)
  
  dtb[, c("family_name", col_names) := lapply(.SD, name_correction_f), 
      .SDcols = c("family_name", col_names) ]
  
  # Correction of place, to avoid encoding problems with fread
  dtb[, place := stri_trans_general(place, "Latin-ASCII")]
  
  cat("Size of the initial sample: ", n_sample, "\n")
  
  cat("Final size of dtb after restriction to years of interest: ", dtb[,.N], "\n")
  
  return(dtb)
  
}