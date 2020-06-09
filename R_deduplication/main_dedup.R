# sessionInfo() 

####################################################################
##########################  Packages used  #########################

sapply(c("data.table", "parallel", "foreign", 
         "colorRamps", "stringi",
         "sp", "maptools", "spatstat", # "classInt", 
         "RecordLinkage",  "parallel",  "igraph"), 
       require, character.only = TRUE)

options("ffbatchbytes" =3e9 )
options("ffmaxbytes" =120e9 )


####################################################################
########################  Useful functions  ########################


### short functions
source("misc_functions/short_functions.R", encoding="utf8")
### functions for soundex
source("deduplication/soundex_f.R")
# function that 'breaks' first names and creates their soundex representation
source("deduplication/names_for_identification_f.R", encoding="utf8")

### Definition of :
# - fn_insee : data.table with first names from the INSEE file
# - fn_insee_sdx : vector of soundex values of INSEE first names
source("load/insee.R", encoding="utf8")


### Maps of France
gpclibPermit()
source("load/load_maps.R")


####################################################################
# _____________________  Loading geneanet data ____________________#
#       + write csv files by stratum (sex and 'département')       #
# _________________________________________________________________#
####################################################################  

source("load/load_data_f.R", encoding="utf8")
# dtb <-  load_data_f("sample", n_sample = 1e5)
# dtb <-  load_data_f("sample", n_sample = 3e6,
#                     rndm_sample_a=T, years_sample_a=1911:1920)
dtb <- load_data_f("full") 


# Loading using load_data_f : 
# - a unique identifier is created for each 'geneanet individuel'
# - dates less than 8 characters long are set to NA
# - the birth year variable is created (dob_y)
# - accents etc. are removed from name and first names using name_correction_f  
# - variable 'place' is readily curated using stri_trans_general
#    --------> These variables are from now on series of a-z characters separated by at most 1 space


source("load/map_regions.R") # creation of the map of geneanet regions

source("load/clean_geodata.R") # curation of geographical data
summary(as.factor(dtb[, cas_geoloc]))

source("load/clean_sex.R", encoding="utf8") # imputation of missing sexes

dir.create("../data/stratified", showWarnings = F)
source("load/save_strata.R") # writing one file / stratum


####################################################################
# __________________ Conversion to long format ___________________ #
#               + save separately uncles and fathers               #
# _________________________________________________________________#
####################################################################  

# Separate csv files are saved for fathers and uncles
# Aunts and ancestors of unknown sex are removed
# Dates are also curated on this occasion

strata <- list.files("../data/stratified", full.names = T)

dir.create("../data/stratified_melted", showWarnings = F)
dir.create("../data/stratified_melted/peres", showWarnings = F)
dir.create("../data/stratified_melted/oncles", showWarnings = F)

source("misc_functions/melt_f.R")

ncores = floor(length(strata)/2)
clust = makeCluster(ncores)
clusterExport(cl = clust, 
              varlist = c("fn_insee", "substr_mod_f", 
                          "correct_date_f", "names_for_identification_f") )
do.call(cbind, clusterEvalQ(clust, require(data.table)))
parSapply(clust, strata, melt_f)
stopCluster(clust) 


###################################################################
#             Identification of duplicates (fathers)              #
# ________________________________________________________________#
###################################################################

strata_fathers <- list.files("../data/stratified_melted/peres", 
                             full.names = T)

dir.create("../data/pairs", showWarnings = F)

# deduplication function
source("deduplication/identify_pairs_f.R")

# deduplication is launched, by stratum
ncores = 20
clust = makeCluster(ncores)

do.call(cbind, clusterEvalQ(clust, require(data.table)))
do.call(cbind, clusterEvalQ(clust, require(RecordLinkage)))
clusterExport(cl = clust, varlist = c( "soundexFR", "accent", "strata_fathers", 
                                        "names_for_identification_f", 
                                        "fn_insee_sdx") )
parSapply(clust, strata_fathers, identify_pairs_f)

stopCluster(clust)


###################################################################
#                  Fusion of duplicates (fathers)                 #
# ________________________________________________________________#
###################################################################


strata_pairs = list.files("../data/pairs/", full.names = T)

# folder for deduplication tables created by join_pairs_peres_f
# These tables will be necessary for the de-duplication of uncles
dir.create("../data/table_deduplication", showWarnings = F) 

# for diagnostic plots
dir.create("../figs/diagnostic_classification", showWarnings = F)
dir.create("../figs/diagnostic_classification/weights", showWarnings = F)
dir.create("../figs/diagnostic_classification/cutoffs", showWarnings = F)


source("deduplication/join_pairs_fathers_f.R")

ncores = 30
clust = makeCluster(ncores)
do.call(cbind, clusterEvalQ(clust, require(data.table)))
do.call(cbind, clusterEvalQ(clust, require(RecordLinkage)))
do.call(cbind, clusterEvalQ(clust, require(igraph)))
clusterExport(cl = clust, varlist = c( "soundexFR", "names_for_identification_f", 
                                        "fn_insee_sdx", "fn_insee", "getmode_f") )
data_fathers <- parLapply(clust, 
                          strata_pairs, 
                          join_pairs_fathers_f, 
                          CUTOFF_a=6, diagnostic_a=T)
data_fathers <- rbindlist(data_fathers)
stopCluster(clust)


dir.create("../data/final_datasets", showWarnings = F)
save(data_fathers, file= "../data/final_datasets/data_fathers.RData")



###################################################################
#        Identification and fusion of duplicates (uncles)         #
# ________________________________________________________________#
###################################################################


strata_uncles <- list.files("../data/stratified_melted/oncles", 
                             full.names = T)

dir.create("../data/pairs_uncles", showWarnings = F)

# remove RData files if any
if( ! is.empty(list.files("../data/pairs_uncles/")) ){
  file.remove(list.files("../data/pairs_uncles/", full.names = T)) }

# deduplication function
source("deduplication/identify_join_pairs_uncles_f.R")

ncores = 30
clust = makeCluster(ncores)
do.call(cbind, clusterEvalQ(clust, require(data.table)))
do.call(cbind, clusterEvalQ(clust, require(RecordLinkage)))
clusterExport(cl = clust, varlist = c( "soundexFR", "accent", "strata_uncles",
                                        "names_for_identification_f", 
                                        "fn_insee_sdx", "getmode_f") )
data_uncles <- parLapply(clust, 
                          strata_uncles, 
                          identify_join_pairs_uncles_f)
data_uncles <- rbindlist(data_uncles)
stopCluster(clust)

save(data_uncles, file= "../data/final_datasets/data_uncles.RData")

