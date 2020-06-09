

# Sex (1 ou 2) and year of birth dob_y define strata,  
# These variables are available for all individuals still in the dataset

dtb[, stratum:=paste0("S", sex2, "Y", dob_y)]

do.call(c, lapply(dtb[,unique(stratum)], 
                  function(x_a){fwrite(dtb[stratum==x_a, ], 
                                       paste0("../data/stratified/", x_a, ".csv"))}
))

rm(dtb) ; gc()

