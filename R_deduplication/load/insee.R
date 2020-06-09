
fn_insee <- read.dbf("../data/nat2015.dbf",as.is=T)

setDT(fn_insee)

fn_insee <- fn_insee[annais!="XXXX" & 
                       preusuel!="_PRENOMS_RARES"]
fn_insee[, annais:=as.numeric(annais)]
fn_insee <- fn_insee[annais %in% 1899:1940]


# encoding problems
fn_insee[, preusuel := iconv(preusuel, from="latin1", to="UTF-8")]

car_list <- unlist( strsplit(fn_insee[,preusuel], ""))
cat("Characters used : \n")
print(summary( as.factor( car_list ) ))
rm(car_list)

# accentuated characters are removed
fn_insee[,preusuel:=tolower(stri_trans_general(preusuel, "Latin-ASCII"))]

# "-" replaced by " "
fn_insee[, preusuel:=gsub(pattern = "-", replacement = " ", x = preusuel)]

car_list <- unlist( strsplit(fn_insee[,preusuel], ""))
cat("Characters used (after curation): \n")
print(summary( as.factor( car_list ) ))

# creation of soundex versions of INSEE names
fn_insee_sdx <- unique(soundexFR(fn_insee[,preusuel]))

# conversion to wide format 
fn_insee <- dcast(fn_insee, preusuel ~ sexe, value.var = "nombre", fun.agg = sum)

setnames(fn_insee, c("1", "2"), c("m", "f"))

fn_insee[,proba_m:=m/(m+f)]


