
return_diff_f = function(date_a){
  return(as.numeric(date_a - as.Date("1905-01-01"))+1) 
  } # return_diff_f

nchar_f = function(x_a){
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n") 
  cat("Number of characters:")
  print(table(nchar(x_a), useNA="always") )  
  return(0)
} # nchar_f

name_correction_f=function(x_a, verbose_a=F){
  
  # test
  # x_a = dtb[, c(family_name, name, name_f, name_u6)]

  # example for stri_trans_general :
  # stri_trans_general("Irénée-Î ô ï", "Latin-ASCII")
  
  # first conversion (accents) + to lower case letters
  x = tolower(stri_trans_general(x_a, "Latin-ASCII"))
  
  if(verbose_a){
  car_list = unlist( strsplit(x, ""))
  cat("xxxxxxxxxxxxxxxxxxx\nNumber of characters used (+1 if NAs) : ", length(unique(car_list)), "\n" )
  cat("Characters used in original versions (upper case letters and cases handled by stri_trans_general left aside) :\n")
  print(summary( as.factor(car_list ) ))
  }
  
  # see first names where a specified character is present
  # grep("|", x = x, value=T, fixed=T)
  
  # replacement of parasitic character : dashes, etc. 
  
  res = gsub(pattern =  "\\.|[[:digit:]]|'|\\+|-|\\[|\\]|\u0093|\u0094|/|<|>|=|,|¦|\u0095|\\{|\\}|`",
             replacement = " ", x ) # "/", ",", "<" et ">" separate alternatives (edward / edwin). No good solution...
  
  res = gsub(pattern =  "\\", replacement = " ", res, fixed=T )
  
  res = gsub("\\(|\\)|¬|\u0089|¨|°|(\\$)|~|¤|\u0083|\\?|\\*|_|#|@|!|&|:|\\^|\u0084|ª|\u0081|¯|·|´|º|`|¿|§", "", res )
  # \u0081 à vérifier
  
  res = gsub("\u0082|\u008a|\u008e|\u008f|\u0090|\u0088|\u009e|\u009a", "e", res )
  
  res = gsub("\u008d|(a§)|\u0087|\u0080|¢", "c", res )
  
  res = gsub("\u0086", "u", res )
  res = gsub("\u008c", "i", res )
  res = gsub("\u009f", "s", res )
  res = gsub("µ", "ae", res )
  
  res = gsub(" +", " ", res)  # removal of supernumerary spaces
  res = gsub("( $)|(^ )", "", res)
  
  car_list = unlist( strsplit(res, ""))
  cat("Characters used in the modified version:\n ")
  print(summary( as.factor( car_list ) ))
  
  if(verbose_a){
  cat("Test of the effect of the 2nd correction on a sample :\n")
  smpl <- sample(which(x!=res), 15, replace = T)
  print(data.frame(original=x[smpl], apres_correction=res[smpl]), quote = T)
  }
  
  cat("Finallly, cases potentially untreated are removed. Only a-z letters and spaces are conserved.\n")
  
  res = gsub("[^a-z ]", "", res)
  
  return(res)
} # name_correction_f

cat_exclus_f=function(...){
cat(..., file = "../exclusions.log", append = TRUE)
} # cat_exclus_f

substr_mod_f = function(x, a, b){
  as.numeric(substr(x, a, b))} # substr_mod_f

correct_date_f=function(x_a, type_a="m", years_a=0:2020){
  
  if(! type_a %in% c("d", "m", "y")){stop("Incorrect argument")}
  
  if(type_a=="d"){
    admis_values =  1:31
  } else if (type_a=="m"){
    admis_values = 1:12
  } else { 
    admis_values = years_a
  }
  
  ifelse(x_a %in% admis_values, x_a, NA) 
} 

getmode_f  <- function(v_a){ # returns the most frequent value, NAs and "" left aside
  uniqv <- unique(v_a[!is.na(v_a) & !(v_a=="")])
  uniqv[which.max(tabulate(match(v_a, uniqv)))]
} # getmode_f

extract_premier_prenom_f=function(x_a){
  x_a = gsub("[^a-zA-Z -]","", x_a) # keeps only a-z, A-Z and dashes
  gsub('[(.*)^ ][A-Za-z-]+','', x_a) # returns x_a after removal of everything after first space
} # extract_premier_prenom_f

