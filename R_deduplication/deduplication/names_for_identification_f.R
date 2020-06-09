
names_for_identification_f = function(data_a, names_a,
                                      type_a="firstname", soundex_a=T){
  
  # This function : 
  # - removes parts of names_a with less than 2 letters
  # - breaks in 1st, 2nd et 3+ names
  # - converts (if soundex_a == T) in soundex
  
  # test
  # names_a=c("prenoms")
  # data_a = data.table(prenoms = c("Jean l enfant yvan y",
  #                                 "Albert enfant Abel Francois",
  #                                 "Pierre Alphonse Renee",
  #                                 "mort ne", "", "ic", "le moal", "le guen"))
  
  if(! type_a %in% c("firstname", "familyname")){stop("Incorrect type_a argument")}
  
  if(soundex_a) {
    transform_loc = soundexFR
    names_loc = fn_insee_sdx
  } else {
    transform_loc = function(x){x} # no transformation
    names_loc = fn_insee[,preusuel]
  }
  
  
  cplt = (type_a != "firstname")  # if family name, we don't impose that transform_loc(x) is in names_loc
  
  
  names2 <- strsplit(data_a[[names_a]], split = " +", fixed = F)
  
  names2 <- lapply(names2, # rmv names with <3 char and not in names_loc 
                   function(x){x[nchar(x)>2 & ( cplt | (transform_loc(x) %in% names_loc)  )]})
  
  # first name
  names2a <- sapply(names2, 
                    function(x){ifelse(length(x)>0,  transform_loc(x[1]), NA) })
  
  # second name
  names2b <- sapply(names2, 
                    function(x){ifelse(length(x)>1,  transform_loc(x[2]), NA) })
  # other names
  names2c <- sapply(names2, 
                    function(x){ifelse(length(x)>2, 
                                       paste( transform_loc(x[3:length(x)]), collapse = " "), NA)})
  
  mid_comp = ifelse(soundex_a, "_sdx", "")
  
  data_a[,paste0(names_a, mid_comp, c("_c1", "_c2", "_c3") ):=list(names2a, names2b, names2c)]
  
  return(0)
}
