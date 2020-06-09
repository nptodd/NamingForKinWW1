######### Imputation of unknown sex

# NB : it is an extremely small fraction of the dataset

dtb[,table(sex, useNA = "always")]

dtb[,sex2:=sex]

########### 

names_imput  = dtb[sex==0, .(name)]

summary(as.factor(names_imput))

# restriction to first name
names_imput[, preusuel:=tolower(gsub(" +.*", "", name))]

names_imput[fn_insee, on=.(preusuel), proba:=proba_m]

# hist(names_imput[,proba])

names_imput[,sex_imput:="0"]
names_imput[proba>0.8, sex_imput:="1"]
names_imput[proba<0.2, sex_imput:="2"]


names_imput[sample(.N)]

dtb[sex==0, sex2:=names_imput[,sex_imput]]

# check
# dtb[sex==0, .(name, sex2)]

# those still of unknown sex
# dtb[sex2==0, .N]

# the majority are stillborn babies
summary(as.factor(dtb[sex2==0][grepl("(enfant)|(mort ne)|(sans vie)", 
                                      name,  ignore.case = T), name]), 
        maxsum=10)

dtb[,table(sex, useNA = "always")]
dtb[,table(sex2, useNA = "always")]

rm(names_imput)


######################################################################################
############################## EXCLUSIONS : sex INCONNU #############################


cat_exclus_f( "............................ Exclusion of individuals of unknown sex\n",
              "... Size of the dataset before exclusions :", dtb[,.N] , 
              "\n... Number of excluded individuals :", dtb[sex2==0,.N], "\n")

dtb = dtb[sex2!=0] # exclusion

cat_exclus_f("... Size of the dataset after exclusions:", dtb[,.N], "\n")

