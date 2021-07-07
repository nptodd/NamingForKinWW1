
# definition of working departements, longitudes et latitudes. 

dtb[,`:=`(lon=as.numeric(lon),
          lat=as.numeric(lat))]
dtb[,`:=`(dep2=dep,
          lon2=lon,
          lat2=lat)]
dtb[(lon2==0 & lat2==0),  # unknown (lon, lat) are valued (0, 0)
    `:=`(lon2=NA, lat2=NA)] # lon and lat are useful for the SpatialPoints


# map on a sample
# plot(SpatialPoints(dtb[sample(1:nrow(dtb), 100000, F),.(lon, lat)], llCRS), pch=20, cex=0.1)

dtb[, ll_in_France := over(x=SpatialPoints(coords = dtb[,.(lon, lat)],
                                        proj4string = France_reg@proj4string), 
                        y=France_reg)
    ] # polygon number if in France, NA otherwise (including (lon, lat) == (0, 0))
dtb[, ll_in_France:=!is.na(ll_in_France)]
dtb[is.na(lon2), ll_in_France:=NA]
summary(dtb[, ll_in_France])

gc()


############################## Coordinates outside of France

# exclusion of individuals born with certainty outside of France (ll_in_France==F)

# View(dtb[ll_in_France==F,.(ll_in_France, family_name, name, place, lat2, lon2, dep2)])

# a few exceptions left aside ('commune' on the frontier...), those ll_in_France == FALSE  
# are indeed born outside of France : they are excluded
cat_exclus_f( "............................ Exclusion des individus nés hors de France\n
... Taille du tableau de données avant exclusions :", dtb[,.N] , "\n
... Nombre d'exclus :", dtb[!is.na(ll_in_France) & ll_in_France==F,.N], "\n")

dtb = dtb[is.na(ll_in_France) | ll_in_France] # exclusion des ll_in_France==FALSE

cat_exclus_f("... Taille du tableau de données après exclusions :", dtb[,.N],
             "\n.........................................................................\n")


# map on a sample
# plot(SpatialPoints(dtb[ll_in_France==T][sample(.N, 100000, F),.(lon2, lat2)], llCRS), pch=20, cex=0.1)



############################## Coordinates in France

#### Descriptives results

# No issue regarding the link between region and departement :  
dtb[, table(dep, useNA = "always")] 
dtb[, table(region, useNA = "always")]
dtb[, summary(lon2, useNA = "always")] 
dtb[,.(dep=unique(dep)), keyby=.(region)][,table(dep)]

# grid::grid.newpage()
# VennDiagram::draw.triple.venn(area1 = dtb[is.na(region),.N],
#                               area2 = dtb[is.na(dep),.N],
#                               area3 = dtb[lon==0,.N],
#                               n12 = dtb[is.na(region) & is.na(dep),.N],
#                               n13 = dtb[is.na(region) & (lon==0),.N],
#                               n23 = dtb[is.na(dep) & (lon==0),.N],
#                               n123 = dtb[is.na(region) & is.na(dep)& (lon==0),.N],
#                               scaled = T,
#                               lty = "blank", fill = c("mediumorchid", "pink1",  "skyblue"),
#                               category = c("Région\ninconnue", "Département\ninconnu", "Coordonnées\ninconnues"))


#### comparison of dep et (lon, lat)

# ajout du département d'après la longitude et la latitude
dtb[, dep_ll := over(x=SpatialPoints(coords = dtb[,.(lon, lat)],
                                     proj4string = France_dpt@proj4string), 
                     y=France_dpt)$CODE_DEPT_G
    ] 
# add region based on longitude and latitude
# dtb[, region_ll := over(x=SpatialPoints(coords = dtb[,.(lon, lat)],
#                                         proj4string = France_reg@proj4string), 
#                         y=France_reg)$NAME
#     ] 

concord = dtb[, dep2==dep_ll]
concord_approx = mapply(is_adj_f, 
                        dep_a = dtb[, dep2], 
                        dep_b = dtb[, dep_ll])

# for the description of case, see below
dtb[,cas_geoloc:=""]
dtb[!is.na(lon2) & !is.na(dep) & concord_approx, 
    cas_geoloc:="cas1"]
dtb[!is.na(lon2) & !is.na(dep2) & !concord_approx & grepl("Paris|Lyon|Marseille", place),
    cas_geoloc:="cas2a"]
dtb[!is.na(lon2) & !is.na(dep2) & !concord_approx & !grepl("Paris|Lyon|Marseille", place),
    cas_geoloc:="cas2b"]
dtb[is.na(lon2) & !is.na(dep2), 
    cas_geoloc:="cas3"]
dtb[!is.na(lon2) & is.na(dep2), 
    cas_geoloc:="cas4"]
dtb[is.na(lon2) & is.na(dep2), 
    cas_geoloc:="cas5"]


### CASE 1

# best and most frequet case : departement and coordinates are available
# and in agreement. Nothing to modify
dtb[, table(!is.na(lon2) & !is.na(dep) & concord_approx)]


### CASE 2

# departement and coordinates are available but in disagreement. 
# Except for the case of Paris, Lyon and Marseille, we follow the departement 
# In a majority of case this is because geolocalisation is incorrect. It is set 
# to NA, and it will be handled by Case 3 (see below)

## Case 2a : Paris, Lyon, Marseille

# View(dtb[!is.na(lon2) & !is.na(dep2) & !concord_approx & grepl("Paris|Lyon|Marseille", place), 
#          .(place, dep2, lat2, lon2)])

dtb[!is.na(lon2) & !is.na(dep2) & !concord_approx & grepl("Paris", place),  dep2:="F75"]
dtb[!is.na(lon2) & !is.na(dep2) & !concord_approx & grepl("Lyon", place),   dep2:="F69"]
dtb[!is.na(lon2) & !is.na(dep2) & !concord_approx & grepl("Marseille", place), dep2:="F13"]

## Case 2b : the rest

# View(dtb[!is.na(lon2) & !is.na(dep) & !concord_approx & !grepl("Paris|Lyon|Marseille", place), 
#          .(place, dep2, lat2, lon2)])

dtb[!is.na(lon2) & !is.na(dep2) & !concord_approx & !grepl("Paris|Lyon|Marseille", place), 
    `:=`(lon2=NA, lat2=NA)]

# geolocalisation of Cases 2b will be attributed in Case 3


### CASE 3

# known departement but exact geolocalisation of the place has failed, so that lon2 and lat2 are 
# unknown, typically because 'place' only provides the département. We sample longitude and latitude
# in the département according to the density observed in complete cases of the departement

# View(dtb[is.na(lon2) & !is.na(dep2), .(place, dep2, lon2, lat2)] )

table_n = dtb[is.na(lon2) & !is.na(dep2), table(dep2)]

hotdeck_f = function(dep_a){
  
  pool_indices =  which(dtb[,!is.na(lon2)  & !is.na(lat2)  & concord & dep==dep_a])

  return( sample(pool_indices, table_n[dep_a], replace = T) )

}

set.seed(233)
imput_indices = lapply(names(table_n), hotdeck_f)
names(imput_indices) = names(table_n)

for(dep_i in names(table_n)){
  
  tab_loc = dtb[ imput_indices[[dep_i]], .(lon, lat)]
  
  lon_loc = tab_loc[, lon]
  lat_loc = tab_loc[, lat]

  dtb[is.na(lon2) & !is.na(dep2) & dep==dep_i, `:=`(lon2=lon_loc,
                                                    lat2 = lat_loc) ]
}

rm(list = c("table_n", "tab_loc", "lon_loc", "lat_loc", "imput_indices", "hotdeck_f"))


# check on an example
# dep_test = "F19"
# par(mfrow=c(1,2))
# plot( SpatialPoints(dtb[lon!=0 & !is.na(dep2) & concord & dep==dep_test, .(lon2, lat2)], proj4string = llCRS), cex=0.1 )
# plot(France_dpt, add=T)
# plot( SpatialPoints(dtb[lon==0 & !is.na(dep2) & dep==dep_test, .(lon2, lat2)], proj4string = llCRS), cex=0.1 )
# plot(France_dpt, add=T)
# rm(dep_test)

# check Case 2b has indeed been handled
# View(dtb[!(lon==0) & !is.na(dep2) & !concord_approx & !grepl("Paris|Lyon|Marseille", place),
#          .(place, dep2, lat, lon, lat2, lon2)])


### CASE 4

# geolocalisation succeeded but the departement is unknown
# we add the departement 

# View(dtb[!is.na(lon2) & is.na(dep2), .(place, dep2, lat2, lon2)] )

dep_loc = over(x=SpatialPoints(coords = dtb[!is.na(lon2) & is.na(dep2), .(lon2, lat2)],
                               proj4string = France_dpt@proj4string), 
               y=France_dpt)$CODE_DEPT_G

dtb[!is.na(lon2) & is.na(dep2), dep2:=dep_loc]

rm(dep_loc)


### CASE 5

# unknown departement and geolocalisation
dtb[is.na(lon2) & is.na(dep2), .N]



#############################
# last check
dtb[, table(is.na(lon2), is.na(dep2) )]

# region left aside, there is from now on one and only one geographical
# information, more (lon2,lat2) or less (dep2) precise

rm(concord)
rm(concord_approx)

################################################################################# 
