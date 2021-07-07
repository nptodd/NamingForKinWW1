
llCRS <- CRS("+proj=longlat +ellps=WGS84")


#########################
##### Contemporary map
France_dpt <- rgdal::readOGR("../data/cartes/Dept_shp/DEPARTEMENT.SHP", stringsAsFactors = FALSE)

############# 
### Curation 

# Removal of useless variables
France_dpt@data <- France_dpt@data[, c("NOM_DEPT", "CODE_DEPT")]

# for concordance with geneanet
France_dpt$CODE_DEPT_G <- paste0("F", France_dpt$CODE_DEPT)
France_dpt$CODE_DEPT_G[France_dpt$CODE_DEPT_G %in% c("F2A", "F2B")] <- "F20"


##### Fusion des deux Corse
data_fr <- France_dpt@data
data_fr <- data_fr[data_fr$NOM_DEPT != "CORSE-DU-SUD",]
data_fr[data_fr$NOM_DEPT == "HAUTE-CORSE", "NOM_DEPT"] <- "CORSE"

France_dpt <- unionSpatialPolygons(France_dpt, 
                                   IDs = France_dpt$CODE_DEPT_G )

data_fr2 <- data_fr[match(getSpPPolygonsIDSlots(France_dpt), 
                         data_fr$CODE_DEPT_G),] 

France_dpt <- SpatialPolygonsDataFrame(France_dpt, data_fr2, match.ID = F)

rm(data_fr)
rm(data_fr2)


# # check
# plot(France_dpt)
# text(coordinates(France_dpt), labels=France_dpt$NOM_DEPT, cex=0.6)




##### Creation of the frontiers from current map (Corse left aside) : France_all
France_all <- unionSpatialPolygons(France_dpt, 
                                   IDs = ifelse(! France_dpt$CODE_DEPT %in%
                                                  c("2A", "2B"), 1, NA))



#########################
##### Map 1900
France_dpt_1900 <- rgdal::readOGR("../data/cartes/donnees_PrincetonEuropeMaps/1900 Ih/euro_1900_princ_data_calcright_wice.shp", 
                                  p4s = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs",
                                  stringsAsFactors = FALSE)

France_dpt_1900 <- France_dpt_1900[France_dpt_1900$LAND_2=="70",]
France_dpt_1900 <- spTransform(France_dpt_1900, CRS=CRS("+init=epsg:4326"))  

# Handle of names
France_dpt_1900$REGION_2[order(France_dpt_1900$REGION_2)]
France_dpt_1900$REGION_2[France_dpt_1900$NAME.1=="SEINE"] <- "SEINE"
France_dpt_1900@data[,c("TEILGEBIET", "NAME", "LAND_2", "AUTONUMBER", "YEAR", "NUM_DEP")] <- NULL


# Get numbers without spelling prolems in the shp of contemporary départements
ORDER <- match(France_dpt_1900$REGION_2, toupper(France_dpt$NOM_DEPT))
France_dpt_1900$NUM_DEP <- as.character(France_dpt@data[ORDER, "CODE_DEPT"])

# Manually retrieve other numbers
France_dpt_1900$REGION_2[is.na(France_dpt_1900$NUM_DEP)]

TC <- data.frame(NOM=c("PAS DECALA", "SEINE INFER", "MEURTHE", "SEINE ET OI", "SEINE ET MA",
                       "EURE ETLOI", "COTES DU NO", "ILLE ETVIL", "MARNE HAUTE", "LOIR ETCHE",
                       "COTE DOR", "HAUTE SAONE", "LOIRE INFER", "BELFORT, TE", "MAINE ET LO",
                       "INDRE ET LO", "SAONE ET LO", "DEUX SEVRES", "SAVOIE-HAUT", "HAUTE VIENN",
                       "CHARENTE IN", "PUY DEDOME", "HAUTE LOIRE", "ALPES HAUTE", "LOT ETGARO",
                       "ALPES BASSE", "TARN ETGAR", "ALPES-MARIT", "BOUCHESDU", "GARONNE",
                       "PYRENNEES H", "PYRENNEES B.", "CORSE", "PYR ORIENT", "SEINE"),
                 CODE=c(62, 76, 54, 78, 77, 
                        28, 22, 35, 52, 41, 
                        21, 70, 44, 90, 49,
                        37, 71, 79, 74, 87,
                        17, 63, 43, 5, 47,
                        4, 82, 6, 13, 31,
                        65, 64, 20, 66, 75))

France_dpt_1900[match(TC$NOM, France_dpt_1900$REGION_2),"NUM_DEP"] <- TC$CODE

France_dpt_1900$NUM_DEP <- as.numeric(France_dpt_1900$NUM_DEP)

France_dpt_1900@data[, c("REGION_2", "NUM_DEP")]

# # check
# plot(France_dpt_1900)
# text(coordinates(France_dpt_1900), labels=France_dpt_1900$REGION_2, cex=0.6)

rm(list=c("TC", "ORDER"))


##### Creation of the adjacency matrice and of 
##### is_adj_f, that tests adjacency


Adj_dpt = spdep::poly2nb(France_dpt) # adjacency list

Adj_mat = diag(nrow(France_dpt))  # adjacency matrice

for(i in 1:nrow(Adj_mat)){
  
  for(j in Adj_dpt[[i]]){
    
    Adj_mat[i,j] = 1
    
  }  }


rm(Adj_dpt)


rownames(Adj_mat) <- colnames(Adj_mat) <- France_dpt$CODE_DEPT_G

isSymmetric(Adj_mat) # check 

is_adj_f = function(dep_a, dep_b){
  if(is.na(dep_a)|is.na(dep_b)  ){
    return(NA)
  }else{
    return(as.logical( Adj_mat[ rownames(Adj_mat)==dep_a, colnames(Adj_mat)==dep_b] ) )
  }
}

# check
# is_adj_f("F14", "F50")
