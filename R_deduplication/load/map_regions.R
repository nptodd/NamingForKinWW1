
### creation of the map of Geneanet regions

# add Geneanet regions to the map of departements
pos_match <- match(France_dpt$CODE_DEPT_G, 
                  dtb[,.(dep=unique(dep)), keyby=.(region)][,dep]  )
France_dpt$REGION_G <- dtb[,.(dep=unique(dep)), keyby=.(region)][pos_match, region]
rm(pos_match)

# France_dpt@data # check

# Creation of the map of regions based on the map of departements
France_reg <- unionSpatialPolygons(France_dpt, 
                                   IDs = France_dpt$REGION_G )

France_reg <- SpatialPolygonsDataFrame(France_reg, 
                                       data=data.frame(NAME =getSpPPolygonsIDSlots(France_reg) ),
                                       match.ID=F)

# # check
# plot(France_reg)
# text(coordinates(France_reg), labels=France_reg$NAME, cex=0.9)
