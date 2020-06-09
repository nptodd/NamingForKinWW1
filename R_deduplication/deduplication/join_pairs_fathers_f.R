join_pairs_fathers_f= function(strat_a, CUTOFF_a, diagnostic_a=F){
  
  # test
  # strat_a = strata_pairs[1]
  # CUTOFF_a <- 6
  
  
  # load results of deduplication procedure
  result0 <-  loadRLObject(strat_a)
  pairs_all <- getPairs(result0,  max.weight = Inf, min.weight = -Inf, single.rows=T)
  setDT(pairs_all)
  
  
  # load father's data
  strat_name = gsub(".*/", "", strat_a)
  strat_name = gsub(".RData", "", strat_name, fixed = T)
  loc_fathers = fread(paste0("../data/stratified_melted/peres/", strat_name, ".csv"))
  
  # creation of fraction. names 
  names_for_identification_f(loc_fathers, "name", soundex_a = F)

  
  if(diagnostic_a){
  pdf(paste0("../figs/diagnostic_classification/weights/", strat_name, ".pdf"))
  hist(pairs_all[,Weight], main=strat_name, xlab="Weight" )
  hist(pairs_all[Weight>0 & Weight<20,Weight],
       main=paste0("Weights >0, ", strat_name), 
       xlab="Weight" )
  dev.off()
  }
  
  ############# 
  # Visual inspection
  
  # pairs_inspect <- getPairs(result0, min.weight=4, max.weight=5, single.rows=T)
  # setDT(pairs_inspect)
  #                              
  # var_see <- c("family_name", "name",
  #              "name_ancestor",
  #              "dep2", "lat2", "lon2", # geographical info
  #              "dob",  "dod",
  #              "dob_f_y", "dod_ancestor_y") # paternal yob and yod
  # X <- expand.grid(c(".1", ".2"), var_see)
  # var_see <- c("Weight", paste0(X[[2]], X[[1]]) )
  # 
  # View(pairs_inspect[, var_see, with=F])
  ############# 
  
  ############# DIAGNOSTIC PLOT
  if(diagnostic_a){
  possible_weights <- seq(-10, 20, 0.5)
  N_final_indiv <- rep(NA, length(possible_weights))
  
  for(I in seq_along(possible_weights)){
    
    table_liens = pairs_all[Weight > possible_weights[I], .(unique_id.1, unique_id.2)]
    links <- graph_from_data_frame(d = table_liens, directed = F)
    components_links <- components(links)
    membership_dt <- data.table(unique_id = names(components_links$membership),
                                merged_id = paste0(strat_name, "_m" , components_links$membership) )
    loc_loc_fathers <- merge(loc_fathers, membership_dt, by = c("unique_id"), all.x=T)
    loc_loc_fathers[is.na(merged_id), merged_id:=unique_id]
    N_final_indiv[I] <- length(unique(loc_loc_fathers[,merged_id]))
  }
  pdf(paste0("../figs/diagnostic_classification/cutoffs/", strat_name, ".pdf"))
  plot(possible_weights, N_final_indiv/1e3, type="l", lwd=1.4,
       xlab="Cut-off", 
       ylab="N final individuals (x1,000)",
       axes=F)
  mtext(strat_name, 3, 1, font = 2)
  axis(1, at=5*c(-4: 12)); axis(2, las=2)
  abline(v=CUTOFF_a, lty=2, lwd=1.4, col="red")
  dev.off()
  }
  ############# 
  
  
    
  table_liens = pairs_all[Weight > CUTOFF_a, .(unique_id.1, unique_id.2)]
  
  # nombre de liens
  table_liens[,.N]
  # nombre de personnes impliquées dans des liens
  # length(unique(c(table_liens[,"unique_id.1"], table_liens[,"unique_id.2"]) ))
  
  
  # création du graphe et extraction des composantes connexes
  # les 2 premières colonnes de table_liens doivent impérativement  
  # être les sommets de départ et d'arrivée de l'arrête
  links <- graph_from_data_frame(d = table_liens, directed = F)
  
  # description du graphe
  # links
  # plot(links)
  
  # détermination du nombre de composantes connexes (individus uniques)
  components_links <- components(links)
  
  # nombre de composantes
  # components_links$no
  
  membership_dt <- data.table(unique_id = names(components_links$membership),
                              merged_id = paste0(strat_name, "_m" , 
                                                 components_links$membership) )
  
  # ajout de la colonne merged_id, nouvel identifiant unique
  loc_fathers <- merge(loc_fathers, membership_dt, by = c("unique_id"), all.x=T)
  
  # les individus qui n'étaient impliqués dans aucun lien potentiel (et donc ne sont dans 
  # aucun lien final) doivent se voir attribuer un nouvel identifiant unique quand même !
  loc_fathers[is.na(merged_id), merged_id:=unique_id]
  
  
  # sauvegarde de la table de correspondance unique_id - merged_id
  fwrite(x = loc_fathers[,.(unique_id, merged_id)], 
         file = paste0("../data/table_deduplication/table_", strat_name, ".csv") )
  
  # voir les plus grosses composantes
  # id_max <- membership_dt[, .(N_compo = .N), by=.(merged_id)][N_compo==max(N_compo), merged_id]
  # View(loc_fathers[merged_id %in% id_max[1]])
  
  ##### élimination des pères doublonnés
  
  
  # colonnes inutiles dans le jeu de données dédoublonné.
  # NB : il est impératif de se fier aux colonnes de prénoms
  # scindées par names_for_identification_f, plutôt qu'aux colonnes
  # originales
  loc_fathers[,`:=`(idtree=NULL,  unique_id=NULL, 
                    name=NULL, name_ancestor=NULL,
                    dob=NULL, dod=NULL, 
                    dob_f=NULL, dod_ancestor=NULL,
                    dod_father_d=NULL, dod_father_m=NULL, dod_father_y=NULL, 
                    ancestor=NULL, sex_ancestor=NULL, type_ancestor=NULL)]
  
  # on retient la valeur la plus fréquente de chaque colonne (hors les NA et les "")
  new_loc_fathers <- loc_fathers[, lapply(.SD, getmode_f) , by=.(merged_id)]
  
  # ajout du nombre de contributeurs au nouvel individu
  n_contrib <- loc_fathers[, .(n_contrib=.N),by=.(merged_id)]
  new_loc_fathers <- merge(new_loc_fathers, n_contrib, 
                           by=c("merged_id"), all.x=T)
  
  # réduction du nombre d'individus due au dédoublonnage
  # new_loc_fathers[,.N]
  # loc_fathers[,.N]
  
  setcolorder(new_loc_fathers,
              c("stratum", "merged_id", "n_contrib", "family_name",
                "name_c1", "name_c2", "name_c3", 
                "sex", "sex2", 
                "dob_y", "dob_m", "dob_d",   # date of birth
                "dod_y", "dod_m", "dod_d",   # date of death
                "place", "dep", "region", "lat","lon",
                "dep2", "lon2", "lat2", 
                "ll_in_France",   "dep_ll", "cas_geoloc", 
                "dob_f_y", "dob_f_m", "dob_f_d" # father's date of birth
              ))
  
  # test_id <- sample(loc_fathers[,.N, by=.(merged_id)][N>1, merged_id], 1)
  # View(rbindlist(list(new_loc_fathers[merged_id == test_id],
  #                     loc_fathers[merged_id == test_id]),
  #                     fill = T))
  
  return(new_loc_fathers)
}
