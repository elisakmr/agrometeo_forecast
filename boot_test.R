boot_month <- list()
boot_seas <- list()

for(b in 1:nboot)   {
  # loading gridded scores

  # seasonal
  nom_prevseas <- paste0(indic[ind], b,"scoredetseas_grid_prev.RData")
  load(file = file.path(dir_data, "score", nom_prevseas))

  box_prev_month <- matrix(nrow = ndimT, ncol = 3)

  # monthly
  if (ind!=3 & ind!=4 & ind!=5) {
    nom_prevmonth <- paste0(indic[ind], b,"scoredetmonth_grid_prev.RData")
    load(file = file.path(dir_data, "score", nom_prevmonth))

    for (t in 1:ndimT){
      # box aggregation with latitude weighting
      val_prevmonth <- list()

      for (i in 1:ngrids){
        val_prevmonth[[i]] <- det_month[i,,t]*cos(pi*lati[i,1]/180)
        coeff[i] <- cos(pi*lati[i,1]/180)
        # removing NAs
        if (length(which(is.na(det_month[i,])))>0){
          coeff[i] <- NA
          val_prevmonth[[i]] <- rep(NA,3)
        }

      }

      box_prev_month[t,] <- apply(array(unlist(val_prevmonth) , c(3,ngrids)), 1, sum, na.rm = T)/sum(coeff, na.rm = T)
      #box_prev_month[t,] <- Reduce("+", val_prevmonth)/sum(coeff)

    }
    boot_month[[b]] <- box_prev_month
  }
  # seasonal
  val_prevseas <- list()

  for (i in 1:ngrids){
    val_prevseas[[i]] <- det_seas[i,]*cos(pi*lati[i,1]/180)
    coeff[i] <- cos(pi*lati[i,1]/180)
    # removing NAs
    if (length(which(is.na(det_seas[i,])))>0){
      coeff[i] <- NA
      val_prevseas[[i]] <- rep(NA,3)
    }
  }

  boot_seas[[b]] <- apply(array(unlist(val_prevseas) , c(3,ngrids)), 1, sum, na.rm = T)/sum(coeff, na.rm = T)
}

if (ind!=3 & ind!=4 & ind!=5) {

  month_ps <- apply(array(unlist(boot_month) , c(6,3,nboot)), c(1,2), mean)
}
seas_ps <- apply(array(unlist(boot_seas) , c(3,nboot)), 1, mean, na.rm=T)


