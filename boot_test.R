tictoc::tic()
#Spearman <- s2dverification:::.Corr
acc_prev <- vector()
acc_clim <- vector()
msess <- vector()

corel <- function(x,y){
  out <- veriApply("Spearman", x, y, method = 'spearman')$corr
  return(out)
}

for (ind in 3:5){

  # getDoParWorkers()
  # getDoParName()
  # registerDoSNOW(makeCluster(2, type = "SOCK"))
  # getDoParWorkers()
  # getDoParName()
  # , .verbose = T
  cl <- makeCluster(no_core)
  registerDoParallel(cl)

  foreach(b=1:8, .packages=c('easyVerification', 's2dverification'), .export = "corel") %dopar%  { # loop on bootstrap samples

    #Spearman <- s2dverification:::.Corr

    nom_ref <- paste0(indic[ind],b,"_ref.RData")
    nom_prev <- paste0(indic[ind],b,"_prev.RData")
    nom_clim <- paste0(indic[ind],b,"_clim.RData")
    nom_an_prev <- paste0(indic[ind],b,"_anomaly_prev.RData")
    nom_an_clim <- paste0(indic[ind],b,"_anomaly_clim.RData")
    nom_an_obs <- paste0(indic[ind],b,"_anomaly_obs.RData")

    load(file = file.path(dir_data, "extract", nom_ref))
    load(file = file.path(dir_data, "extract", nom_prev))
    load(file = file.path(dir_data, "extract", nom_clim))
    load(file = file.path(dir_data, "extract", nom_an_prev))
    load(file = file.path(dir_data, "extract", nom_an_clim))
    load(file = file.path(dir_data, "extract", nom_an_obs))

    # ensemble mean
    prev_mean <- apply(array_prev, c(1,2,4), mean)
    clim_mean <- apply(array_clim, c(1,2,4), mean)

    ### Score computing: We compute bias, and cross validated rmse and correlation coefficient.

    ### MONTHLY ###

    det_month <- array(NA, dim = c(ngrids,3,ndimT))

    for (t in 1:ndimT){

      # MSESS

      msess <- veriApply("EnsMsess", fcst=t(prev_mean[,,t]), fcst.ref=t(clim_mean[,,t]), obs=t(array_obs[,,t]))

      # ACC #

      # Pearson for continued indicators
      if (ind!=3 & ind!=4 & ind!=5) {
        # ref vs prev
        acc_prev <- veriApply("EnsCorr", fcst=prev_anomaly[,,,t], obs=obs_anomaly[,,t])
        # ref vs clim
        acc_clim <- veriApply("EnsCorr", fcst=clim_anomaly[,,,t], obs=obs_anomaly[,,t])

      } else { # Spearman for ordinal indicators
        # ref vs prev
        acc_prev <- veriApply(".Corr", fcst=prev_anomaly[,,,t], obs=obs_anomaly[,,t])
        # ref vs clim
        #acc_clim <- corel(clim_anomaly[,,,t], obs_anomaly[,,t])

      }

      #det_month[,,t] <- array(c(msess, acc_prev, acc_clim), dim=c(ngrids,3))

    }


  }
  stopCluster(cl)

}
tictoc::toc()

