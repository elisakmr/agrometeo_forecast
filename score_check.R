# looking for NAs score
ind <- 8
score_ps <- array(NA, dim=c(ngrids, event, 1000))

for (b in 1:1000)  {

  nom_prev <- paste0(indic[ind], b,"scoreprobseason_grid_prev.RData")

  load(file = file.path(dir_data, "score", nom_prev))

  score_ps[,1,b] <- scoreprob_ps[,1] # low tercile
  score_ps[,2,b] <- scoreprob_ps[,2] # high tercile

}

which(is.na(score_ps), arr.ind = TRUE) #  grid 268    event 2  iteration 265 - 405    2  265 - 527    2  559

# analyzing corresponding probabilistic vector

nom_ref <- paste0(indic[ind],b,"_ref.RData")
nom_prev <- paste0(indic[ind],b,"_prev.RData")

load(file = file.path(dir_data, "extract", nom_ref))
load(file = file.path(dir_data, "extract", nom_prev))


seasprob_ps <- tercquint("prev", "seasonal", array_prev, nan, ngrids)
seasbin_ref <- tercquint("obs", "seasonal", array_obs, nan, ngrids)

seasbin_ref[527,,3]
seasprob_ps[527,,3]

b<-265

roc.area(seasbin_ref[267,,3],seasprob_ps[267,,3])


library(verification)


nom_ref <- paste0(indicateur,b,"_ref.RData")
nom_prev <- paste0(indicateur,b,"_prev.RData")

load(file = file.path(dir_data, "extract", nom_ref))
load(file = file.path(dir_data, "extract", nom_prev))

seas_obs <- apply(array_obs[,,9:20],c(1,2), sum)
seas_prev <- apply(array_prev[,,,9:20],c(1,2,3), sum)

    ## tercile ##

nom_ref <- paste0(indic[ind],b,"_ref.RData")
nom_prev <- paste0(indic[ind],b,"_prev.RData")

load(file = file.path(dir_data, "extract", nom_ref))
load(file = file.path(dir_data, "extract", nom_prev))

seas_obs <- apply(array_obs[,,],c(1,2), sum)
seas_prev <- apply(array_prev[,,,],c(1,2,3), sum)

veriApply("EnsRoca", fcst=seas_prev[8,,], obs=seas_obs[8,], prob=1:2/3, parallel = TRUE)$cat1

nom_prevseas <- paste0(indic[ind],b,"_seasprob_prev.RData")
nom_refseas <- paste0(indic[ind],b,"_seasbin_obs.RData")
load(file = file.path(dir_data, "extract", nom_prevseas))
load(file = file.path(dir_data, "extract", nom_refseas))

roc.area(obs=seasbin_ref[8,,1], pred=seasprob_ps[8,,1])

veriApply("EnsRoca", fcst=seas_prev[8,,], obs=seas_obs[8,], prob=1:2/3, parallel = TRUE)$cat3

roc.area(obs=seasbin_ref[8,,3], pred=seasprob_ps[8,,3])

    ## quintile ##

nom_ref <- paste0(indic[ind],b,"_ref.RData")
nom_prev <- paste0(indic[ind],b,"_prev.RData")

load(file = file.path(dir_data, "extract", nom_ref))
load(file = file.path(dir_data, "extract", nom_prev))

seas_obs <- apply(array_obs[,,],c(1,2), sum)
seas_prev <- apply(array_prev[,,,],c(1,2,3), sum)

veriApply("EnsRoca", fcst=seas_prev[8,,], obs=seas_obs[8,], prob=1:4/5, parallel = TRUE)$cat1

nom_prevseas <- paste0(indic[ind],b,"_seasquint_prev.RData")
nom_refseas <- paste0(indic[ind],b,"_seasquint_obs.RData")
load(file = file.path(dir_data, "extract", nom_prevseas))
load(file = file.path(dir_data, "extract", nom_refseas))

roc.area(obs=seasbin_ref[8,,1], pred=seasprob_ps[8,,1])

veriApply("EnsRoca", fcst=seas_prev[8,,], obs=seas_obs[8,], prob=1:4/5, parallel = TRUE)$cat5
roc.area(obs=seasbin_ref[8,,5], pred=seasprob_ps[8,,5])


    ## msess ##
nom_ref <- paste0(indic[ind],b,"_ref.RData")
nom_prev <- paste0(indic[ind],b,"_prev.RData")
nom_clim <- paste0(indic[ind],b,"_clim.RData")
load(file = file.path(dir_data, "extract", nom_ref))
load(file = file.path(dir_data, "extract", nom_prev))
load(file = file.path(dir_data, "extract", nom_clim))

prev_seas <- apply(array_prev, c(1,2,3), sum)
clim_seas <- apply(array_clim, c(1,2,3), sum)
ref_seas <- apply(array_obs, c(1,2), sum)

veriApply("EnsMsess", fcst=prev_seas, fcst.ref=clim_seas, obs=ref_seas, parallel = TRUE)[75]
meanps <- apply(prev_seas[75,,],1,mean)
meanclim <- apply(clim_seas[75,,],1,mean)
mses <- 1-mean((meanps-ref_seas[75,])^2)/mean((meanclim-ref_seas[75,])^2)

    ## acc ##

nom_an_prev <- paste0(indic[ind],b,"_anomaly_prev.RData")
nom_an_obs <- paste0(indic[ind],b,"_anomaly_obs.RData")
load(file = file.path(dir_data, "extract", nom_an_prev))
load(file = file.path(dir_data, "extract", nom_an_obs))

veriApply("EnsCorr", fcst=prev_anomaly[45,,], obs=obs_anomaly[45,])
prev_seas <- apply(prev_anomaly, c(1,2), mean)
cor(prev_seas[45,],obs_anomaly[45,])


    ## det box ##

  boot_seas <- list()

  for(b in 1:nboot)   {
    # loading gridded scores

    # seasonal
    nom_prevseas <- paste0(indic[ind], b,"scoredetseas_grid_prev.RData")
    load(file = file.path(dir_data, "score", nom_prevseas))
    val_prevseas <- list()

    for (i in 1:ngrids){
      val_prevseas[[i]] <- det_seas[i,]*cos(pi*lati[i,1]/180)
      coeff[i] <- cos(pi*lati[i,1]/180)
      # removing NAs
      if (length(which(is.na(det_seas[i,])))>0){
        coeff[i] <- NA
        val_prevseas[[i]] <- rep(NA,2)
      }
    }

    boot_seas[[b]] <- apply(array(unlist(val_prevseas) , c(2,ngrids)), 1, sum, na.rm = T)/sum(coeff, na.rm = T)
  }

  seas_ps <- apply(array(unlist(boot_seas) , c(2,nboot)), 1, mean, na.rm=T)


sum(coeff, na.rm = T)
cos(pi*lati[i,1]/180)


