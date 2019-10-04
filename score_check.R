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
