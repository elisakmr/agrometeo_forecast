library(s2dverification)

tictoc::tic()
testy <- Ano_CrossValid(array_prev, array_clim)
tictoc::toc()

prev_anomaly <- array(NA,dim=c(ngrids,nan,nrun,ndimT))
clim_anomaly <- array(NA,dim=c(ngrids,nan,nrun,ndimT))
obs_anomaly <- array(NA,dim=c(ngrids,nan,ndimT))

for (y in 1:nan){
  cv_mean_prev <- apply(array_prev[,-y,,], c(1,3,4), mean)
  cv_mean_clim <- apply(array_clim[,-y,,], c(1,3,4), mean)
  cv_mean_obs <- apply(array_obs[,-y,], c(1,3), mean)

  prev_anomaly[,y,,] <- array_prev[,y,,]-cv_mean_prev
  clim_anomaly[,y,,] <- array_clim[,y,,]-cv_mean_clim
  obs_anomaly[,y,] <- array_obs[,y,]-cv_mean_obs

}
