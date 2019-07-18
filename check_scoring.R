library(ggplot2)
library(ggpubr)
ind_eng <- c("Rain – evaporation", "Accumulated rainfall","Number of rainy days","Number of days T max > 25°C",
             "Number of days T max > 35°C", "T max mean","T min mean","T mean mean")

### CALCULATING NUMBER OF SCORING BUGS FOR EACH GRID AND CLIMATO PER INDICATOR

int_det1 <- list()
int_det2 <- list()
int_prob1 <- list()
int_prob2 <- list()

detgridna_1 <- vector()
detgridna_2 <- vector()
probgridna_1 <- vector()
probgridna_2 <- vector()

detna_1 <- list()
detna_2 <- list()
probna_1 <- list()
probna_2 <- list()

df_error <- array(NA, dim=c(ngrids, 4, nindic))
colnames(df_error) <- c("det_ps","det_clim","prob_ps","prob_clim")
  # loading score arrays

for (ind in 1:nindic){

score_detps <- array(NA, dim=c(ngrids, 3, ndimT, 1000))
score_detclim <- array(NA, dim=c(ngrids, 3, ndimT, 1000))

score_prps <- array(NA, dim=c(ngrids, ndimT, 2, 1000))
score_prclim <- array(NA, dim=c(ngrids, ndimT, 2, 1000))

  # binding iterative scores in same data frame

  for (b in 1:1000)  {

      nom_prev <- paste0(indic[ind], b,"scoredet_grid_prev.RData")
      nom_clim <- paste0(indic[ind], b,"scoredet_grid_clim.RData")
      load(file = file.path(dir_data, "score", nom_prev))
      load(file = file.path(dir_data, "score", nom_clim))

      score_detps[,,,b] <- det_prev[,,]
      score_detclim[,,,b] <- det_prev[,,]

      nom_prev <- paste0(indic[ind], b,"scoreprob_grid_prev.RData")
      nom_clim <- paste0(indic[ind], b,"scoreprob_grid_clim.RData")
      load(file = file.path(dir_data, "score", nom_prev))
      load(file = file.path(dir_data, "score", nom_clim))

      score_prps[,,,b] <- scoreprob_ps[,,1:2]
      score_prclim[,,,b] <- scoreprob_clim[,,1:2]

  }

  # getting na value indices (no score distinction: HT and LT mixed together, same as RMSE/ACC/R²)

int_det1[[ind]]<-which(is.na(score_detps), arr.ind = TRUE)
int_det2[[ind]]<-which(is.na(score_detclim), arr.ind = TRUE)

int_prob1[[ind]]<-which(is.na(score_prps), arr.ind = TRUE)
int_prob2[[ind]]<-which(is.na(score_prclim), arr.ind = TRUE)

  # counting number of na per grid and saving figures as an array dim [ grids*type(ps/clim et prob/det)*ind ]

   for (i in 1:ngrids){
      detgridna_1[i] <- sum(int_det1[[ind]][,1]==i)
      detgridna_2[i] <- sum(int_det2[[ind]][,1]==i)

      probgridna_1[i] <- sum(int_prob1[[ind]][,1]==i)
      probgridna_2[i] <- sum(int_prob2[[ind]][,1]==i)
  }

detna_1[[ind]] <- detgridna_1
detna_2[[ind]] <- detgridna_2

probna_1[[ind]] <- probgridna_1
probna_2[[ind]] <- probgridna_2

df_error[,,ind] <- c(detgridna_1, detgridna_2, probgridna_1, probgridna_2)

}

### PLOTTING ERROR SUMMARY
# parameter: which score to check 1=prob ps 2=prob clim 3=det ps 4=det clim
para <- "det_ps"

# faceted histogram
histo <- list()
for (ind in 1:nindic){
  if (sum(which(df_error[,para,ind]>10))==0){
    histo[[ind]] <- ggplot() + geom_point() + labs(title = ind_eng[ind], subtitle = paste0(para,"  without error"))+
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10)
      )
  }
  else {histo[[ind]] <- ggplot(as.data.frame(df_error[,,ind]), aes(x=prob_ps)) + geom_histogram(bins=10) +
    labs(title = ind_eng[ind], subtitle = paste0(para, " error histogram "))+
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10))+
    scale_x_continuous(name = "added errors")+
    scale_y_continuous(name = "frequency")
    }
}
ggarrange(histo[[1]],histo[[2]],histo[[3]],histo[[4]],histo[[5]],histo[[6]],histo[[7]],histo[[8]])

