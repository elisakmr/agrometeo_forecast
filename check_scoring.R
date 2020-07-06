library(ggplot2)
library(ggpubr)
ind_eng <- c("Rain – evaporation", "Accumulated rainfall","Number of rainy days","Number of days T max > 25°C",
             "Number of days T max > 35°C", "T max mean","T min mean","T mean mean")
nboot <-500

                ### CALCULATING NUMBER OF SCORING BUGS FOR EACH GRID AND CLIMATO PER INDICATOR ###


                                       ################# SEASON #################

kantile <- "ter" # ter or quint
ngrids <- 541

int_det <- list()
int_prob <- list()

detgridna <- vector()
probgridna <- vector()

detna <- list()
probna <- list()

df_error <- array(NA, dim=c(ngrids, 2, nindic))
colnames(df_error) <- c("det_error","prob_error")
# loading score arrays

for (ind in c(3:5)){

  score_deter <- array(NA, dim=c(ngrids, 2, nboot))
  score_prps <- array(NA, dim=c(ngrids, 2, nboot))

  # binding iterative scores in same data frame

  for (b in 1:nboot)  {

    nom_prev <- paste0(indic[ind], b,"scoredet_grid_safran8.RData")
    load(file = file.path(dir_data, "score", nom_prev))

    score_deter[,,b] <- score_det[,]

    nom_scoreroc <- paste0(ind, b, "_", kantile, "aucgrid_safran8.RData")
    load(file = file.path(dir_data, "score", nom_prev))

    score_prps[,,b] <- scoreprob_seas_ps[,]

  }

  # getting na value indices (no score distinction: HT and LT mixed together, same as RMSE/ACC/R²)

  int_det[[ind]]<-which(is.na(score_deter), arr.ind = TRUE)

  int_prob[[ind]]<-which(is.na(score_prps), arr.ind = TRUE)

  # counting number of na per grid and saving figures as an array dim [ grids*type(ps/clim et prob/det)*ind ]

  for (i in 1:ngrids){
    detgridna[i] <- sum(int_det[[ind]][,1]==i) # 3 scores mixed together
    # detgridna_accps[i] <- sum(int_det[[ind]][,2]==i)
    # detgridna_accclim[i] <- sum(int_det[[ind]][,3]==i)

    probgridna[i] <- sum(int_prob[[ind]][,1]==i) # low and high tercile mixed together
  }

  detna[[ind]] <- detgridna

  probna[[ind]] <- probgridna

  df_error[,,ind] <- c(detgridna, probgridna)

}

### PLOTTING ERROR SUMMARY
# parameter: which score to check 1=prob ps 2=det ps 3=det clim
para <- "prob_error"

# faceted histogram
histo <- list()
for (ind in c(1:8)){
  if (sum(which(df_error[,para,ind]>10))==0){
    histo[[ind]] <- ggplot() + geom_point() + labs(title = ind_eng[ind], subtitle = paste0("no season ",para))+
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10)
      )
  }
  else {histo[[ind]] <- ggplot(as.data.frame(df_error[,,ind]), aes(x=prob_error)) + geom_histogram(bins=10) +
    labs(title = ind_eng[ind], subtitle = paste0(para, " on season "))+
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10))+
    scale_x_continuous(name = "added errors")+
    scale_y_continuous(name = "frequency")
  }
}
ggarrange(histo[[1]],histo[[2]],histo[[3]],histo[[4]],histo[[5]],histo[[6]],histo[[7]],histo[[8]])

