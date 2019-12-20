
m <- 3

shp_quint1<- list()

sp_quint1 <- rasterToPolygons(stacky_mois[[2+3*(m-1)]])
names(sp_quint1) <- "quintile1"
sp_quint1$quintile1[which(sp_quint1$quintile1==0)] <- NA # to help plotting
shp_quint1[[m]] <- st_as_sf(sp_quint1)

shp_quint1[[m]]$quintile1 <- as.factor(shp_quint1[[m]]$quintile1)

### we build the shapefile of prevision confidence
sp_quint5 <- rasterToPolygons(stacky_mois[[3+3*(m-1)]])
names(sp_quint5) <- "quintile5"
sp_quint5$quintile5[which(sp_quint5$quintile5==0)] <- NA # to help plotting
shp_quint5[[m]] <- st_as_sf(sp_conf)

ht_score <- rasterToPolygons(stack_month[[4+5*(m-1)]])
names(ht_score) <- "ROC.High.Tercile"
ht_score$ROC.High.Tercile[which(ht_score$ROC.High.Tercile==-1)] <- NA # to help plotting
htshp_score[[m]] <- st_as_sf(ht_score)


#    1st tercile = lower tercile; 2nd = middle; 3rd = upper
### mapping
titre <- paste0(ind_eng[ind]," ","for"," ", moi[m], " ", select_year)
maprev [[m]]<- qtm(shp_quint1[[m]], fill = "quintile1",
                   attr.outside = TRUE,borders = NULL, fill.style="cont",fill.breaks=c(seq(0.45, 1, length.out = 6)),
                   fill.palette = score_palette)+
   qtm(shp_quint5[[m]], fill = "quintile5",
       attr.outside = TRUE,borders = NULL, fill.style="cont",fill.breaks=c(seq(0.45, 1, length.out = 6)),
       fill.palette = score_palette, legend.fill.show=FALSE)+
   #tm_grid(y=c(43.487, 43.787), x=c(4,03,4.5,5),  labels.inside.frame = TRUE, alpha = 0, labels.rot = c(0,90))+
   #tm_scale_bar()+
   #tm_add_legend(type = "fill", labels = c("3rd tercile","2nd tercile","1st tercile"), col =  c("dodgerblue3","lightgrey","darkslategray2"), alpha = 0.5)+
   tm_layout(title = titre, title.position = c("left","bottom"), title.size = 0.5, legend.show = FALSE)

qtm(htshp_score[[m]], fill = "ROC.High.Tercile",
    fill.style="cont", fill.breaks=c(seq(0, 1, length.out = 6)),borders = NULL, fill.palette = score_palette)+
qtm(shp_quint1[[m]], fill = "quintile1",
    fill.style="cont",borders = NULL, fill.palette = score_palette) #fill.breaks=c(seq(0, 10, length.out = 6)),
