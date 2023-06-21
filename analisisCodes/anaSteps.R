
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")

mycolsBW <- c("#000000", "#999999", "#FFFFFF")



library(poweRlaw)
#replaces_broom::
#bootstrap::
# library(rsample)
library(parallel); library(tictoc)

#' Gather results of non-linear distribution fits to validate a power law fit
#'
#' @param RealFreq
#' @param xMin
#' @param xMax
#' @param nSims
#'
#' @return "results" data frame
#' @export
#' @import poweRlaw, tictoc
#' @examples

CheckPoweRlaw <- function(
    
  RealFreq,
  xMin = NULL, #=default4bootstrap_p()
  
  #poweRlaw::bootstrap()Default
  #<9999999,>slow5000sec
  xMax = 140,  #140 x 10
  # xMax = 100000,
  
  #poweRlaw::bootstrap()Default
  # nSims = 100
  nSims = 99
  
) {  #freqvector
  #Clauset2009,Gillespie2015
  #bottleneck==boostrap()
  
  tic()
  nCores <- parallel::detectCores()
  
  
  #na.omit()----
  
  
  if(
    any(is.na(RealFreq)) | is.null(RealFreq) |
    all(RealFreq == 1 | RealFreq == 0) |
    (length(RealFreq) == 0) | (length(RealFreq) < 3)
  ) {
    
    results <- data.frame(
      "PlP" = NA,
      "PlExpP" = NA, "PlLognormP" = NA, "PlExpP" = NA,
      
      "PlPar" = NA,
      "ExpPar" = NA, "ExpPar" = NA,
      "LognormPar1" = NA, "LognormPar2" = NA
      
      # "PlVar" = PlVar,
      # "ExpVar" = ExpVar, "PoisVar" = PoisVar,
      # "LognormVar1" = LognormVar1, "LognormVar2" = LognormVar2
    )
    
    
    return(results)
    
    
    
  } else {
    # stopifnot(all(is.numeric(RealFreq)))
    RealFreq <-# if(!all(is.numeric(as_vector(RealFreq)))) {
      #addressPkg1:(x - 1)issue
      round(as.numeric(RealFreq) + 2)
    # } else { c(1, 1, 1, 1, 1) }
    
    
    
    #power----
    
    
    Pl <- poweRlaw::conpl$new(RealFreq)
    
    if(is.numeric(xMin)) {
      Pl$setXmin(xMin)
    } else {
      Pl$setXmin(
        #initial
        estimate_xmin(Pl,
                      #setXminsExplicitly2addressGillespie's1:(x - 1)uncertainty...
                      #assumesLeftCumsum()tail!=veryFlat...
                      # xmins = min(RealFreq):RealFreq[min(RealFreq + 2)],
                      xmax = xMax
        )
      )
    }
    
    Pl$setPars(estimate_pars(Pl))
    

    PlP <- bootstrap_p(
      Pl,
      # xmins = seq(
      #   from = xMin, to = xMax
      #   # by = ((xMax - xMin) / ((xMin + 1)))
      # ),
      xmins = Pl$getXmin(),
      xmax = xMax,
      threads = nCores, no_of_sims = nSims
    )$p  #>0.1passes
    
    
    #EXP
    
    Exp <- conexp$new(RealFreq)
    
    if(is.numeric(xMin)) {
      Exp$setXmin(xMin)
    } else { Exp$setXmin(estimate_xmin(Exp, xmax = xMax)) }
    
    Exp$setPars(estimate_pars(Exp))
    
    
    
    # PoisVar <- var(
    #   poweRlaw::bootstrap(
    #     Pois,
    #     xmins = Pois$getXmin(),
    #     xmax = xMax,
    #     threads = nCores, no_of_sims = nSims
    #   )$bootstraps$pars
    # )
    
    
    
    #lognorm----
    
    
    
    Lognorm <- conlnorm$new(RealFreq)
    if(is.numeric(xMin)) {
      Lognorm$setXmin(xMin)
    } else { Lognorm$setXmin(estimate_xmin(Lognorm, xmax = xMax)) }
    
    Lognorm$setPars(estimate_pars(Lognorm))
    
    
    
    #compare----
    
    
    
    #dis1xmin==dis2xmin
    
    #null=bothOK #1sided=arg1==arg2
   
    Lognorm$setXmin(Pl$getXmin())
    LognormPlP <- compare_distributions(Lognorm, Pl)$p_one_sided
    
    Exp$setXmin(Pl$getXmin())
    ExpPlP <- compare_distributions(Exp, Pl)$p_one_sided
    
    LogNormExpP <- compare_distributions(Lognorm, Exp)$p_one_sided
    
    #org----
    
    #pivot?==majorBreak!
    results <- data.frame(
      "LognormPlP" = LognormPlP, "ExpPlP" = ExpPlP, "LogNormExpP" = LogNormExpP,
      
      "PlPar" = Pl$pars,
      "ExpPar" = Exp$pars,
      "LognormPar1" = Lognorm$pars[1], "LognormPar2" = Lognorm$pars[2]
      
    )
    
    toc()
  }
  return(results)
}



DF_SAM <- read.csv("../../data/DF_muestrasPath_complete.csv", header = TRUE)


#nP <- 2000
#nW <- "1 worker"
###########Plo

DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="0.5"] <- "Asynchronous"
DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="1"] <- "Synchronous" 
DF_SAM$numWorkers[DF_SAM$numWorkers =="1"] <- "1 worker" 
DF_SAM$numWorkers[DF_SAM$numWorkers =="5"] <- "5 workers" 




DF_SAM$DistanceW <- sqrt(DF_SAM$DistanceW)

DF_TOTAL = DF_SAM %>%
  filter(Time ==5.5)

DF_TOTAL$Conteo <- 1
#DF_TOTAL <- DF_TOTAL %>% filter(DistanceW > 1.5)  ##ESTO ES TRAMPA, perp vamos a ver
DF_TOTAL$DistanceW <- round(DF_TOTAL$DistanceW,1) 

DF_TOTAL$Infection <- 0
DF_TOTAL$Infection[DF_TOTAL$Rust =="1"] <- "No Infection" 
DF_TOTAL$Infection[DF_TOTAL$Rust =="0"] <- "No Infection"
DF_TOTAL$Infection[DF_TOTAL$Rust =="0.5"] <- "New Infection"



DF_TOTAL_AG <- DF_TOTAL %>%
  group_by(HarvestModel, numPlants, numWorkers, DistanceW, HarvestTime, porcionCosecha, SimID) %>%
  summarise(FrecAbs =sum(Conteo))# 



DF_TOTAL_AG$Frec <- 2* DF_TOTAL_AG$FrecAbs/(DF_TOTAL_AG$numPlants*length(unique(DF_TOTAL$Rep)))
DF_TOTAL_AG$logDistanceW <- log(DF_TOTAL_AG$DistanceW)
DF_TOTAL_AG$logFrec <- log(DF_TOTAL_AG$Frec)




DF_TOTAL_AG_SHORT <- DF_TOTAL_AG %>%
  filter(!is.na(DistanceW))%>%
  filter(DistanceW != 0)%>%  #aqui perdemos un porcentaje del total
  filter(numWorkers== "1 worker")

DF_TOTAL_AG_SHORT <- DF_TOTAL_AG_SHORT %>%
  group_by(numPlants, porcionCosecha)%>%
  mutate(D_MEAND = DistanceW/ mean(DistanceW, na.rm = TRUE))

DF_TOTAL_AG_SHORT$logD_MEAND <- log(DF_TOTAL_AG_SHORT$D_MEAND)









############intento de mega loop para dataframe#############333
DF_INICIAL <- DF_TOTAL_AG_SHORT%>%
  filter(numPlants == 2000)%>%
  filter(porcionCosecha == "Asynchronous")

DF_DIST <- CheckPoweRlaw(DF_INICIAL$DistanceW)  #esta es solo para establecer
DF_DIST$numPlants <- 0
DF_DIST$porcionCosecha <- 0

for (nP in unique(DF_TOTAL_AG_SHORT$numPlants)){
  for (pC in unique(DF_TOTAL_AG_SHORT$porcionCosecha)){
  
    DF_TEMP <- DF_TOTAL_AG_SHORT %>%
      filter(numPlants == nP)%>%
      filter(porcionCosecha == pC)
    df1 <- CheckPoweRlaw(DF_TEMP$DistanceW)
    
    df1$numPlants <- nP
    df1$porcionCosecha <-pC
    
    
    DF_DIST <- rbind(DF_DIST, df1)
    
    #EXAMPLES
    m1= conpl$new(DF_TEMP$DistanceW)
    m1$setPars(estimate_pars(m1))
    m2 = conexp$new(DF_TEMP$DistanceW)
    m2$setPars(estimate_pars(m2))
    m3 = conlnorm$new(DF_TEMP$DistanceW)
    m3$setPars(estimate_pars(m3))
    
    jpeg(paste("../../output/graficas/SUP_FIG/dist_","numPlants_", nP, "coffee maturation_", pC, ".jpg"), width = 7, height = 6, unit="in", res=300)
    #par(mar = c(0.3, 0.3, 0.3, 0.3))
    plot(m2, ylab = "CDF", xlim=c(0.1,110), col= alpha(1, 0.4), main=paste(nP, "Plants/ha", pC))
    lines(m1, col=1, lty= 1, lwd= 2)
    lines(m2, col = 2, lty = 2, lwd= 2)
    lines(m3, col = 3, lty = 3, lwd= 2)
    legend("bottomleft", legend=c(paste("PowerLaw ", "alpha=", round(m1$pars, 2)), 
                                 paste("Exponential ", "lambda=", round(m2$pars, 2)), 
                                 paste("LogNormal ", "mu=", round(m3$pars[1],2), 
                                        ", sigma=", round(m3$pars[2],2))),
           col=c(1,2,3),
          lty=c(1,2,3), cex=0.8)
    dev.off()
    
    }
  }

DF_DIST <- DF_DIST %>% filter(numPlants !=0)

write_csv(DF_DIST, "../../output/distribuciones.csv")


DF_MEANS <- DF_TOTAL_AG_SHORT %>%
  group_by(numPlants, porcionCosecha)%>%
  summarise(MEAND = mean(DistanceW, na.rm = TRUE), MODED = DistanceW[Frec == max(Frec)], MEDIAND = median(DistanceW, na.rm= TRUE))

write_csv(DF_MEANS, "../../output/valoresTC.csv")



#######################333
# FIG_PASOS_LOG <- DF_TOTAL_AG_SHORT %>%
#   # filter(Rust == 0.5)%>%  
#   filter(numPlants== 2000 | numPlants== 500| numPlants== 5000)%>%
#   ggplot(aes(x= D_MEAND, y = Frec)) +
#   geom_jitter(aes(alpha=0.7, size= 1.5))+
#   scale_color_manual(values = mycols)+
#   facet_grid(porcionCosecha ~numPlants) +
#   theme_bw() +
#   #geom_segment(aes(x=log(1.5), y=-11, xend= log(1.5), yend=-2), size = 0.2, color= "Black")+
#   theme(text = element_text(size = 25))+
#   theme(strip.background = element_rect(fill = "white"))
# 
#   #labs(x= "ln(Distance Step)", y= "ln(Proportion of Steps)", color= "Rust")



# 
# 
# 
# 
# 
# 
# #DF_TOTAL <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)
# DF_TOTAL <- read.csv("../../data/DF_total_TF.csv", header = TRUE)
# 
# #DF_TOTAL$numPlants <- DF_TOTAL$numPlants /10000
# 
# maxLim <- c(5, 10, 15)
# 
# for (mL in maxLim) { 
# DF_BASE <- DF_TOTAL
# 
# DF_BASE$DistanceW <- sqrt(DF_BASE$DistanceW)
# maxLim <- mL
# 
# DF_BASE$LIM_1.5 <- (DF_BASE$DistanceW>1.5)*1
# DF_BASE$LIM_VAR <- (DF_BASE$DistanceW>maxLim) *1
# DF_BASE$CAT <- DF_BASE$LIM_1.5 +DF_BASE$LIM_VAR  ##describir categorias
# 
# DF_BASE$CAT[DF_BASE$CAT==0] <- "[0-1.5)"
# DF_BASE$CAT[DF_BASE$CAT==1] <- paste("[1.5-", maxLim, ")",sep = "")
# DF_BASE$CAT[DF_BASE$CAT==2] <- paste("[",maxLim,"-inf)",sep = "")
# 
# 
# DF_BASE$contador <-1
# 
# DF_TOTAL_AG <- DF_BASE %>%
#     group_by(HarvestModel, numPlants, numWorkers, Rust, Rep, HarvestTime, SimID, porcionCosecha, CAT) %>%
#     summarise(sumCAT = sum(contador))
# 
# DF_TOTAL_AG$proCAT <- 100 *(DF_TOTAL_AG$sumCAT/(DF_TOTAL_AG$numPlants))  #vuelvo a pasarlo a total por ha para eso.
# 
# DF_TOTAL_AG$porcionCosecha[DF_TOTAL_AG$porcionCosecha =="0.5"] <- "Asynchronic"
# DF_TOTAL_AG$porcionCosecha[DF_TOTAL_AG$porcionCosecha =="1"] <- "Synchronic" 
# 
# DF_TOTAL_AG_R <- DF_TOTAL_AG %>%
#   group_by(HarvestModel, numPlants, numWorkers, Rust, HarvestTime, SimID, porcionCosecha, CAT) %>%
#   summarise(AV_proCAT = mean(proCAT), SD_proCAT = sd(proCAT))
# 
# FIG_PASOS <- DF_TOTAL_AG_R %>%
#     #filter(numPlants== nP)%>%
#     filter(Rust== 0.5)%>%
#     filter(numWorkers== 5)%>%
#     #filter(HarvestModel == "closeness") %>%
#     #filter(HarvestTime == 2)%>%
#     #filter(CAT ==0)%>%
#     ggplot(aes(x= HarvestTime, y = AV_proCAT)) +
#     geom_bar(stat= "identity", aes(fill = as.character(CAT)))+
#     #geom_jitter(size= 1)+
#     #  geom_line()+
#    # scale_fill_manual(values = mycolsBW)+
#     scale_fill_viridis_d()+ 
#     facet_wrap(~porcionCosecha*numPlants, nrow = 2) +
#     theme_bw() +
#     theme(text = element_text(size = 20))+
#     labs(x= "Harvest Time", y= "% Rust-Effective Steps per size", fill= "SizeStep")
#   
# ggsave(FIG_PASOS,filename=paste("../../output/saltosEfectivos/PROP/","plantas", "FigN_pasosDIS", maxLim, ".png", sep=""),  height = 8, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
# 
# }
# ###Lo que falta, correlacionarlo con la cantidad de small steps DEjar esto para pensar entre todo
# 
#   
# #DF_TOTAL_AG_EF <- DF_TOTAL_AG %>%
#  # filter(numPlants== 3000)%>%
#   #filter(Rust== 0.5)
# 
# 
# #DF_DIF<- DF_TOTAL_AG_EF  %>%
#   #esto es para tener la maxima diferencia, pero tambien porque es aqui en donde pasa la cosecha
#  # group_by(Rep, numPlants, numWorkers, HarvestTime, CAT)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
# #  summarise(difPro = -diff(proCAT))  # esto es solo para que despues de leer por condicion, regrese...?
# 
# #FIG_DIF <- DF_DIF %>%
 # ggplot(aes(x= CAT, y = difPro)) +
  #geom_boxplot(aes(fill = as.character(CAT)))+
  #geom_jitter(size= 1)+
  #  geom_line()+
  #scale_fill_manual(values = mycolsBW)+
  #facet_wrap(~HarvestTime, nrow = 1) +
  #geom_segment(aes(x=0.5, y=0, xend= 3.5, yend=0), linewidth = 0.2, color= "DarkRed")+
  #theme_bw() +
  #theme(text = element_text(size = 20))+
  #labs(x= "HarvestingTime", y= "% Rust-Effective Steps per size", fill= "SizeStep")


#ggsave(FIG_DIF,filename=paste("archivosTrabajandose/toyModelHarvest/output/saltosEfectivos/","Fig_DIF", maxLim, ".png", sep=""),  height = 8, width = 14) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
