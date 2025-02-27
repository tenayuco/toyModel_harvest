
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

#DF_SAM <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_muestrasPath_complete.csv", header = TRUE)


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
    plot(m2, xlab= "size step (m)",  ylab = "CDF", xlim=c(0.1,110), col= alpha(1, 0.4), main=paste(nP, "Plants/ha", pC))
    lines(m1, col=1, lty= 1, lwd= 2)
    lines(m2, col = 2, lty = 2, lwd= 2)
    lines(m3, col = 3, lty = 3, lwd= 2)
    legend("bottomleft", legend=c(paste("PowerLaw ", "alpha=", round(m1$pars, 2)), 
                                 paste("Exponential ", "lambda=", round(m2$pars, 2)), 
                                 paste("LogNormal ", "mu=", round(m3$pars[1],2), 
                                        ", sigma=", round(m3$pars[2],2))),
           col=c(1,2,3),
          lty=c(1,2,3), cex=1.2)
    dev.off()
    
    }
  }

DF_DIST <- DF_DIST %>% filter(numPlants !=0)

write_csv(DF_DIST, "../../output/distribuciones.csv")


DF_MEANS <- DF_TOTAL_AG_SHORT %>%
  group_by(numPlants, porcionCosecha)%>%
  summarise(MEAND = mean(DistanceW, na.rm = TRUE), MODED = DistanceW[Frec == max(Frec)], MEDIAND = median(DistanceW, na.rm= TRUE))

write_csv(DF_MEANS, "../../output/valoresTC.csv")

