# setwd("D:/a-Mes Documents/a-Current/PowerBiodiv/Systematic review/R")

list.of.packages <- c("ggplot2", "readxl", "plyr", "Hmisc", "psych", "polycor", "tidyverse", "lavaan", "semPlot", "lavaanPlot", "semptools", "GPArotation", "missMDA")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(ggplot2)
library(readxl) 
library(plyr) 
# library(Hmisc)
library(psych)
library(stringr)
library(polycor)
library(dplyr)
library(car)
library(FactoMineR) 
library(psych)     
library(stringr)
library(lavaan)
library(semPlot)
library(lavaanPlot)
library(semptools)
library(GPArotation)
library(missMDA)
library(ade4)
library(magrittr)



################# CC
load(file = here::here("data/cc.RData"))

myFile <- "data/PowerBiodiv_systematic_review_data_v20241017_BL.xlsx"
myData <- as.data.frame(read_excel(myFile, sheet = "Data"))
myData <- myData[ -1, ] 
myData <- as.data.frame(lapply(myData, function(x) {gsub("\u2010", "-", x)}))

myVarTypes <- as.data.frame(read_excel(myFile, sheet = "Variables"))
myVarTypes$Description <- gsub("\u2010", '-', myVarTypes$Description)
myVarTypes$Number_different_responses <- apply(myData, 2, function(x) {length(unique(x))})


myGroup = "Social"



################# CC end

#################
#### My functions
wrapper <- function(x, ...) { paste(strwrap(x, ...), collapse = "\n") }
createModel <- function(myGroup) {
  if (is.list(allSelectionsBis[[myGroup]])) {
    SEM1 <- "\n"
    for (j in 1:length(allSelectionsBis[[myGroup]])) {
      if (length(allSelectionsBis[[myGroup]][[j]]) > 1) {
        SEM1 <- paste0(SEM1, allSelectionsBisNames[[myGroup]][j], " =~ ", paste0(allSelectionsBis[[myGroup]][[j]], collapse =  " + "), "\n")
      } else {
        SEM1 <- paste0(SEM1, allSelectionsBisNames[[myGroup]][j], " =~ 1 * ", allSelectionsBis[[myGroup]][[j]], "\n")
      }
    }
  } else {
    SEM1 <- paste0(paste0(myGroup, " =~ ", paste0(allSelectionsBis[[myGroup]], collapse =  " + "), collapse = "\n"), "\n")
  }
  return(SEM1)
}


#################
####### Final list of variables
# They are grouped by the initial latent variables we wanted to create (e.g., "Devolution").  
# The "allSelections" list stores all observed variables that we initially included in the group.
# Within each group, the Exploratory Factor Analysis identified sometime more than one latent variable (e.g., "Partic_Cont" and "Infl_Asym" under "Devolution").
# The "allSelectionsBisNames" list stores the names of these final latent variables. 
# The "allSelectionsBis" list stores the observed variables related to the final latent variables.
allSelections <- allSelectionsBis <- allSelectionsBisNames <- list()
allSelections[["Devolution"]] <- c("V019", "V043", "V053", "V054", "V055", "V057", "V058", "V059",  "V095", "V110", "V111", "V112") # CC: j'ai enlevé "V090" pour voir. Mais ça ne devrait rien changer vu qu'elle n'est pas retenue dans le allSelectionbis
allSelectionsBis[["Devolution"]] <- list(c("V043", "V053", "V054", "V110"), c("V111", "V112"))
allSelectionsBisNames[["Devolution"]] <- c("Partic_Cont", "Infl_Asym")
allSelections[["Inclusive"]] <- c("V048", "V049", "V050", "V051", "V052", "V071", "V074")
allSelectionsBis[["Inclusive"]] <- c("V048", "V049", "V051", "V052")
allSelections[["Constructive"]] <- c("V070", "V085", "V086", "V093", "V094",  "V096")
allSelectionsBis[["Constructive"]] <- c("V070", "V093", "V094",  "V096")
allSelections[["Knowledge"]] <- c("V047", "V073", "V075", "V089", "V090", "V091", "V092")
allSelectionsBis[["Knowledge"]] <- list(c("V047", "V091", "V092"), c("V089", "V090", "V075"))
allSelectionsBisNames[["Knowledge"]] <- c("Int_Know", "Ext_Know")
allSelections[["Facilitation"]] <- c("V045","V046", "V081", "V082", "V083")
allSelectionsBis[["Facilitation"]] <- c("V045","V046", "V082", "V083")
allSelectionsBis[["Facilitation"]] <- list(c("V045", "V083"), c("V046",  "V082"))
allSelectionsBisNames[["Facilitation"]] <- c("Facil_capac", "Facil_Power")
allSelections[["Context"]] <- c("V027","V029", "V030", "V031", "V037", "V038", "V039", "V044", "V028b", "V040", "V041c")
allSelectionsBis[["Context"]] <- list(c("V037", "V038"), c("V029", "V030", "V027"), "V028b", "V040")
allSelectionsBisNames[["Context"]] <- c("Tensions", "Trust", "Civic", "Hidden")
allSelections[["Goals"]] <- c("V101", "V102", "V103", "V104", "V105", "V106", "V107", "V109")
allSelectionsBis[["Goals"]] <- list(c("V102", "V105", "V106",  "V109"), c("V101", "V103", "V104", "V107"))
allSelectionsBisNames[["Goals"]] <- c("Organiz_O", "Cogniti_O")
allSelections[["Social"]] <- c("V114", "V115", "V116", "V117", "V118", "V119", "V120")
allSelectionsBis[["Social"]] <- c("V114", "V115", "V116", "V117", "V118", "V119", "V120")
allSelections[["Environmental"]] <- c("V121b", "V122", "V123", "V124", "V125")
allSelectionsBis[["Environmental"]] <- c("V121b", "V122", "V123", "V124", "V125")
allSelections[["Process"]] <- c("V014" , "V015" , "V020" , "V021" , "V128b" , "V011c" , "V060c" , "V065c" )
allSelectionsBis[["Process"]] <- list(c("V014" , "V015", "V020", "V128b"), "V065c", "V011c")
allSelectionsBisNames[["Process"]] <- c("Level", "Magnitude", "Duration")



final_names <- allSelectionsBis
final_names2 <- lapply(1:length(final_names), function(x){
  latvar <- names(final_names[x])
  names(final_names[[x]]) <- allSelectionsBisNames[[latvar]]
  return(final_names[x])
})


cc_names <- list()
cc_names[["Devolution"]] <- list(
  f1=c( "V019", "V043" ,"V053" ,"V054" ,"V095"),
  f2 = c("V053","V057","V059", "V110", "V112")
)
cc_names[["Inclusive"]] <- list(
  f1=c( "V048", "V049" ,"V050" ,"V051")
)
# this one doesn't really give anything out. Perhaps use obs variables direclty rather tah latent vars ?
cc_names[["Constructive"]] <- list(
  f1=c( "V070", "V093" )
)

cc_names[["Knowledge"]] <- list(
  f1=c("V047", "V089", "V090", "V091", "V092")
)
cc_names[["Facilitation"]] <- list(
  f1=c("V046", "V081", "V082", "V083")
)
cc_names[["Context"]] <- list(
  f1=c("V039", "V040", "V041c", "V031"),
  f2=c("V029", "V044", "V030")
)
cc_names[["Goals"]] <- list(
  f1=c("V101", "V102", "V103", "V106")
)
cc_names[["Social"]] <- list(
  f1=c("V114", "V115", "V117", "V119")
)
# obs variables are too correlated with one another. 
cc_names[["Environmental"]] <- list(
  f1=c("124")
)

cc_names[["Process"]] <- list(
  f1=c("V015", "V021", "V128b", "V060c"),
  f2=c("V114", "V020", "V011c", "V065c")
)
#################
#### Load and curate data, impute missing data
######## Note: I keep this code here for inforamation but we don't need to rerun it: the curated data is saved as "myDataFacImp.RData"
optionCurate <- TRUE
if (optionCurate) {
  myFile <- "data/PowerBiodiv_systematic_review_data_v20241017_BL.xlsx"
  myData <- as.data.frame(read_excel(myFile, sheet = "Data"))
  myData <- myData[ -1, ] 
  myData <- as.data.frame(lapply(myData, function(x) {gsub("\u2010", "-", x)}))
  n <- dim(myData)[1]
  p <- dim(myData)[2]
  myVarTypes <- as.data.frame(read_excel(myFile, sheet = "Variables"))
  myVarTypes$Description <- gsub("\u2010", '-', myVarTypes$Description)
  myVarTypes$Number_different_responses <- apply(myData, 2, function(x) {length(unique(x))})
  ## Start curating data
  myDataClean <- myData
  rownames(myDataClean) <- myData$V001
  ## Ensure that quantitative variables are numeric
  for (i in which(myVarTypes$Type == "Quantitative")) {
    tmp <- myDataClean[ , i]
    tmp[tmp == "NIL"] <- NA
    myDataClean[ , i] <- as.numeric(tmp)
  }
  ## Create new dummy variables for "Multiple"
  for (i in which(myVarTypes$Type == "Multiple")) {
    resp.split <- strsplit(myData[,i], ", ")
    lev <- unique(unlist(resp.split))
    newDummies <- as.data.frame(do.call(rbind, lapply(resp.split, function(x) table(factor(x, levels=lev)))))
    newDummies$NIL <- NULL
    names(newDummies) <- paste0(myVarTypes$VarID[i], "_", substr(names(newDummies), 1, 4))
    myDataClean[ , names(newDummies)] <- NULL # remove the dummies in case they had been already created
    myDataClean <- cbind(myDataClean, newDummies)
    myVarTypes <- myVarTypes[!(myVarTypes$VarID %in% names(newDummies)), ]
    myVarTypes <- rbind(myVarTypes, 
                        data.frame(VarID = names(newDummies), 
                                   Description = paste0(myVarTypes$VarID[i], " takes the value: ",  lev[lev != "NIL"]),
                                   Number_different_responses = 2,
                                   Type = "Dummy"    ))
  }
  p <- dim(myDataClean)[2]
  ## Transform dummy and ordinal into numbers
  intoNumbers <- function(x) {
    y <- gsub("\u2010", '-', x)
    y <- mapvalues(substr(y,1,2), from = c("No", "Ye", "Un", "NI", "-9", "\u20109"), to = c(0, 1, NA, NA, NA, NA), warn_missing = FALSE) # in plyr
    y <- gsub("=", "", y)
    y <- as.numeric(y)
    return(y)
  }
  for (i in 1:p) {
    if (myVarTypes$Type[i] %in% c("Dummy", "Ordinal")) {
      myDataClean[ , i] <- intoNumbers(myDataClean[ , i])
    }
  }
  # create V041c number of Invisible and systematic power mentioned
  myDataClean$V041c <- apply(myDataClean[ , c("V041b_West", "V041b_Colo", "V041b_Capi", "V041b_Rura", "V041b_Ethn", "V041b_Weal", "V041b_Gend", "V041b_Gove", "V041b_Diff", "V041b_Scie")], 1, sum)
  # Convert ordinal variables V028b V011c V060c V065c (1 to 4)
  myDataClean$V028b <- ntile(as.numeric(myDataClean$V028), 4) 
  myDataClean$V011c <- ntile(as.numeric(myDataClean$V011b), 4) 
  myDataClean$V060c <- ntile(as.numeric(myDataClean$V060b), 4) 
  myDataClean$V065c <- ntile(as.numeric(myDataClean$V065b), 4) 
  # Imputation of missing data With imputeFAMD, impute missing data within each group separately
  myDataNum <- myDataClean[ , unlist(allSelections)] ################ Camille: il y a un pb avec la colonne V090: elle est en double dans Devolution et Knowledge.
  # autre pb: il y a des valeurs négatives, par ex dans la colonne V029 (qui est la 31e colonne)


  # # je vais l'enlever de Devolution pour voir. cf plus haut dans les allselection   s.
  # ### Camille
  # unlist(allSelections) %>% duplicated() %>% sum
  # unlist(allSelections)[31]
  ### Camille

  myDataFac <- as.data.frame(lapply(myDataNum, factor, ordered = TRUE))
  myDataNumImp <- myDataFacImp <- as.data.frame(matrix(0, nrow = n, ncol = length(unlist(allSelections))))
  colnames(myDataNumImp) <- colnames(myDataFacImp) <- unlist(allSelections) ############### ptet corrigé ici ??
  for (i in names(allSelections) ) {  # impute by groups separately
    set.seed(1234)

    # impute from ordered factors:
    myDataFacImp[ , allSelections[[i]] ] <- as.data.frame(lapply(imputeFAMD(myDataFac[, allSelections[[i]]], ncp = 2)$completeObs, factor, ordered = TRUE)) # pb for i= "Social" dans les V119 et V120

    # impute from numeric
    myDataNumImp[ , allSelections[[i]] ] <- as.data.frame(imputeFAMD(myDataNum[, allSelections[[i]]], ncp = 2)$completeObs) 
  }
}


#################
##########  Load curated data

# load(here::here("data/myDataFacImp.RData"))
load(file = "data/myDataFacImp.rdata")
load(file = here::here("data/cc.rdata"))


# save(myDataFacImp, allSelections, allSelectionsBis, allSelectionsBisNames, myFile, createModel, intoNumbers, wrapper, file="data/cc.RData" )


# ""Devolution"    "Inclusive"     "Constructive"  "Knowledge"     "Facilitation"  "Context"       "Goals"         "Social"       "Environmental" "Process" 
# Social : ok. Leave as is.

myGroup = "Process"

## EFA with polychoric FA (iterative process: change the number of factors and observe results: do they make sense?)
# groupvars <-  allSelections[[myGroup]][-grep("V111", allSelections[[myGroup]])]
# myDataFactor <- myDataFacImp[ , groupvars]
myDataFactor <- myDataFacImp[ , allSelections[[myGroup]]]
myDataFactor <- myDataFacImp[ , unlist(allSelectionsBis[[myGroup]])]

myCor <- hetcor(myDataFactor)   # polychoric corr matrix
cc_cor <- lavCor(myDataFactor, ordered = unlist(allSelections[[myGroup]]) )
lavaan_EFA <- lavaan::efa(data = myDataFactor,
  ov.names = unlist(allSelections[[myGroup]]) ,
  nfactors = 2, 
  rotation= "varimax",
  output="lavaan"
)
# to find out how many factors are needed: 2. All good.
fa.parallel(myCor$correlations, fm='minres', fa='fa')

# CC: this example shows the same warnings without much concern: https://rpubs.com/Pun_/Exploratory_factor_Analysis
# also read this for later: https://groups.google.com/g/lavaan/c/tPW4URKfhOE
# maybe this one too on EFA: https://wnarifin.github.io/lecture/mstat/efa_medstat_practical.html


# CC: try to use lavCor instead of fa():
cc_cor <- lavCor(myDataFactor, ordered =  unlist(allSelections[[myGroup]]) )
diag(cc_cor) <- 0
cc_cor_mat <- as.data.frame(cc_cor) %>% as.matrix

cc_cor_mat %>% abs() %>% apply(., 1, max)

# V074 et V071 sont corrélées à 0.875 : exit V094
# V071 et V050 sont corrélées à 0.0.9 : exit V070
# V052 et V049 sont corrélées à 0.8: exit V052 

cc_selection <- c( "V014" , "V015" , "V020",  "V021"  ,"V128b", "V011c", "V060c", "V065c" )
myDataFactor <- myDataFacImp[ , cc_selection]
myCor <- hetcor(myDataFactor)  
cc_cor <- lavCor(myDataFactor, ordered =  cc_selection )

diag(cc_cor) <- 1
cc_cor_mat <- as.data.frame(cc_cor) %>% as.matrix

cc_cor_mat %>% abs() %>% apply(., 1, max)

image(cc_cor_mat)

n=82
myEFA <- psych::fa(r=myCor$correlations, nfactors = 2, n.obs=n, rotate = "varimax", fm="ml")
cc_EFA <- psych::fa(r=cc_cor_mat, nfactors = 3, n.obs=n, rotate = "varimax", fm="ml")


lavaan_EFA <- lavaan::efa(data = myDataFactor,
  ov.names = cc_selection,
  nfactors = 2, 
  rotation= "varimax",
  output="lavaan"
)
summary(lavaan_EFA)
fitMeasures(lavaan_EFA, fit.measures = c("cfi", "rmsea", "srmr", "tli"))
lavaanPlot(lavaan_EFA,  coefs = TRUE)
inspect(lavaan_EFA,what="std")$lambda



fa.diagram(myEFA, cut = 0.2, digits = 2, simple = TRUE,)
summary(myEFA)
print(myEFA$loadings)




## CFA
myDataFactor <- myDataFacImp[ , unlist(allSelectionsBis[[myGroup]])]
myDataFactor <- myDataFacImp[ , unlist(cc_names[[myGroup]])]
SEM1 <- createModel(myGroup)
SEM1 <- c("Process =~ V015 + V021 + V128b + V060c")
fit1 <- lavaan::cfa(SEM1, data = myDataFacImp, sample.nobs = n, std.lv = TRUE, ordered = TRUE)
summary(fit1, standardized = TRUE) # we want the pvalue to be > 0.05 because the model has to be similar to expectations, rather than different
# look at the 1st table of latent variables. Not the thresholds.
myFitMeasures <- fitMeasures(fit1, c("cfi", "rmsea", "srmr", "tli")) # not bad !
myFit <- paste(paste(c("CFI", "RMSEA", "SRMR"), round(c(myFitMeasures), 2), sep = "="), collapse = ", ")
if (!is.list(allSelectionsBis[[myGroup]])) {
  # myCronbach <- psych::alpha(hetcor(myDataFactor)$correlations, n.obs = n, check.keys=TRUE)  
  # CC: 
  cc_Cronbach <- psych::alpha(lavaan::lavCor(myDataFactor))  


  myFit <- paste(myFit, "\n", "Cronbach reliability=", round(cc_Cronbach$total$raw_alpha, 2))
}
plotFit1 <- semPaths(fit1,  what = "path", whatLabels = "stand", rotation = 2, intercepts = FALSE, thresholds = FALSE,
                     residuals = TRUE,
                     sizeMan = 7, edge.label.cex = 1, nCharNodes = 0, nCharEdges = 0, DoNotPlot = TRUE, title = FALSE)
plotFit1b <- mark_sig(plotFit1, fit1)
plot(plotFit1b)
par(mar = c(5, 1, 1, 1))
title(main = myGroup, sub = myFit,
      col.main= "blue", col.sub = "red", cex.sub = 0.95)




## Diagnostics 
covMat <- lavInspect(fit1, "cov.lv") 
paste0(colnames(covMat)[which(abs(covMat)>1, arr.ind = TRUE)[,1]], "-", colnames(covMat)[which(abs(covMat)>1, arr.ind = TRUE)[,2]])
# Residual Variance of Observed Indicators: Check values < 0
round(diag(lavInspect(fit1, "est")$theta) , 2)
## modification index (mi in the output) is the expected decrease in the model xhi2. A larger number would lead to a better fit if that path were included
## diagnoMI <- modindices(fit1) 
## diagnoMI[order(diagnoMI$mi, decreasing = TRUE)[1:20], ]

# Testing for Multicollinearity VIF = 1: No multicollinearity, 1-5: Low to moderate multicollinearity, > 5: strong multicollinearity, > 10: very strong multicollinearity
testMulticol <- lm(reformulate(paste0(names(myDataFactor), collapse = "+"), "y")  , 
                   data = cbind(data.frame(y = runif(n)), lapply(myDataFactor, function(x){ as.numeric(str_sub(x, -1, -1)) }  )))
sort(round(vif(testMulticol),2)) # should be 1 < x < 5 # all good
# anova(fitOld , fit1)
vcov(fit1)
#  det(vcov(fit1)) 
# round(hetcor(myDataFactor)$correlations, 2)
round(lavCor(myDataFactor), 2)

round(rcorr(as.matrix(myDataNumImp[ , names(myDataFactor)]), type="spearman")$r, 2)


#################
##### Overall SEM
# Note: this part is under construction. It is for testing options rather than analyzing data.
# Lou and Bruno need to discuss how to build a simple model first.
# The measurement model corresponds to the CFA findings.
# As the path model is non-sense at this stage, it is shown as comment.

for (i in names(allSelections)) {
  cat(createModel(i))
}

SEM1 <- '
  ## Measurement model

Partic_Cont =~ V043 + V053 + V054 + V110
Infl_Asym =~ V111 + V112
Inclusive =~ V048 + V049 + V051 + V052
Constructive =~ V070 + V093 + V094 + V096

Int_Know =~ V047 + V091 + V092
Ext_Know =~ V089 + V090 + V075

Facil_capac =~ V045 + V083
Facil_Power =~ V046 + V082

Tensions =~ V037 + V038
Trust =~ V029 + V030 + V027
Civic =~ 1 * V028b
Hidden =~ 1 * V040

Organiz_O =~ V102 + V105 + V106 + V109
Cogniti_O =~ V101 + V103 + V104 + V107
Social =~ V114 + V115 + V116 + V117 + V118 + V119 + V120
Environmental =~ V121b + V122 + V123 + V124 + V125

Level =~ V014 + V015 + V020 + V128b
Magnitude =~ 1 * V065c
Duration =~ 1 * V011c

  ## Structural model [JUST AS EXAMPLE]
  # path: direct effect
  # Facil_capac ~ Tensions + Trust + Civic + Hidden + Level + Magnitude + Duration
  # Int_Know ~ Tensions + Trust + Civic + Hidden + Level + Magnitude + Duration
  # Ext_Know ~ Tensions + Trust + Civic + Hidden + Level + Magnitude + Duration
 
  # path: devolution as mediator
  # Devolution ~ b * Facilitation
  # Social ~ c * Devolution
  # Social ~  Devolution + Inclusive + Constructive + Knowledge
  
  ## Parameters
  # direct := a
  # indirect :=  b*c 
  # total := a + (b*c)
  
  ## Covariance/Variance structure 
  # Facil_capac ~~ 1 * Facil_capac
  # Int_Know ~~ 1 * Int_Know
  # Ext_Know ~~ 1 * Ext_Know
'   

# simpler model:
SEM1 <- '
  ## Measurement model

Partic_Cont =~ V043 + V053 + V054 + V110
Infl_Asym =~ V111 + V112
Inclusive =~ V048 + V049 + V051 
Deliberation =~ V070 + V093 + V094 + V096
Facil_Power =~ V046 + V082
Social =~ V114 + V115 + V116 + V117 + V118 + V119 + V120

  ## Structural model 
  # path: direct effect
  Social ~ Partic_Cont + Infl_Asym + Inclusive + Deliberation + Facil_Power
'



fit1 <- lavaan::sem(SEM1, data = myDataFacImp, ordered = colnames(myDataFacImp),
            meanstructure = FALSE,  fixed.x = FALSE, std.lv = TRUE)
# CC: 
 fit1_resids <- resid(fit1, type="cor")$cov
range(fit1_resids)
myDataFactor <- myDataFacImp[ , colnames(residuals(fit1)$cov)]
summary(fit1, standardized = TRUE)
# need to look at latent variables and especially the regressions. 

myFitMeasures <- fitMeasures(fit1, c("cfi", "rmsea", "srmr"))
myFit <- paste(paste(c("CFI", "RMSEA", "SRMR"), round(c(myFitMeasures), 2), sep = "="), collapse = ", ")
plotFit1 <- semPaths(fit1,  what = "path", whatLabels = "stand", 
                     rotation = 1, intercepts = FALSE, thresholds = FALSE,
                     sizeMan = 7, edge.label.cex = 1, nCharNodes = 0, nCharEdges = 0, 
                     DoNotPlot = TRUE, title = FALSE, layout = "tree", exoVar = FALSE) # "circle2" exoVar = FALSE, exoCov = FALSE,
plotFit1b <- mark_sig(plotFit1, fit1)
plot(plotFit1b)
par(mar = c(5, 1, 1, 1))
title(main = myFit, col.main= "red", cex.sub = 0.95)

# besoin d'aide ici pour comprendre les messages d'erreur.


## CC playing around

SEM_cc <- '
  ## Measurement model

Partic_Cont =~ V043 + V053 + V054 
Infl_Asym =~ V111 + V112
Social =~ V114 + V115 + V116 

  ## Structural model 
  # path: direct effect
  Social ~ Partic_Cont + Infl_Asym 
'

fit_cc <- lavaan::sem(SEM_cc, data = myDataFacImp, ordered = colnames(myDataFacImp),
            meanstructure = FALSE,  fixed.x = FALSE, std.lv = TRUE)
# CC: 
 fit_cc_resids <- resid(fit_cc, type="cor")$cov
range(fit_cc_resids)
myDataFactor <- myDataFacImp[ , colnames(residuals(fit_cc)$cov)]
summary(fit_cc, standardized = TRUE)
# need to look at latent variables and especially the regressions. 

myFitMeasures <- fitMeasures(fit_cc, c("cfi", "rmsea", "srmr"))
myFit <- paste(paste(c("CFI", "RMSEA", "SRMR"), round(c(myFitMeasures), 2), sep = "="), collapse = ", ")
plotFit1 <- semPaths(fit1,  what = "path", whatLabels = "stand", 
                     rotation = 1, intercepts = FALSE, thresholds = FALSE,
                     sizeMan = 7, edge.label.cex = 1, nCharNodes = 0, nCharEdges = 0, 
                     DoNotPlot = TRUE, title = FALSE, layout = "tree", exoVar = FALSE) # "circle2" exoVar = FALSE, exoCov = FALSE,
plotFit1b <- mark_sig(plotFit1, fit1)
plot(plotFit1b)
par(mar = c(5, 1, 1, 1))
title(main = myFit, col.main= "red", cex.sub = 0.95)

# besoin d'aide ici pour comprendre les messages d'erreur.















# plot without the measurement part
# Note: try also a different layout with "circle2", exoVar = FALSE
fit2 <- semptools::drop_nodes(object = semPlotModel(fit1),  nodes = names(myDataFactor))
plotFitNoMeasurement <- semPaths(fit2,  what = "path", whatLabels = "stand", 
                                 rotation = 2, intercepts = FALSE, thresholds = FALSE,
                                 sizeMan = 7, edge.label.cex = 1, nCharNodes = 0, nCharEdges = 0, 
                                 title = FALSE, layout = "tree", exoVar = TRUE, exoCov = FALSE) 
plotFitNoMeasurementb <- mark_sig(plotFitNoMeasurement, fit1)
plot(plotFitNoMeasurementb)
par(mar = c(5, 1, 1, 1))
title(main = myFit, col.main= "red", cex.sub = 0.95)

# plot without the measurement part and only with paths with absolute estimate above a threshold
estThreshold <- 0.3
fit2 <- semptools::drop_nodes(object = semPlotModel(fit1),  nodes = names(myDataFactor))
original_Pars <- fit2@Pars
new_Pars <- original_Pars[abs(original_Pars$est) > estThreshold, ]
fit2@Pars <- new_Pars
plotFitNoMeasurement <- semPaths(fit2,  what = "path", whatLabels = "stand", 
                                 bifactor = "g",
                                 rotation = 2, intercepts = FALSE, thresholds = FALSE,
                                 sizeMan = 7, edge.label.cex = 1, nCharNodes = 0, nCharEdges = 0, 
                                 title = FALSE, layout = "tree2", exoVar = TRUE, exoCov = FALSE)
plotFitNoMeasurementb <- mark_sig(plotFitNoMeasurement, fit1)
plot(plotFitNoMeasurementb)
par(mar = c(5, 1, 1, 1))
title(main = myFit, col.main= "red", cex.sub = 0.95)


# lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Times"), 
#            edge_options = list(color = "grey"), 
#            coefs = TRUE, sig = 0.10, stand = TRUE, stars = c("regress", "latent"), covs = TRUE)
# lavTestWald(fit1, "a == c")




 