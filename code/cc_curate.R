  
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


# raw data
myFile <- "data/PowerBiodiv_systematic_review_data_v20241017_BL.xlsx"
myData <- as.data.frame(read_excel(myFile, sheet = "Data"))
myData <- myData[ -1, ] 
myData <- as.data.frame(lapply(myData, function(x) {gsub("\u2010", "-", x)}))
n <- dim(myData)[1]
p <- dim(myData)[2]

# Variable description and encoding
myVarTypes <- as.data.frame(read_excel(myFile, sheet = "Variables"))
myVarTypes$Description <- gsub("\u2010", '-', myVarTypes$Description)
myVarTypes$Number_different_responses <- apply(myData, 2, function(x) {length(unique(x))})


## Start curating data
myDataClean <- myData
rownames(myDataClean) <- myData$V001

# Ensure that quantitative variables are numeric
for (i in which(myVarTypes$Type == "Quantitative")) {
  tmp <- myDataClean[ , i]
  tmp[tmp == "NIL"] <- NA
  myDataClean[ , i] <- as.numeric(tmp)
}

# Create new dummy variables for "Multiple" : this adds a bunch of columns
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

# Update nub of columns
p <- dim(myDataClean)[2]

# Transform dummy and ordinal into numbers
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
myDataClean$V041c <- rowSums(myDataClean[ , c("V041b_West", "V041b_Colo", "V041b_Capi", "V041b_Rura", "V041b_Ethn", "V041b_Weal", "V041b_Gend", "V041b_Gove", "V041b_Diff", "V041b_Scie")])

# Convert ordinal variables V028b V011c V060c V065c (1 to 4)
myDataClean$V028b <- ntile(as.numeric(myDataClean$V028), 4) # this technique may bin identical values in different bins just to keep bin sizes even.
myDataClean$V011c <- ntile(as.numeric(myDataClean$V011b), 4) 
myDataClean$V060c <- ntile(as.numeric(myDataClean$V060b), 4) 
myDataClean$V065c <- ntile(as.numeric(myDataClean$V065b), 4) 


# Imputation of missing data With imputeFAMD, impute missing data within each group separately
myDataNum <- myDataClean[ , unlist(allSelections)] ################ Camille: il y a un pb avec la colonne V090: elle est en double dans Devolution et Knowledge.
# myDataNum contains only numerics, i.e. vars that have been converted as such.

# # je vais l'enlever de Devolution pour voir. cf plus haut dans les allselection   s.
# ### Camille
# unlist(allSelections) %>% duplicated() %>% sum
# unlist(allSelections)[31]
### Camille

# convert numerics to ordered factors (ordinal?)
myDataFac <- as.data.frame(lapply(myDataNum, factor, ordered = TRUE))

# make empty dfs
myDataNumImp <- myDataFacImp <- as.data.frame(matrix(0, nrow = n, ncol = length(unlist(allSelections))))
colnames(myDataNumImp) <- colnames(myDataFacImp) <- unlist(allSelections) ############### ptet corrigé ici ??
for (i in names(allSelections) ) {  # impute by groups separately
  set.seed(1234)
  myDataFacImp[ , allSelections[[i]] ] <- 
    imputeFAMD(myDataFac[, allSelections[[i]]], , ncp = 2)$completeObs %>%
    as.data.frame(.) %>%
    apply(., 2, function(x){ 
      x <- as.character(x) 
      y <- gsub("^V..._", "", x)
    
    
    }) 
    
    factor, ordered = TRUE) %>%
    lapply(., as.character) %>%
          as.data.frame

    
  myDataFacImp[ , allSelections[[i]] ] <- as.data.frame(
    lapply(
      imputeFAMD(
        myDataFac[, allSelections[[i]]], ncp = 2)$completeObs, factor, ordered = TRUE)) # pb for i= "Social" dans les V119 et V120
  myDataNumImp[ , allSelections[[i]] ] <- as.data.frame(imputeFAMD(myDataNum[, allSelections[[i]]], ncp = 2)$completeObs) 
}



myDataFac[, allSelections[[i]]] %>% apply(., 2, as.factor)%>% apply(., 2, as.numeric) == 
  myDataNum[, allSelections[[i]]]



tab <- myDataFacImp[ , allSelections[[i]] ] 