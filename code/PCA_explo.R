## PCA, PCoA, MFA etc.
## 
data(geomorphology)
res <- FAMD(geomorphology)
summary(res)

## Graphical interface
require(Factoshiny)
res <- Factoshiny(geomorphology)

### with missing values
require(missMDA)
data(ozone)
res.impute <- imputeFAMD(ozone, ncp=3) 
res.afdm <- FAMD(ozone,tab.disj=res.impute$tab.disj) 

df <- xlsx::read.xlsx("../Without comment_ cleaned _PowerBiodiv systematic review codebook _VF_CC.xlsx", sheet=1)



# Pour le Structural Equation Modelling, cette première analyse met l’accent sur la partie « measurement model » car le « path model » viendra ensuite. 
# Nous avons voulu créer des variables latentes à partir des variables mesurées. 

# Nous avons utilisé une EFA pour observer nos variables mesurées et voir ce qu’elles disaient (« An exploratory factor analysis aims at exploring the relationships among the variables and does not have an a priori fixed number of factors. You may have a general idea about what you think you will find, but you have not yet settled on a specific hypothesis”).
# 
# Une fois que les variables étaient sélectionnées car elles nous informaient sur des variables latentes (théoriquement et statistiquement), nous avons testé le modele de mesure avec une CFA (« A confirmatory factor analysis assumes that you enter the factor analysis with a firm idea about the number of factors you will encounter, and about which variables will most likely load onto each factor”).
# 
# Pour le Polychoric Factor Analysis, c’est une analyse factorielle qui utilise une matrice de corrélations polychoric (pertinentes pour des données ordinales). Ici, j’ai utilisé hetcor pour les corrélations polychoriques (dans le package polycor). Pour l’analyse factorielle, on peut utiliser deux options (mais la deuxième bugge si les variables ordinales n’ont pas toutes le même nombre de valeurs possibles) :
#   •	psych::fa(polycor::hetcor(myDataFactor), …)
#   •	psych::fa(myDataFactor , cor="poly", …)


?psych::fa

?polycor::hetcor




