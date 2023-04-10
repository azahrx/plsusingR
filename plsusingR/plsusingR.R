library(lattice)
library(semPLS)

dataequip <- as.data.frame(dataequip)
EQUIPsm <- as.matrix(EQUIPsm)
EQUIPmm <- as.matrix(EQUIPmm)

#saving datafile
save(dataequip, file = "dataequip.rda")
save(EQUIPsm, file = "EQUIPsm.rda")
save(EQUIPmm, file = "EQUIPmm.rda")

#calling datafile
load("dataequip.rda")
load("EQUIPsm.rda")
load("EQUIPmm.rda")

#create model
EQUIP <- plsm(data = dataequip, 
              strucmod = EQUIPsm, 
              measuremod = EQUIPmm)

#relation between indicator in variables
mvpairs(model = EQUIP, 
        data = dataequip, 
        LVs = "Optimism")

#preparing model command
equip <- sempls(model = EQUIP, 
                data = dataequip, 
                wscheme = "centroid")
equip

#path image
pathDiagram(equip, 
            file = "graph_equip", 
            full = TRUE, 
            edge.labels = "both", 
            output.type = "graphics", 
            digits = 2, 
            graphics.fmt = "pdf")

library(DiagrammeR)
grViz("graph_equip.dot")

names(equip)

#measurement model assessment

#convergent validity
equip$outer_loadings

#discriminant validity cross loadings
equip$cross_loadings

#indicator reliability
communality(equip)

#internal consistency reliability
dgrho(equip)

#discriminant validity
plsLoadings(equip)

#total effects
equip$total_effects

#path coefficients R2
pC <- pathCoeff(equip)
print(pC, abbreviate = TRUE, minlength = 3)

#total effects
tE <- totalEffects(equip)
print(tE, abbreviate = TRUE, minlength = 3)

#outer weights
plsWeights(equip)

#discriminant validity loadings
plsLoadings(equip)

#kernel density for model adequacy
densityplot(equip, use = "residuals")

#structural model assessment
rSquared(equip)

redundancy(equip)

gof(equip)

#bootstrapping
set.seed(123)
equipBoot <- bootsempls(equip, 
                        nboot = 500, 
                        start = "ones", 
                        verbose = FALSE)
equipBoot

equipBootsummary <- summary(equipBoot, 
                            type = "bca", 
                            level = 0.95)
equipBootsummary
