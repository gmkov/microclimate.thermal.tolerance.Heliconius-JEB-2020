### Montejo-Kovacevich, G., Martin, S.H., Meier, J.I., Bacquet, C.N., Monllor, M., Jiggins, C.D. and Nadeau, N.J., 2020. ###
### Microclimate buffering and thermal tolerance across elevations in a tropical butterfly. ###
### Journal of Experimental Biology ######
##### packages ######

rm(list=ls())
dev.off()
library(dplyr)
library(tidyr)
library(reshape2)
library(vegan)
library(grid)
library(cowplot)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(lemon)
library(multcompView)
library(pwr)
library(lme4)
library(lmerTest)
library(egg)
library(broom)
library(MuMIn)
library(piecewiseSEM)
library(rr2)
library(stargazer)
library(rptR)
library(hms)
library(nlme)
library(MASS)
library(sjstats)


###################### 0. data ######
setwd("...")
coll <- read.csv("microclimate.thermal.tolerance.Heliconius-JEB-2020/1.data/cg.coll.reach.adult.csv")
ther <- read.csv("microclimate.thermal.tolerance.Heliconius-JEB-2020/1.data/ther.ALL.csv")
summ.localities <- read.csv("microclimate.thermal.tolerance.Heliconius-JEB-2020/1.data/summ.localities.csv")

ther$altitude <- as.numeric(as.character(ther$altitude))
coll$adult.mass <- as.numeric(as.character(coll$adult.mass))
coll$pupa.clean.mass <- as.numeric(as.character(coll$pupa.clean.mass ))

# convert minss to hms
coll$mins.40.to.ko <- as.hms(as.character(coll$time.40.to.ko))
coll$mins.to.40 <- as.hms(as.character(coll$time.to.40))
coll$mins.ko.total <- as.hms(as.character(coll$time.ko.total))

ther$mins.40.to.ko <- as.hms(as.character(ther$time.40.to.ko))
ther$mins.to.40 <- as.hms(as.character(ther$time.to.40))
ther$mins.ko.total <- as.hms(as.character(ther$time.ko.total))
ther$temp.at.ko <- as.numeric(sub(",", ".", ther$temp.at.ko))

coll$mins.40.to.ko <- as.double.difftime(coll$mins.40.to.ko)/60
coll$mins.to.40 <- as.double.difftime(coll$mins.to.40)/60
coll$mins.ko.total <- as.double.difftime(coll$mins.ko.total)/60

ther$mins.40.to.ko <- as.double.difftime(ther$mins.40.to.ko)/60
ther$mins.to.40 <- as.double.difftime(ther$mins.to.40)/60
ther$mins.ko.total <- as.double.difftime(ther$mins.ko.total)/60

ther$egg.no.per.brood <- coll$egg.no.per.brood[match(ther$cam_code, coll$cam.id)]
ther$adult.mass <- coll$adult.mass[match(ther$cam_code, coll$cam.id)]
ther$pupa.clean.mass <- coll$pupa.clean.mass[match(ther$cam_code, coll$cam.id)]
ther$dev.time.larva.to.pupa <- coll$dev.time.larva.to.pupa[match(ther$cam_code, coll$cam.id)]
ther$dev.time.larva.to.adult <- coll$dev.time.larva.to.adult[match(ther$cam_code, coll$cam.id)]
ther$dev.time.pupa.to.adult <- coll$dev.time.pupa.to.adult[match(ther$cam_code, coll$cam.id)]

# make mid as low
ther$type.alt1 <- if_else(ther$altitude>700, "high", "low")
coll$type.alt1 <- if_else(coll$mother.alt>700, "high", "low")

#### subset #####

######### era for TT
era.tt <- subset(ther, species=="erato"&type.alt1!=""&exp.success=="yes") #402-316/329, 86/73 above 41
era.tt.cg <- subset(ther, species=="erato"&type.alt1!=""&exp.success=="yes"&type.reared=="reared")
era.tt.wild <- subset(ther, species=="erato"&type.alt1!=""&exp.success=="yes"&type.reared=="wild")

######### coll era for TT
# coll era data
coll.era.tt <- subset(coll, exp.success=="yes") #203

#### wild subset #####
# subset by number of indivs, remove species with less than 5 indivs,n=14
ther.wild <- subset(ther, type.reared=="wild")
sp.no <- dplyr::summarise(group_by(ther.wild, species, type.alt1),
                          mean.alt=mean(altitude),
                          n=n()); sp.more.5 <- subset(sp.no, n>4)


ther.wild <- subset(ther.wild, paste(species, type.alt1) %in% paste(sp.more.5$species, sp.more.5$type.alt1))

# subset by exp success
ther.wild.tt <- subset(ther.wild, type.reared=="wild"& exp.success=="yes")


## color prep
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sp.ther.cols <- c("#999999","#999999","#999999","#E69F00","#999999","#56B4E9", "#009E73","#999999","#999999")
sp.ther.cols <- c("#E69F00","#CC79A7","#56B4E9", "#009E73","#999999","#999999","#999999","#999999","#999999","#999999","#999999","#999999")

era.cols <- c("#E69F00","#D55E00")

#### functions #### 
# capitalize facet wrap titles
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string}

# change facet wrap titles
alt_names <- list('high'="Highlands", 'low'="Lowlands")
alt_labeller <- function(variable,value){
  return(alt_names[value])}

r.squared.lme <- function(mdl){
  # Get design matrix of fixed effects from model
  Fmat <- model.matrix(eval(mdl$call$fixed)[-2], mdl$data)
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(nlme::fixef(mdl) %*% t(Fmat)))
  # Get variance of random effects by extracting variance components
  VarRand <- sum(suppressWarnings(as.numeric(nlme::VarCorr(mdl)
                                             [rownames(nlme::VarCorr(mdl)) != "Residual",
                                               1])), na.rm=T)
  # Get residual variance
  VarResid <- as.numeric(nlme::VarCorr(mdl)[rownames(nlme::VarCorr(mdl))=="Residual", 1])
  # Call the internal function to do the pseudo r-squared calculations
  .rsquared.glmm(VarF, VarRand, VarResid, family = "gaussian", link = "identity",
                 mdl.aic = AIC(update(mdl, method="ML")),
                 mdl.class = class(mdl))
}

rsquared.glmm <- function(modlist) {
  # Iterate over each model in the list
  do.call(rbind, lapply(modlist, r.squared))
}

.rsquared.glmm <- function(varF, varRand, varResid = NULL, family, link,
                           mdl.aic, mdl.class, null.fixef = NULL){
  if(family == "gaussian"){
    # Only works with identity link
    if(link != "identity")
      family_link.stop(family, link)
    # Calculate marginal R-squared (fixed effects/total variance)
    Rm <- varF/(varF+varRand+varResid)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varResid)
  }
  else if(family == "binomial"){
    # Get the distribution-specific variance
    if(link == "logit")
      varDist <- (pi^2)/3
    else if(link == "probit")
      varDist <- 1
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist)
  }
  else if(family == "poisson"){
    # Get the distribution-specific variance
    if(link == "log")
      varDist <- log(1+1/exp(null.fixef))
    else if(link == "sqrt")
      varDist <- 0.25
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist)
  }
  else
    family_link.stop(family, link)
  # Bind R^2s into a matrix and return with AIC values
  data.frame(Class=mdl.class, Family = family, Link = link,
             Marginal=Rm, Conditional=Rc, AIC=mdl.aic)
}

#' stop execution if unable to calculate variance for a given family and link
#' @export
family_link.stop <- function(family, link){
  stop(paste("Don't know how to calculate variance for",
             family, "family and", link, "link."))
}
###################### 1. WILD THERMAL TOLERANCE ###############
##### repeatability anova across sp ########
# anova
tt.aov<-aov(ther.wild.tt$mins.40.to.ko ~ ther.wild.tt$species, data = ther.wild.tt); summary(tt.aov)

# lmm method
rep.tt <- rpt(mins.40.to.ko ~ 1 +(1|species), grname=c("species"), datatype="Gaussian", nboot = 1000, data=ther.wild.tt); print(rep.tt) ; plot(rep.tt, cex.main = 1, main = "Wild thermal tolerance\n across species") 

##### checks, prep ####
hist(ther.wild.tt$mins.ko.total)
hist(ther.wild.tt$altitude)
hist(ther.wild.tt$mins.to.40)

## scale explanatory variables, only
ther.wild.tt$mins.to.40.scaled <- scale(ther.wild.tt$mins.to.40)
ther.wild.tt$altitude.scaled <- scale(ther.wild.tt$altitude )
ther.wild.tt$temp.at.ko.scaled <- scale(ther.wild.tt$temp.at.ko )
ther.wild.tt$area.mm2.scaled <- scale(ther.wild.tt$area.mm2 )
ther.wild.tt.res.df <- subset(ther.wild.tt, !(is.na(ther.wild.tt$mins.to.40)))
ther.wild.tt.res.df$species <- factor(ther.wild.tt.res.df$species)

##### lmerTest model selection #####  
names(ther.wild.tt.res.df)
ther.wild.tt.res.df$range <- if_else((ther.wild.tt.res.df$species=="erato"|ther.wild.tt.res.df$species=="sara"|
                                        ther.wild.tt.res.df$species=="melpomene"|ther.wild.tt.res.df$species=="timareta"), "wide-range","specialist")

m1.lmer <- lmerTest::lmer(mins.ko.total ~ (altitude.scaled + area.mm2.scaled+sex)^2+
                            mins.to.40.scaled + temp.at.ko.scaled +(1|species), 
                          data=ther.wild.tt.res.df); plot(m1.lmer);summary(m1.lmer)

lmerTest::ranova(m1.lmer)
step(m1.lmer, alpha.fixed = 0.05)
m2.lmer <- lmerTest::lmer(mins.ko.total ~ altitude.scaled + mins.to.40.scaled + (1 | species),data=ther.wild.tt.res.df); plot(m2.lmer); summary(m2.lmer)
##### R2 mumin #####
# marginal:  proportion of variance explained by the fixed factor(s)
# conditional: proportion of variance explained by both the fixed and random factors. info: https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
MuMIn::r.squaredGLMM(m2.lmer)

# similar values when estimated with other packages
sjstats::r2(m2.lmer)
rsquared(m2.lmer)


###################### 2. ERATO COMMON GARDEN THERMAL TOLERANCE ###############
##### repeatability anova across broods ########
# anova
tt.aov<-aov(era.tt.cg$mins.40.to.ko ~ era.tt.cg$mother.id, data = era.tt.cg); summary(tt.aov)

# lmm method
rep.tt <- rpt(mins.40.to.ko ~ 1 +(1|mother.id), grname=c("mother.id"), datatype="Gaussian", nboot = 1000, data=era.tt.cg); print(rep.tt) ; plot(rep.tt, cex.main = 1, main = "CG thermal tolerance\n across broods") 

#### checks, prep ####
## scale explanatory variables, only
era.tt.cg$mins.to.40.scaled <- scale(era.tt.cg$mins.to.40)
era.tt.cg$altitude.scaled <- scale(era.tt.cg$altitude )
era.tt.cg$temp.at.ko.scaled <- scale(era.tt.cg$temp.at.ko )
era.tt.cg$area.mm2.scaled <- scale(era.tt.cg$area.mm2 )
era.tt.cg$egg.no.per.brood.scaled <- scale(era.tt.cg$egg.no.per.brood )
era.tt.cg$adult.mass.scaled <- scale(era.tt.cg$adult.mass )
era.tt.cg$pupa.clean.mass.scaled <- scale(era.tt.cg$pupa.clean.mass )
era.tt.cg$dev.time.larva.to.adult <- scale(era.tt.cg$dev.time.larva.to.adult)
era.tt.cg.res.df <- subset(era.tt.cg, !(is.na(era.tt.cg$mins.to.40)))
era.tt.cg.res.df$mother.id <- factor(era.tt.cg.res.df$mother.id)
era.tt.cg.res.df.sub <- subset(era.tt.cg.res.df, adult.mass!=""&egg.no.per.brood!=""&mother.id!="20?"&altitude<1500&
                                 !(is.na(mins.to.40))&!(is.na(dev.time.larva.to.pupa))&!(is.na(area.mm2.scaled)))

#### lmerTest model selection #####  
names(ther.wild.tt.res.df)
#  egg.no.per.brood.scaled similar to adult wing size
m1.lmer <- lmerTest::lmer(mins.ko.total ~ (altitude.scaled+area.mm2.scaled +dev.time.larva.to.pupa+ egg.no.per.brood.scaled+sex)^2+
                            mins.to.40.scaled + temp.at.ko.scaled+ (1|mother.id), 
                          data=era.tt.cg.res.df.sub ); plot(m1.lmer);summary(m1.lmer)

lmerTest::ranova(m1.lmer)
step(m1.lmer, alpha.fixed = 0.05)

m2.lmer <- lmerTest::lmer(mins.ko.total ~ altitude.scaled + dev.time.larva.to.pupa + egg.no.per.brood.scaled + 
                            mins.to.40.scaled + temp.at.ko.scaled + (1 | mother.id) + 
                            altitude.scaled:egg.no.per.brood.scaled,
                          data=era.tt.cg.res.df.sub); plot(m2.lmer); summary(m2.lmer)
