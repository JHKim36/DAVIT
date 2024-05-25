##install.packages("MCMCglmm")
##install.package("caret")
library(MCMCglmm)
library(caret)

data1<-read.csv("data-raw/all.grid.lulch.csv")
str(data1)
head(data1)
data2<-na.omit(data1)
head(data2)
str(data2)
data2<-data2[c(-1)]
grid.data2<-data2[2] #Extrect grid
wo.grid.data2<-data2[-2]
head(wo.grid.data2)

#####data rescale######
wo.grid.data2.scale<-preProcess(as.data.frame(wo.grid.data2),
                                method = c("range"))
wo.grid.data2.scale.norm<-predict(wo.grid.data2.scale,
                                  as.data.frame(wo.grid.data2))

summary(wo.grid.data2.scale.norm)
wo.grid.data2.scale.norm$grid<-grid.data2
head(wo.grid.data2.scale.norm)

####prior infomation#####
k<-15
prior1.1<-list(B=list(V=diag(k)*1e4, mu=rep(0,k)),
               R=list(V=diag(1),nu=0.002),
               G=list(G1=list(V=diag(1), nu=1)))
THIN<-500
BURNIN<-500000
NITT<-900000

####MCMCglmm#####
######fdiv mcmc#
fdiv.mcmc <- MCMCglmm (Fdiv~elemean+ slopemean + Human_pop +Cultivated.and.managed.vegetation+
                         Shrubs+Urban...built.up +Permanent.water.bodies+
                         Herbaceous.wetland+Herbaceous.vegetation+
                         Bare...sparse.vegetation+Closed.forest+Open.forest+
                         Oceans+lulc.sumforest.diversit,
                       random = ~grid,
                       data = wo.grid.data2.scale.norm,
                       prior=prior1.1, nitt = NITT,
                       burnin=BURNIN,
                       thin=THIN)
autocorr.diag(fdiv.mcmc$Sol[, 1:k])#under0.1
autocorr.diag(fdiv.mcmc$VCV)#under0.1

summary(fdiv.mcmc)


######frich mcmc#
str(wo.grid.data2.scale.norm)
fric.mcmc <- MCMCglmm (Fric~elemean+ slopemean + Human_pop +Cultivated.and.managed.vegetation+
                         Shrubs+Urban...built.up +Permanent.water.bodies+
                         Herbaceous.wetland+Herbaceous.vegetation+
                         Bare...sparse.vegetation+Closed.forest+Open.forest+
                         Oceans+lulc.sumforest.diversit,
                       random = ~grid,
                       data = wo.grid.data2.scale.norm,
                       prior=prior1.1, nitt = NITT,
                       burnin=BURNIN,
                       thin=THIN)
autocorr.diag(fric.mcmc$Sol[, 1:k])
autocorr.diag(fric.mcmc$VCV)
summary(fric.mcmc)

######feve mcmc##
str(wo.grid.data2.scale.norm)
Feve.mcmc <- MCMCglmm (Feve~elemean+ slopemean + Human_pop +Cultivated.and.managed.vegetation+
                         Shrubs+Urban...built.up +Permanent.water.bodies+
                         Herbaceous.wetland+Herbaceous.vegetation+
                         Bare...sparse.vegetation+Closed.forest+Open.forest+
                         Oceans+lulc.sumforest.diversit,
                       random = ~grid,
                       data = wo.grid.data2.scale.norm,
                       prior=prior1.1, nitt = NITT,
                       burnin=BURNIN,
                       thin=THIN)
autocorr.diag(Feve.mcmc$Sol[, 1:k])
autocorr.diag(Feve.mcmc$VCV)
summary(Feve.mcmc)

######sr mcmc#
str(wo.grid.data2.scale.norm)

sr.mcmc <- MCMCglmm (SR~elemean+ slopemean + Human_pop +Cultivated.and.managed.vegetation+
                       Shrubs+Urban...built.up +Permanent.water.bodies+
                       Herbaceous.wetland+Herbaceous.vegetation+
                       Bare...sparse.vegetation+Closed.forest+Open.forest+
                       Oceans+lulc.sumforest.diversit,
                     random = ~grid,
                     data = wo.grid.data2.scale.norm,
                     prior=prior1.1, nitt = NITT,
                     burnin=BURNIN,
                     thin=THIN)
autocorr.diag(sr.mcmc$Sol[, 1:k])
autocorr.diag(sr.mcmc$VCV)
summary(sr.mcmc)


######sr mcmc#
str(wo.grid.data2.scale.norm)
h.mcmc <- MCMCglmm (H~elemean+ slopemean + Human_pop +Cultivated.and.managed.vegetation+
                      Shrubs+Urban...built.up +Permanent.water.bodies+
                      Herbaceous.wetland+Herbaceous.vegetation+
                      Bare...sparse.vegetation+Closed.forest+Open.forest+
                      Oceans+lulc.sumforest.diversit,
                    random = ~grid,
                    data = wo.grid.data2.scale.norm,
                    prior=prior1.1, nitt = NITT,
                    burnin=BURNIN,
                    thin=THIN)
autocorr.diag(h.mcmc$Sol[, 1:k])
autocorr.diag(h.mcmc$VCV)
summary(h.mcmc)
