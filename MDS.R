### Punta Eugenia MDS plots #####
#################################
rm(list=ls()) # remove all clean all
library(vegan)

# load data

x<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_algae_data.csv")
xm<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_algae_Mid_data.csv")
xs<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_algae_Shallow_data.csv")
xd<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_algae_Deep_data.csv")

y<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_inverts_data.csv")
ym<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_inverts_Mid_data.csv") 
ys<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_inverts_Shallow_data.csv") 
yd<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_inverts_Deep_data.csv") 

z<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_fish_data.csv")
zm<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_fish_Mid_data.csv")
zs<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_fish_Shallow_data.csv")
zd<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_fish_Deep_data.csv")

k<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_upc_data.csv")
km<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_upc_Mid_data.csv")
ks<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_upc_Shallow_data.csv")
kd<-read.csv("data_clean/2018_04_punta_eugenia_wide_msd_upc_Deep_data.csv")

x2<-x[2:17] 
x3 <- metaMDS(x2, distance='bray')
x3$stress
stressplot(x3, main="Shepard plot")
gof=goodness(x3)
plot(x3, type="t", main="Goodness of fit")

xm2<-xm[2:15] 
xm3 <- metaMDS(xm2, distance='bray')
xm3$stress
stressplot(xm3, main="Shepard plot")
gof=goodness(xm3)
plot(xm3, type="t", main="Goodness of fit")

xs2<-xs[2:13] 
xs3 <- metaMDS(xs2, distance='bray')
xs3$stress
stressplot(xs3, main="Shepard plot")
gof=goodness(xs3)
plot(xs3, type="t", main="Goodness of fit")

xd2<-xd[2:11] 
xd3 <- metaMDS(xd2, distance='bray')
xd3$stress
stressplot(xd3, main="Shepard plot")
gof=goodness(xd3)
plot(xd3, type="t", main="Goodness of fit")

y2<-y[2:53]
y3 <- metaMDS(y2, distance='bray')
y3$stress
stressplot(y3, main="Shepard plot")
gof=goodness(y3)
plot(y3, type="t", main="Goodness of fit")

ym2<-ym[2:42]
ym3 <- metaMDS(ym2, distance='bray')
ym3$stress
stressplot(ym3, main="Shepard plot")
gof=goodness(ym3)
plot(ym3, type="t", main="Goodness of fit")

ys2<-ys[2:29]
ys3 <- metaMDS(ys2, distance='bray')
ys3$stress
stressplot(ys3, main="Shepard plot")
gof=goodness(ys3)
plot(ys3, type="t", main="Goodness of fit")

yd2<-yd[2:34]
yd3 <- metaMDS(yd2, distance='bray')
yd3$stress
stressplot(yd3, main="Shepard plot")
gof=goodness(yd3)
plot(yd3, type="t", main="Goodness of fit")

z2<-z[2:25]
z3 <- metaMDS(z2, distance='bray')
z3$stress
stressplot(z3, main="Shepard plot")
gof=goodness(z3)
plot(z3, type="t", main="Goodness of fit")

zm2<-zm[2:22]
zm3 <- metaMDS(zm2, distance='bray')
zm3$stress
stressplot(zm3, main="Shepard plot")
gof=goodness(zm3)
plot(zm3, type="t", main="Goodness of fit")

zs2<-zs[2:17]
zs3 <- metaMDS(zs2, distance='bray')
zs3$stress
stressplot(zs3, main="Shepard plot")
gof=goodness(zs3)
plot(zs3, type="t", main="Goodness of fit")

zd2<-zd[2:18]
zd3 <- metaMDS(zd2, distance='bray')
zd3$stress
stressplot(zd3, main="Shepard plot")
gof=goodness(zd3)
plot(zd3, type="t", main="Goodness of fit")

k2<-k[2:48]
k3 <- metaMDS(k2, distance='bray')
k3$stress
stressplot(k3, main="Shepard plot")
gof=goodness(k3)
plot(k3, type="t", main="Goodness of fit")

km2<-km[2:38]
km3 <- metaMDS(km2, distance='bray')
km3$stress
stressplot(km3, main="Shepard plot")
gof=goodness(km3)
plot(km3, type="t", main="Goodness of fit")

ks2<-ks[2:36]
ks3 <- metaMDS(ks2, distance='bray')
ks3$stress
stressplot(ks3, main="Shepard plot")
gof=goodness(ks3)
plot(ks3, type="t", main="Goodness of fit")

kd2<-kd[2:37]
kd3 <- metaMDS(kd2, distance='bray')
kd3$stress
stressplot(kd3, main="Shepard plot")
gof=goodness(kd3)
plot(kd3, type="t", main="Goodness of fit")

#######
## One way to do it ###

#### algae

dune<-x[4:17] # separate vectors with data
dune.env<-x[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("Algae - NMSS/Bray-Stress=", round(x3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('bottomright', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-xm[4:15] # separate vectors with data
dune.env<-xm[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("AlgaeMid - NMSS/Bray-Stress=", round(xm3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('bottomright', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-xs[4:13] # separate vectors with data
dune.env<-xs[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("AlgaeShallow - NMSS/Bray-Stress=", round(xs3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('bottomright', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-xd[4:17] # separate vectors with data
dune.env<-xd[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("AlgaeDeep - NMSS/Bray-Stress=", round(xd3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('bottomright', pch = shps,legend=unique(dune.env$Site), cex = 1)

#### inverts

dune<-y[4:53] # separate vectors with data
dune.env<-y[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("Inverts - NMSS/Bray-Stress=", round(y3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-ym[4:42] # separate vectors with data
dune.env<-y[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("InvertsMid - NMSS/Bray-Stress=", round(ym3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-ys[4:29] # separate vectors with data
dune.env<-ys[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("InvertsShallow - NMSS/Bray-Stress=", round(ys3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('topleft', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('bottomleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-yd[4:34] # separate vectors with data
dune.env<-yd[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("InvertsDeep - NMSS/Bray-Stress=", round(yd3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

#### fish

dune<-z[4:25] # separate vectors with data
dune.env<-z[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("Fish - NMSS/Bray-Stress=", round(z3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('topright', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('bottomright', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-zm[4:22] # separate vectors with data
dune.env<-zm[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("FishMid - NMSS/Bray-Stress=", round(zm3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-zs[4:17] # separate vectors with data
dune.env<-zs[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("FishShallow - NMSS/Bray-Stress=", round(zs3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-zd[4:18] # separate vectors with data
dune.env<-zd[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("FishDeep - NMSS/Bray-Stress=", round(zd3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-z[4:25] # separate vectors with data
dune.env<-z[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("Fish - NMSS/Bray-Stress=", round(z3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-k[4:48] # separate vectors with data
dune.env<-k[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("UPC - NMSS/Bray-Stress=", round(k3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('bottomrigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('bottomleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-km[4:38] # separate vectors with data
dune.env<-km[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("UPCMid - NMSS/Bray-Stress=", round(km3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)


dune<-ks[4:36] # separate vectors with data
dune.env<-ks[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("UPCShallow - NMSS/Bray-Stress=", round(ks3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)

dune<-kd[4:37] # separate vectors with data
dune.env<-kd[1:3] #separate vectors with names
dune.env$Year<-as.factor(dune.env$Year) ## make year column a factor
mds <- cmdscale(vegdist(dune, method='bray')) # mds matrix
cols = c('red', 'orange', "blue") # color code sites
shps = seq(length(unique(dune.env$Site))) # color code year
plot(mds, type = 'n', main=paste("UPCDeep - NMSS/Bray-Stress=", round(kd3$stress,3)))# empty plot
points(mds, col = cols[dune.env$Year], pch = shps[dune.env$Site], cex=2)# add points
legend('toprigh', col=cols, legend=levels(dune.env$Year), pch = 16, cex = 1)# add legend
legend('topleft', pch = shps,legend=unique(dune.env$Site), cex = 1)