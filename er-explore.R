setwd("~/GitHub/poolingrobust")
library(Hmisc)
dat <- read.csv("gex.data", header=TRUE)
dat$seen <- apply(dat[,16:23],1,sum) # number times seen by anyone
# densities by stratum
density <- c(130/1040, 120/640)

hist(abs(dat$SignPerp[dat$seen>0]), nc=20, xlim=c(0,4), main="Golf tees, pooled across all covariates", 
     xlab="Distance (m)", sub="250 groups, 162 detected")

par(mfrow=c(2,2))
out <- histbackback(split(dat$s, dat$Stratum), 
                    main="Size distribution of groups\nbetween strata",
                    xlab=c("Stratum 1", "Stratum 2"))
barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)

out <- histbackback(split(abs(dat[dat$seen>0, "SignPerp"]), dat$Stratum[dat$seen>0]),
                    brks=seq(0,4,by=.25),
                    main="Golf tees, detection distances by strata",
                    xlab=c("Stratum 1", "Stratum 2"))
barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)

out <- histbackback(split(abs(dat[dat$seen>0, "SignPerp"]), dat$c[dat$seen>0]),
                    brks=seq(0,4,by=.25),
                    main="Golf tees, detection distances by colour",
                    xlab=c("Green (?)", "Yellow (?)"))
barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)

out <- histbackback(split(abs(dat[dat$seen>0, "SignPerp"]), dat$v[dat$seen>0]),
                    brks=seq(0,4,by=.25),
                    main="Golf tees, detection distances by visibility (exposure)",
                    xlab=c("0", "1"))
barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
par(mfrow=c(1,1))

#  Below are hazard rate detection function fits with various covariates
#    of models examined, colour effect is most pronounced but
#    estimates of P_a for all models are very close

fordis <- subset(dat, select=c(Stratum, Area, Strip, Length, x, s, c, v, seen))
names(fordis) <- c("Region.Label", "Area", "Sample.Label", 
                   "Effort", "distance", "size", "colour", 
                   "visib", "detected")
fordis <- fordis[fordis$detected>0,]
fordis <- subset(fordis, select=-detected)
library(Distance)
haz <- ds(data=fordis, key="hr", transect = "line")
haz.str <- ds(data=fordis, key="hr", formula = ~Region.Label)
haz.vis <- ds(data=fordis, key="hr", formula=~visib)
haz.size <- ds(data=fordis, key="hr", formula=~size)
haz.colour.size <- ds(data=fordis, key="hr", formula=~colour+size)
haz.colour <- ds(data=fordis, key="hr", formula=~colour)
summarize_ds_models(haz, haz.str, haz.vis, haz.size, haz.colour.size, haz.colour)
plot(haz.colour)
