gex.data=read.csv("gex.data")
attach(gex.data)
plot(east,north,cex=s/2,col=gray(1-(v+1)/max(v+1)),pch=19-c) #s is group size, v is visibility (0/1), c is color (green/yellow, referred to as "sex" in the book)
detach(gex.data)
glines=read.csv("glines")
attach(glines)
for(i in 1:(length(glines[,1])/2)) lines(c(x[2*i-1],x[2*i]),c(y[2*i-1],y[2*i]),col=Stratum[2*i]+1,lwd=2)
gstrips=read.csv("glines")
gbnd=read.csv("gbnd")
detach(glines)
attach(gbnd)
for(i in 1:(length(gbnd[,1])-1)) lines(c(x[i],x[i+1]),c(y[i],y[i+1]),col=type[i]*Stratum[i]+1,lwd=2)

seen = apply(gex.data[,16:23],1,sum) # number times seen by anyone
plot(abs(gex.data$SignPerp),seen>0) # seen at all vs perp dist
