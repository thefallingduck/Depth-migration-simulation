## Simulation of stratified water column, cephalopod, and resulting SIMS data

#To do: Bind simulated columns and sort by distance,
#			Formulas for other 'types' of depth migration behavior
#			

depth.max<-500														#Maximum Depth of Waterway
	
depth.range<-c(0,depth.max)											#Range of depth possible at point in the seaway

depth<-seq(from=depth.range[1],to=depth.range[2],by=1)

temp.0<-26															#Surface temperature
temp.deep<-5
temp.halflife<-1/200
ceph.growth.day<-25													#Growth/day in microns

ceph.days<-80														#Days continuous measure

var.ceph.depth<-1													#How variable the values are around 'normal' depth pattern
ceph.depth.amp<-50

sims.growth.ave<-10													#Quantity describing growth distance averaged by SIMS pit in um

sim.type='o'

salinity.strat<-seq(from=depth.range[1],to=depth.range[2],by=1)
temp.strat<-seq(from=depth.range[1],to=depth.range[2],by=1)
sal.mag<-20
sal.mean<-50
d.o.open<-(-1)

for (j in 1:length(depth)){

salinity.strat[j]<-sal.mag*cos(depth[j]*(2*pi)/length(depth))+sal.mean						#Function that describes salinity at depth in seaway

}

for (i in 1:length(depth)){

temp.strat[i]<-(temp.0-temp.deep)*exp(-temp.halflife*i)+temp.deep										#Function describing the temperature change with depth in seaway

}

d.o.water<-(-(salinity.strat/34.3)-1)*(d.o.open+1.22)-1.22								#Equation from Wright 1986-replace with recent equation

d.o.carb<-(temp.strat-21.8)/(-4.69)+(d.o.water)											#Grossman and Ku 1986 Funtion combining temperature and d18Owater variation to predict carbonate at depth

ceph.growth<-seq(from=1,to=(ceph.growth.day*ceph.days),by=1)											#Function describing cephalopod growth distance
swim.path<-ceph.growth
swim.d.o<-ceph.growth

for (k in 1:length(ceph.growth)){

swim.path[k]<-round(ceph.depth.amp*sin(ceph.growth[k]*(2*pi)/ceph.days)+(1/2*depth.max)+rnorm(n=1,mean=0,sd=var.ceph.depth))																			#Function describing swimming depth of cephalopod

}

depth.d.o.carb<-as.data.frame(cbind(depth,d.o.carb))

for (m in 1:length(ceph.growth)){

swim.d.o[m]<-depth.d.o.carb$d.o.carb[depth.d.o.carb$depth==swim.path[m]]

}

sims.data.series<-seq(1,(length(ceph.growth)/sims.growth.ave),by=1)

var.sims<-round(rnorm(n=(length(ceph.growth)/sims.growth.ave),sd=6),0)

sims.growth.dist<-seq(0,length(ceph.growth)-1,by=sims.growth.ave)

for (k in 1:((length(ceph.growth)/sims.growth.ave))){

from<-((((k+2)*5)+var.sims[k])-5)
to<-((((k+2)*5)+var.sims[k])+4)

sims.data.series[k]<-mean(swim.d.o[from:to])

sims.growth.dist[k]<-mean(c(from,to))

}

windows()
layout(matrix(c(1,1,2,2,3,4), nrow=3, ncol=2, byrow=TRUE), widths=c(1,1,0.5,0.5), heights=1)

plot(x=ceph.growth,
y=swim.path,
ylim=c(min(swim.path)-10,max(swim.path)+10),
ylab='Depth',
xlab='Growth Distance (um)',
type=sim.type)	#Behavioral Pattern

plot(x=sims.growth.dist,
y=sims.data.series,
ylim=c(min(sims.data.series)-.5,max(sims.data.series)+.5),
ylab=expression(delta^18*O~VPDB("")),
xlab='Growth Distance (um)',
type=sim.type) #Simulation Time Series

points(x=sims.growth.dist,
y=sims.data.series+.2,
type='l')

points(x=sims.growth.dist,
y=sims.data.series-.2,
type='l')

plot(x=d.o.carb,
y=depth,
ylim=c(max(depth),min(depth)),
xlab=expression(delta^18*O~VPDB("")),
ylab='Depth (m)')  #Water depth and expected delta18O

hist(sims.data.series,
main='',
xlab=expression(delta^18*O~VPDB("")))

#Distribution of differences between pits as function of distance


#plot(x=salinity.strat,y=depth,ylim=c(max(depth),min(depth)))
#plot(x=temp.strat,y=depth,ylim=c(max(depth),min(depth)))
#plot(x=d.o.carb,y=depth,ylim=c(max(depth),min(depth)))
#plot(x=ceph.growth,swim.d.o,type='b')
#


#hist(sims.data.series)

#plot(x=ceph.growth,y=swim.d.o)
