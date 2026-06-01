# prediction of annual mean anomaly from year to date in GISTEMP data.
# bstryr,bendyr are for the baseline

# make a line a stepped function. 
# x is a regular two-columned series
mkstep <- function(x,del) {
	dx=x[2,1]-x[1,1]
	lx=dim(x)[1]
	y=matrix(NA,lx*2,2)
	y[2*(1:lx)-1,1]=x[,1]-dx*del
	y[2*(1:lx),1]=x[,1]+dx*del
	y[2*(1:lx)-1,2]=x[,2]
	y[2*(1:lx),2]=x[,2]
	return(y)
}

src="giss"
rec="GISTEMP v4 LOTI"
tmp=read.csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",skip=1,na.strings="***")
temp0=c(t(tmp[,2:13]))
bstryr=1850
bendyr=1900
stmon=1

stryr=tmp[1,1]  # start year of index 
endyr=tmp[dim(tmp)[1],1] # current year
yrs=stryr:endyr
curr=tail(which(!is.na(tail(temp0,12))),1) # last available month

# vector with dates
temp=cbind(stryr+((1:length(temp0)-0.5)/12),temp0)

ann=NA*(1:(endyr-stryr))
ytd=NA*(1:(endyr-stryr+1))

# rebaseline 
base=mean(temp[temp[,1] > bstryr & temp[,1] < bendyr+1,2])
temp[,2]=temp[,2]-base
if (src == "giss" & bstryr==1850) {temp[,2] = temp[,2]-0.02} # fix to get 'pre-industrial' based on other time-series that go back further

for (i in 1:(endyr-stryr+1)) {ytd[i]=mean(temp[(12*(i-1)+stmon):(12*(i-1)+curr+stmon-1),2])}
for (i in 1:(endyr-stryr)) {ann[i]=mean(temp[(12*(i-1)+stmon):(12*i+stmon-1),2])}

ytd1=ytd[1:(endyr-stryr)]

# linear regression btw ann and ytd
fred=lm(ann ~ ytd1)
pred=predict(fred,newdata=data.frame(ytd1=ytd[endyr-stryr+1]),se.fit=TRUE,interval="prediction")

#print(paste("Previous max (",yrs[ann==max(ann)],"):",round(max(ann),2)))
print(paste(endyr,": ",round(pred$fit[1],3),"CI:",round(pred$fit[2],3),round(pred$fit[3],3)))

# plotting
ci = TRUE
unc=read.csv("https://data.giss.nasa.gov/gistemp/graphs_v4/graph_data/totalCI_ERA.csv")
y1=mkstep(unc[,c(1,2)],0.5)

xymax=round(1.2*max(ann,ytd),1) 
xymin=round(1.2*min(ann,ytd),1) 

mon=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

plot(ytd1,ann,pch=16,col=2,xlim=c(xymin,xymax),ylim=c(xymin,xymax),xlab="Year-to-Date mean anomaly (ºC)",ylab="Annual mean anomaly (ºC)", main=paste("Predicting the",endyr,rec,"mean anomaly"))
lines(c(xymin,xymax),c(1,1),col=4,lty=2)
points(ytd[endyr-stryr],ann[endyr-stryr],col=4,pch=16)
points(ytd[endyr-stryr+1],pred$fit[1],col=3,pch=16,lwd=6)
lines(c(ytd[endyr-stryr+1],ytd[endyr-stryr+1]),pred$fit[2:3],col=3,lwd=3)
legend(0.8*xymax,ann[endyr-stryr]+0.06,legend=as.character(endyr-1),box.lwd=0,col=4,text.col=4)
legend(ytd[endyr-stryr+1]-0.4,pred$fit[1]+0.06,legend=paste(as.character(endyr)," Prediction",sep=""),box.lwd=0,col=3,text.col=3)
legend(xymin+0.5*(xymax+xymin),xymin+0.3,legend=paste("Estimate based on ",mon[1],"-",mon[curr],sep=""),box.lwd=0,col=1,text.col=1,cex=1.5)
legend(xymin+0.5*(xymax+xymin)+0.1,xymin+0.1,legend=paste("Baseline (",as.character(bstryr),"-",as.character(bendyr),")",sep=""),box.lwd=0,col=1,text.col=1,cex=1)
if (bendyr <= "1900") {
legend(0.15,1.1,legend="1ºC above late 19th Century",box.lwd=0,text.col=4,cex=1)}

# stats of likelihoods 
nex=1000000
x=rnorm(nex,pred$fit[1],abs(pred$fit[2]-pred$fit[1])/1.96)
pc_new_rec=sum(x > max(ann))/nex
pc_above_1C=sum(x > 1.0)/nex

#print(paste("P(new record): ",pc_new_rec))
#print(paste("P(T > 1 C): ",pc_above_1C))

# above last few years
pc_yr1=sum(x > ann[endyr-stryr])/nex
pc_yr2=sum(x > ann[endyr-stryr-1])/nex
pc_yr3=sum(x > ann[endyr-stryr-2])/nex
pc_yr4=sum(x > ann[endyr-stryr-3])/nex
pc_top5=sum(x > tail(sort(ann),5)[1])/nex
pc_top10=sum(x > tail(sort(ann),10)[1])/nex

    
#print(paste("P(>last year):",pc_yr1))
#print(paste("P(>two years ago):",pc_yr2))
#print(paste("P(>three years ago):",pc_yr3))
#print(paste("P(>four years ago):",pc_yr4))
#print(paste("P(top 5):",pc_top5))
#print(paste("P(top 10):",pc_top10))
    
# assuming sampling error of 0.05, likelihood that current year > max year
x2=rnorm(nex,pred$fit[1],0.05)
x3=rnorm(nex,max(ann),0.05)
pc_prob_new_rec=sum(x2 > x3)/nex

#print(paste("P(new record - incl sampling): ",pc_prob_new_rec))

# make a timeseries plot including this yrs prediction
# add uncertainty CI where possible
y0=mkstep(cbind(stryr:(endyr-1),ann),0.5)
ny0=dim(y0)[1]

quartz(width=10,height=8)
plot((stryr):(endyr-1),ann,type="n",ylim=c(xymin,xymax),xlim=c(stryr-2,endyr+2),xaxs="i",main=paste(rec,"(incl.",endyr,"prediction)"),ylab=paste("Anomaly w.r.t. baseline (",as.character(bstryr),"-",as.character(bendyr),") (ºC)",sep=""),xlab="Year",lwd=2)
if (ci) {polygon(c(y1[1:ny0,1],rev(y1[1:ny0,1])),c(y0[,2]+y1[1:ny0,2],rev(y0[,2]-y1[1:ny0,2])),col="light gray",border=NA)}
lines((stryr):(endyr-1),ann,lwd=2)
points(stryr:(endyr-1),ann,pch=15)
points(endyr,pred$fit[1],col=3,pch=16,lwd=6)
lines(c(endyr,endyr),pred$fit[2:3],col=3,lwd=3)

if (bendyr <= "1900") {
lines(c(1850,2050),c(1,1),col=4,lty=2)
legend(stryr,1.13,legend="1ºC above the pre-industrial",box.lwd=0,text.col=4,cex=1)
lines(c(1850,2050),c(1.5,1.5),col=4,lty=2)
legend(stryr,1.63,legend="1.5ºC above the pre-industrial",box.lwd=0,text.col=4,cex=1)
}

legend(0.25*stryr+0.75*endyr,pred$fit[1]+0.06,legend=paste(as.character(endyr)," prediction",sep=""),box.lwd=0,col=3,text.col=3,cex=1.2)
legend(0.6*stryr+0.4*endyr,xymin+0.1,legend=paste(as.character(endyr)," annual estimate based on ",mon[1],"-",mon[curr],".    @ClimateOfGavin",sep=""),box.lwd=0,col=1,text.col=1,cex=1.)
