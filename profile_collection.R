library(aqp)
#profile collection test
foo=read.csv("para_3holetest_samples.csv",stringsAsFactors=FALSE) #todo all
corez=read.csv("para_3holetest_cores.csv",stringsAsFactors=FALSE)

foo$NO3[which(foo$NO3=="ND")] = 0.0
foo$NO3=as.numeric(foo$NO3)

boreholez=factor(paste(foo$SITE,foo$ROW,foo$TREE,sep="-"),levels=c("PARA-5-10","PARA-15-10","PARA-25-10"))
id=as.integer(boreholez)

#create sampled depth intervals scaled by Geoprobe % recovery by core and by borehole
boreholes=data.frame(sid=id,cid=foo$CORE,scale=corez$rec[foo$CORE],interval=foo$BDEPTH-foo$TDEPTH)
boreholes=cbind(boreholes,sinterval=boreholes$interval/boreholes$scale)

#calculate boundaries of sampled intervals

addzero=function(x) {
  return(c(0,x))
}
makeupperbounds=function(x) {
  return(cumsum(addzero(x[1:length(x)-1])))
}

makelowerbounds=function(x) {
  return(cumsum(x[1:length(x)]))
}
uboundz=as.numeric(unlist(by(data=boreholes$sinterval,INDICES=id,FUN=makeupperbounds)))
lboundz=as.numeric(unlist(by(data=boreholes$sinterval,INDICES=id,FUN=makelowerbounds)))

hz_top=uboundz
hz_bot=lboundz
name=rep("C",length(hz_top))
sp=data.frame(id,name,hz_top,hz_bot,foo$NO3,foo$PH,foo$EC,foo$Sand,foo$Silt,foo$Clay)
depths(sp) <- id ~ hz_top + hz_bot
print(sp)
plot(sp,"Modesto - Paradise Almond Site Example",color="foo.EC")
slabz=slab(sp, fm= ~  foo.Clay+ foo.NO3 + foo.PH + foo.EC, slab.structure=seq(0,400,4))
slabz

library(lattice)

xyplot(top ~ p.q50 | variable, data=slabz, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=slabz$p.q25, upper=slabz$p.q75, ylim=c(401,-2),
       panel=panel.depth_function,
       alpha=0.25, sync.colors=TRUE,
       par.settings=list(superpose.line=list(col='RoyalBlue', lwd=2)),
       prepanel=prepanel.depth_function,
       cf=slabz$contributing_fraction, cf.col='black', cf.interval=6, 
       layout=c(5,1), strip=strip.custom(bg=grey(0.8)),
       scales=list(x=list(tick.number=4, alternating=3, relation='free'))
)

