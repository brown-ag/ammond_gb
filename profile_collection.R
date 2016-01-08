library(aqp)
#profile collection test
foo=read.csv("para_3holetest_samples.csv",stringsAsFactors=FALSE) #todo all
corez=read.csv("para_3holetest_cores.csv",stringsAsFactors=FALSE)

#create sampled depth intervals scaled by Geoprobe % recovery
scalez=corez$rec[foo$CORE]
intervalz=foo$BDEPTH-foo$TDEPTH
sintervalz=intervalz/scalez

#calculate boundaries of sampled intervals
boundz=cumsum(sintervalz)
boundz2=c(0,boundz)
uboundz=boundz2[1:length(boundz2)-1]
lboundz=boundz2[2:length(boundz2)]

boreholez=factor(paste(foo$SITE,foo$ROW,foo$TREE,sep="-"))
id=as.integer(boreholez)
hz_top=uboundz
hz_bot=lboundz
name=rep("C",length(hz_top))
sp=data.frame(id,name,hz_top,hz_bot,foo$NO3,foo$PH,foo$EC,foo$Sand,foo$Silt,foo$Clay,foo$texturez)
depths(sp) <- id ~ hz_top + hz_bot
print(sp)
plot(sp,"Modesto - Paradise Almond Site Example",color="foo.Clay")
slabz=  slab(sp, fm= ~ foo.Sand + foo.Silt + foo.Clay, slab.structure=c(0,100,200,300,400), na.rm=TRUE)
slabz

library(lattice)

xyplot(top ~ p.q50 | variable, data=slabz, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=slabz$p.q25, upper=slabz$p.q75, ylim=c(401,-2),
       panel=panel.depth_function,
       alpha=0.25, sync.colors=TRUE,
       par.settings=list(superpose.line=list(col='RoyalBlue', lwd=2)),
       prepanel=prepanel.depth_function,
       cf=slabz$contributing_fraction, cf.col='black', cf.interval=5, 
       layout=c(5,1), strip=strip.custom(bg=grey(0.8)),
       scales=list(x=list(tick.number=4, alternating=3, relation='free'))
)

