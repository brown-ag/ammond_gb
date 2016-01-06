library(ggplot2)
library(reshape2)
library(grid)

#x11()
foo=read.csv("para_15-10_samples.csv",stringsAsFactors=FALSE)
corez=read.csv("para_15-10_cores.csv",stringsAsFactors=FALSE)

#create depths scaled by % recovery
scalez=corez$rec[foo$CORE]
intervalz=foo$BDEPTH-foo$TDEPTH
sintervalz=intervalz/scalez
depthz=cumsum(sintervalz)
foo=cbind(foo,depthz)

#convert Non-detectable into zero values
foo$NO3[which(foo$NO3=="ND")] = 0.0
foo$NO3=as.numeric(foo$NO3)

foo=cbind(foo,depthz)

#calculate textures
lstrat=length(foo$Clay)
texturez=mapply(getTexture,foo$Sand,foo$Clay,foo$Silt)
foo=cbind(foo,texturez,thicknezz)
#foo$NO3
#plot(foo$NO3,-depthz,type="l")

#grid.locator()
#log <- structure(list(Depth = foo$depthz, Nitrate = foo$NO3, Clay = foo$Clay, .Names #= c("Depth", "NO3-N", "%Clay"), class = "data.frame"))

ftextures=factor(texturez)
texture_map=c("Loam"=1,"Loamy Sand"=2,"Sandy Loam"=3,"Silt Loam"=4)
ntexture=texture_map[as.character(ftextures)]

#melting
subset=data.frame(foo$depthz,ntexture,cumsum(sintervalz)-sintervalz[1], foo$NO3,foo$Clay,foo$PH,foo$EC)
colnames(subset)=c("Depth","Texture","Thickness","NO3-N, ppm","% Clay","pH","EC, μS/cm")
melted <- melt(subset, id.vars=c('Depth','Texture','Thickness'))

#strata from bottom to top
sp <- ggplot(melted) +
    theme_bw() + 
    facet_grid(.~variable, scales='free_x') +
    geom_path(aes(x=value, y=Depth, ylab="Depth, cm")) + 
    geom_hline(yintercept=depthz)+
    labs(title='') +
    scale_y_reverse(name=("Depth, cm")) + 
    scale_x_continuous(name=("Value")) +
    facet_grid(. ~ variable, scales='free_x',labeller=vlabeller) +
    theme(strip.text.x = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x  = element_text(size=16),
          axis.title.y = element_text(vjust=-0.5,size=20),
          axis.text.y  = element_text(size=16))
sp

ggplot()+geom_bar(data=foo,aes(x=foo$depthz,fill=foo$texturez, xlab="Depth, cm"))+
  coord_flip()+
  scale_x_reverse(name="Depth, cm")

#sp=qplot(foo$NO3, depthz,geom="path")+scale_y_reverse()
#sp
#sp=qplot(foo$Clay, depthz,geom="path")+scale_y_reverse()
#sp

#Clay and NO3 are significantly positively correlated for samples with appreciable NO3
subse=foo[which(foo$NO3>2.5),]
#subse
#ee=(lm(subse$NO3~subse$Clay))
#plot(subse$NO3~subse$Clay)
#abline(ee)
#summary(ee)
#plot(ee)

