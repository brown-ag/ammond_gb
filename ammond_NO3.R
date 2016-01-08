library(ggplot2)
library(reshape2)
library(plyr)
library(grid)
library(aqp)

#x11()
foo=read.csv("para_15-10_samples.csv",stringsAsFactors=FALSE)
corez=read.csv("para_15-10_cores.csv",stringsAsFactors=FALSE)

#create sampled depth intervals scaled by Geoprobe % recovery
scalez=corez$rec[foo$CORE]
intervalz=foo$BDEPTH-foo$TDEPTH
sintervalz=intervalz/scalez
#calculate boundaries of sampled intervals
boundz=cumsum(sintervalz)
boundz2=c(0,boundz)
uboundz=boundz2[1:length(boundz)-1]
lboundz=boundz2[2:length(boundz)]
#calculate midpoints (for soil property plots)
depthz=c()
last=0
for(b in boundz[1:20]) {
  depthz=c(depthz,b-((b-last)/2))
  last=b
}
#depthz # midpoints of each sampled interval, corrected for # recovery
foo=cbind(foo,depthz)

#convert non-detectable NO3 values into zero values
foo$NO3[which(foo$NO3=="ND")] = 0.0
foo$NO3=as.numeric(foo$NO3)

#calculate textures
lstrat=length(foo$Clay)
texturez=mapply(getTexture,foo$Sand,foo$Clay,foo$Silt)
foo=cbind(foo,texturez,thicknezz)
#foo$NO3
#plot(foo$NO3,-depthz,type="l")

#grid.locator()
#log <- structure(list(Depth = foo$depthz, Nitrate = foo$NO3, Clay = foo$Clay, .Names #= c("Depth", "NO3-N", "%Clay"), class = "data.frame"))

ftextures=factor(texturez,levels=c("Loamy Sand","Sandy Loam", "Loam", "Silt Loam"))
texture_map=c("Loamy Sand"=1,"Sandy Loam"=2,"Loam"=3,"Silt Loam"=4)
plot_texture_map=(1="Dotted",)
ntexture=texture_map[as.character(ftextures)]

#melting
subset=data.frame(foo$depthz,ftextures,sintervalz, foo$NO3,foo$Clay,foo$PH,foo$EC,rep(0,length(ntexture)))
colnames(subset)=c("Depth","Texture2","Thickness","no3","clay","ph","ec","texture")
melted <- melt(subset, id.vars=c('Depth','Texture2','Thickness'))

mround <- function(x,base){ 
  base*round(x/base) 
} 

equal_breaks <- function(n,s,x){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    round(seq(min(x)+d, max(x)-d, length=n),digits=1)
  }
}

blank_data=data.frame(variable=c("N","N",'C','C',"P","P","E","E","T","T"),Depth=0,value=c(0,25,0,25,5,9,0,500,1,2))
#strata from bottom to top
#melted$Thickness = melted$Thickness/max(melted$Depth)
label_map=list("no3"=bquote("NO"[3]*"-N, ppm"),"clay"="% Clay","ph"="pH","ec"="EC, μS/cm","texture"="Texture")
vlabeller <- function(variable,value){
    return(label_map[value])
}
sp <- ggplot() +
  theme_bw() + 
  geom_bar(data=subset(melted,variable=="texture"),aes(x=value,y=Thickness,ylab="Depth, cm",fill=Texture2,xmin=-0.5,xmax=0.5,ymin=0,ymax=400),stat="identity")+ 
  scale_fill_brewer("USDA Soil\nTextural\nClass",direction=1,palette="YlGn")+ 
  geom_path(data=subset(melted,variable!="texture"),aes(x=value, y=Depth, ylab="Depth, cm"))+
  #geom_blank(data = blank_data, aes(x = value, y = Depth)) +
  facet_grid(.~variable, scales='free_x',space='fixed',labeller=vlabeller) +
  expand_limits(x=c(0,0))+
  geom_hline(yintercept=c(0,boundz),linetype="dashed",colour="#AAAAAA")+
  labs(title='') +
  scale_y_reverse(name=("Depth, cm"),expand=c(0,0)) + 
  scale_x_continuous(name=("Value"),expand=c(0,0))+#,breaks=equal_breaks(n=4,s=0.1)) +
  theme(strip.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(vjust=-0.5,size=14),
        axis.text.y  = element_text(size=12),
        panel.margin = unit(1, "lines"))
g1=ggplotGrob(sp)
g1$grobs[[12]]
g1$grobs[[17]]<- nullGrob() #Remove X axis on texture stacked bar
grid.draw(g1)

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

