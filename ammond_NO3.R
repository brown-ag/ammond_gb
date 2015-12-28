library(ggplot2)
library(reshape2)
library(grid)

#x11()
#your data, note changed field names
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

#melting
subset=data.frame(foo$depthz,foo$NO3,foo$Clay)#,foo$PH,foo$EC)
colnames(subset)=c("Depth","NO3","Clay")#,"pH","EC, uS/cm")
melted <- melt(subset, id.vars='Depth')

vnames <-list(
  'NO3' = bquote("NO"[3]*"-N, ppm"),
  'Clay' = '% Clay')

vlabeller <- function(variable,value){
  return(vnames[value])
}

sp <- ggplot(melted, x=value, y=Depth) +
    theme_bw() + 
    geom_path(aes(value,Depth,ylab="Depth, cm")) + 
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

#Clay and NO3 are significantly positively correlated for samples with appreciable NO3
subse=foo[which(foo$NO3>2.5),]
#subse
#ee=(lm(subse$NO3~subse$Clay))
#plot(subse$NO3~subse$Clay)
#abline(ee)
#summary(ee)
#plot(ee)

