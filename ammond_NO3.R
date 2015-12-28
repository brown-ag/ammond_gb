library(ggplot2)
library(reshape2)
library(grid)

x11()
#your data, note changed field names
foo=read.csv("para_15-10_samples.csv",stringsAsFactors=FALSE)
corez=read.csv("para_15-10_cores.csv",stringsAsFactors=FALSE)

#create depths scaled by % recovery
scalez=corez$rec[foo$CORE]
intervalz=foo$BDEPTH-foo$TDEPTH
sintervalz=intervalz/scalez
depthz=cumsum(sintervalz)
#convert Non-detectable into zero values
foo$NO3[which(foo$NO3=="ND")] = 0.0
foo$NO3=as.numeric(foo$NO3)
#foo$NO3
#plot(foo$NO3,-depthz,type="l")

#grid.locator()
#log <- structure(list(Depth = depthz, Nitrate = foo$NO3, Clay = foo$Clay, .Names = c("Depth", 
#"Nitrate", "Clay"), class = "data.frame"))

#melting
#melted <- melt(log, id.vars='Depth')
#melted
#use of colsplit
#melted[, 'Var'] <- colsplit(melted$variable, '_', c('Var'))
#melted$Well <- as.factor(melted$Well)
#melted$Var

#sp <- ggplot(foo, aes(x=foo$NO3, y=depthz)) +
#    theme_bw() + 
#    geom_path(aes(linetype=1)) + 
#    labs(title='') +
#    scale_y_reverse() #+ 
#    #facet_grid(. ~ foo$NO3, scales='free_x')
sp=qplot(foo$NO3, depthz,geom="path")+scale_y_reverse()
sp
sp=qplot(foo$Clay, depthz,geom="path")+scale_y_reverse()
sp

#Clay and NO3 are significantly positively correlated for samples with appreciable NO3
subse=foo[which(foo$NO3>2.5),]
#subse
ee=(lm(subse$NO3~subse$Clay))
plot(subse$NO3~subse$Clay)
abline(ee)
summary(ee)
#plot(ee)

grid.locator()
