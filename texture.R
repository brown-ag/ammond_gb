#TEXTURE
#set parameters
vcs=0
cs=0
ms=0
fs=0
vfs=0
color="red"

sand=60
clay=34
silt=100-sand-clay

getTexture = function(sa,cl,si) {
  #BASIC SOIL TEXTURE calculation - adapted from http://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/?cid=nrcs142p2_054167
  if((si + 1.5*cl) <15)
    return("Sand")
  else if ((si + 1.5*cl >= 15) & (si + 2*cl < 30))
    return("Loamy Sand")
  else if ((cl >= 7 & cl < 20) & (sa > 52) & ((si + 2*cl) >= 30) | (cl < 7 & si < 50 & (si+2*cl)>=30))
    return("Sandy Loam")
  else if ((cl >= 7 & cl < 27) & (si >= 28 & si < 50) & (sa <= 52))
    return('Loam')
  else if ((si >= 50 & (cl >= 12 & cl < 27)) | ((si >= 50 & si < 80) & cl < 12))
    return('Silt Loam')
  else if (si >= 80 & cl < 12)
    return('Silt')
  else if ((cl >= 20 & cl < 35) & (si < 28) & (sa > 45))
    return('Sandy Clay Loam')
  else if ((cl >= 27 & cl < 40) & (sa > 20 & sa <= 45))
    return('Clay Loam')
  else if ((cl >= 27 & cl < 40) & (sa  <= 20))
    return('Silty Clay Loam')
  else if (cl >= 35 & sa > 45)
    return('Sandy Clay')
  else if (cl >= 40 & si >= 40)
    return('Silty Clay')
  else if (cl >= 40 & sa <= 45 & si < 40)
    return('Clay')
  else
    return('Undefined')
}

getTexture(sand,clay,silt)
