x<-1
xtotal<-2
y<-1
ytotal<-2
prob <-1/2
  
entropy_calc <- function(x,y,xtotal,ytotal){
xt <- -(x/xtotal)*log2(x/xtotal)
yt <- -(y/ytotal)*log2(y/ytotal)

if (is.nan(xt)){
  entropy <- yt
} else if (is.nan(yt)) {
  entropy <- xt
} else {
  entropy <- xt + yt
}
return (entropy)
}

conditional_ent <- function(fp1,fp2,p1,p2){
  val=(fp1*p1)+(fp2*p2)
  return (val);
}

v <-entropy_calc(x,y,xtotal,ytotal)
print(v)
v*prob
#entropy_calc(4,1,5,5)
#conditional_ent(3/8,5/8,0,0.7219)