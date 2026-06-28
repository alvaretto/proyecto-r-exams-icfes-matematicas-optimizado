library(ggplot2)
library(grid)

png("output_r_v1.png", width = 800, height = 600, bg = "white", res = 100)
grid.newpage()

slate900 <- "#0F172A"; slate800 <- "#1E293B"; slate600 <- "#475569"
teal <- "#0F766E"; orange <- "#EA580C"

vp <- viewport(xscale = c(0, 800), yscale = c(600, 0))
pushViewport(vp)
grid.rect(gp = gpar(fill = "white", col = NA))

line <- function(x1,y1,x2,y2,col=slate600,lwd=1.5) grid.lines(unit(c(x1,x2), "native"), unit(c(y1,y2), "native"), gp=gpar(col=col,lwd=lwd,lineend="round"))
text <- function(label,x,y,size=13,col=slate600,just="centre",font=1) grid.text(label, unit(x,"native"), unit(y,"native"), gp=gpar(col=col, fontsize=size, fontface=font), just=just)
circ <- function(x,y,r,col) grid.circle(unit(x,"native"), unit(y,"native"), r=unit(r,"native"), gp=gpar(fill=col,col=col))
poly <- function(xs,ys,col) grid.polygon(unit(xs,"native"), unit(ys,"native"), gp=gpar(fill=col,col=col))

arrowheads <- function(cx,cy){
  poly(c(cx,cx-4,cx+4), c(cy-116,cy-108,cy-108), slate600)
  poly(c(cx,cx-4,cx+4), c(cy+116,cy+108,cy+108), slate600)
  poly(c(cx-116,cx-108,cx-108), c(cy,cy-4,cy+4), slate600)
  poly(c(cx+116,cx+108,cx+108), c(cy,cy-4,cy+4), slate600)
}
axes <- function(cx,cy,side="left"){
  line(cx-108,cy,cx+108,cy); line(cx,cy-108,cx,cy+108); arrowheads(cx,cy)
  text("N", cx, cy-125, 15, slate800, font=2)
  if(side=="right") text("Aeropuerto", cx+12, cy-7, 13, slate600, just="left") else text("Aeropuerto", cx-12, cy-7, 13, slate600, just="right")
  circ(cx,cy,4,slate600)
}
arc_poly <- function(cx,cy,r,start,end){
  th <- seq(start,end,length.out=40)*pi/180
  grid.lines(unit(cx + r*cos(th), "native"), unit(cy - r*sin(th), "native"), gp=gpar(col=orange,lwd=1.5))
}
diag <- function(label,cx,cy,ex,ey,dist,side="left",angle="north",vx=NULL,px=NULL){
  text(paste0(label,"."), cx-155, cy-105, 24, slate900, just="left", font=2)
  axes(cx,cy,side)
  line(cx,cy,ex,ey,teal,2.5); circ(ex,ey,5,orange)
  if(angle=="north") { arc_poly(cx,cy,25,40,90); text("50°",cx-6,cy-22,13,orange,font=2) } else { arc_poly(cx,cy,25,220,270); text("50°",cx-4,cy+32,13,orange,font=2) }
  if(is.null(px)) px <- list(ex, ey-14, "centre"); if(is.null(vx)) vx <- list(ex+12, ey+13, "left")
  text("Avión", px[[1]], px[[2]], 13, slate600, just=px[[3]])
  text(dist, vx[[1]], vx[[2]], 13, teal, just=vx[[3]], font=2)
}

diag("A",200,160,169.4,185.7,"30 km","right","south",list(148,198,"right"),list(148,216,"right"))
diag("B",600,160,630.6,134.3,"30 km","left","north",list(644,142,"left"),list(644,126,"left"))
diag("C",200,440,257.5,391.8,"70 km","left","north",list(270,405,"left"),list(257.5,378,"centre"))
diag("D",600,440,691.9,362.9,"130 km","left","north",list(704,376,"left"),list(691.9,349,"centre"))
popViewport()
dev.off()
