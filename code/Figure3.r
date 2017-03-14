load("Figure3.RData")

pdf("Figure3.pdf", width=6, height=6)
  plot(x=as.numeric(xi), y=as.numeric(yi), main=ti, type="n", xlim=c(-1,1), ylim=c(7,13),
     xlab = "Intensity Contrast", ylab="Intensity Average")
  text(x=as.numeric(xi), y=as.numeric(yi), as.character(vi), cex=0.4, col=pal[as.character(vi)])  
  text(x=contj, y=avgj, texti, cex=0.8)
  coveredj = texti == "13"
  points(x=contj[coveredj], y=avgj[coveredj], pch=19, col="red", cex=1.6)
dev.off()