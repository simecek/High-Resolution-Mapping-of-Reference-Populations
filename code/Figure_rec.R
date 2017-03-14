library(dplyr)
library(ggplot2)
library(gridExtra)
library(RSkittleBrewer)

rec <- read.csv("data/rec.together.csv", as.is=TRUE)
hist(log10(rec$to - rec$from))

geno.files <- list("AXB" = "C:/Users/petrs/Dropbox/High Resolution Mapping of Reference Populations/axb/geno.axb.csv",
                   "BXD" = "C:/Users/petrs/Dropbox/High Resolution Mapping of Reference Populations/bxd/geno.bxd.csv",
                   "LXS" = "C:/Users/petrs/Dropbox/High Resolution Mapping of Reference Populations/lxs/geno.lxs.csv")
# colors <- c("AXB" = "red", "BXD" = "green", "LXS" = "blue")
colors <- RSkittleBrewer('original')[1:3]
names(colors) <- names(geno.files)

panels <- names(geno.files)
output <- NULL
plots <- list()

for (sel.panel in panels) {
  print(sel.panel)
  prec <- subset(rec, panel == sel.panel)
  
  geno = read.csv(geno.files[[sel.panel]], as.is=TRUE)
  names(geno)[names(geno)=="chrId"] <- "chr"
  
  tmp <- geno %>%
    mutate(block = positionBp %/% 1e7) %>%
    group_by(chr, block) %>%
    summarise(Nsnps = n())
  
  
  tmp2 <- prec %>%
    mutate(block = (from+to) %/% 2e7) %>%
    group_by(chr, block) %>%
    summarise(Nrec = n())
  
  dt <- tmp %>% 
    left_join(tmp2, by=c("chr", "block")) %>%
    filter(Nsnps > 250) %>%
    mutate(panel = sel.panel)
  dt$Nrec[is.na(dt$Nrec)] <- 0
  
  pval = cor.test(dt$Nrec, dt$Nsnps)$p.value
  ptext = paste("p-value =", round(pval,3))
  
  plots[[sel.panel]] <- ggplot(dt, aes(x=Nsnps, y=Nrec, group=panel)) +
    geom_point(color = colors[sel.panel]) +
    geom_smooth(method="lm", se=FALSE, color="grey75") +
    labs(title = sel.panel, subtitle = ptext) +
    theme_minimal()
  output <- rbind(output, dt)
}

pdf("FigureS4.pdf", width=9, height=9)
grid.arrange(plots[["AXB"]] + geom_point(color = colors["AXB"]),
             plots[["BXD"]] + geom_point(color = colors["BXD"]),
             plots[["LXS"]] + geom_point(color = colors["LXS"]),
             ncol = 2)
dev.off()
