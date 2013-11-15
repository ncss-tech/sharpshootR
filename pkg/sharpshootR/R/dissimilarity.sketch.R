# TODO: better generalization:
#         - generate box-whisker plots on re-scaled axis with custom scales
dissimilarity.sketch <- function(f.i, n.groups=2, v=c('clay', 'total_frags_pct')) {
  require(cluster)
  require(MASS)
  require(ape)
  require(reshape2)
  require(RColorBrewer)
  
  # nice colors
  cols <- brewer.pal(n=8, name='Set1')
  
  # generate an index to non-R|Cr|Cd horizons
  filter.idx <- grep('R|Cr|Cd', f.i$hzname, invert=TRUE)
  
  # establish max depth of eval, using presence of clay
  max.depth <- max(f.i, 'clay') + 10
  
  # dissimilarity matrix
  d <- profile_compare(f.i, vars=v, k=0, max_d=max.depth, rescale.result=TRUE, filter=filter.idx)
  
  # reduce to 2D: fudge 0-distances by adding a little bit
  s <- isoMDS(d+0.001)
  # generate plotting order as Euclidean distance to origin
  new.order <- order(sqrt(rowSums(sweep(s$points, MARGIN=2, STATS=c(0, 0), FUN='-')^2)))
  
  # convert distance matrix into dendrogram
  h <- as.hclust(diana(d))
  p <- as.phylo(h)
  
  # cut into groups
  g <- cutree(h, n.groups)
  # merge into site data
  site(f.i) <- data.frame(pedon_id=names(g), group=g, stringsAsFactors=FALSE)
  
  # subset variable + group for box-whisker plot
  m <- melt(as(f.i, 'data.frame'), id.vars='group', measure.vars=c(v))
  m$group <- factor(m$group)
  m$variable <- factor(m$variable)
  
  # setup layout
  layout(matrix(c(1,2,3,3), byrow=TRUE, nrow=2), heights=c(1, 1.5))
  
  # dendrogram
  par(mar=c(0.125,0.25,0.25,0.25))
  plot(p, label.offset=0.01, x.lim=c(0,0.75), font=1, cex=0.75)
  tiplabels(pch=15, col=cols[g])
  
  # box-whisker plot
  par(mar=c(0.125,0.75,0.25,0.25))
  boxplot(value ~ variable, data=m, horizontal=TRUE, las=1, cex.axis=0.75, xlab='Percent', boxwex=0.5, border=grey(0.5), axes=FALSE)
  text(x=tapply(m$value, m$variable, median, na.rm=TRUE), y=(1:2)+0.35, levels(m$variable), cex=1, font=2, pos=4)
  points(m$value, jitter(as.numeric(m$variable)), col=cols[m$group], pch=0)
  axis(side=1, line=-1.5, cex.axis=0.75, padj=-1)
  
  # lower panel: profiles
  par(mar=c(0.25,0.25,0.25,2))
  plot(f.i, name='hzname', id.style='side', cex.depth.axis=1.125, cex.id=0.8, cex.names=0.65, plot.order=new.order, max.depth=max.depth, axis.line.offset=-3.5)
  segments(x0=0.5, y0=c(50, 75, 100, 150), x1=length(f.i) + 1.5, y1=c(50, 75, 100, 150), lty=3, col=rgb(0, 0, 0, alpha=0.25))
  points(x=1:length(f.i), y=rep(-2.5, times=length(f.i)), pch=15, col=cols[g[new.order]], cex=1.5)
}
