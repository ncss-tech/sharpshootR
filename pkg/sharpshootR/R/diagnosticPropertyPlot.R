diagnosticPropertyPlot <- function(f, v, k) {
  
  # extract site data
  s <- site(f)
  
  ## TODO: why would there be NA in here?
  # filter NA
  no.na.idx <- which(complete.cases(s[, v]))
  s <- s[no.na.idx, ]
  
  # save diagnostic properties
  m <- s[, v]
  
  # convert to factors
  m <- as.data.frame(lapply(m, factor, levels=c('FALSE', 'TRUE'), labels=c(0, 1)))
  
  # extract IDs
  m.id <- s[, c('peiid', 'pedon_id')]
  
  # make a copy of the matrix for plotting, as numerical data and transpose
  m.plot <- t(as.matrix(as.data.frame(lapply(m, as.numeric))))
  
  # compute dissimilarity between profiles
  d <- daisy(m, metric='gower')
  h.profiles <- as.hclust(diana(d))
  h.profiles$labels <- m.id$pedon_id
  p <- as.phylo(h.profiles)
  
  # cut tree at user-specified number of groups
  h.cut <- cutree(h.profiles, k=k)
  
  # setup plot layout
  layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(1,1))
  
  # get number of vars + number of profiles
  n.vars <- ncol(m)
  n.profiles <- nrow(m)
  
  ## temp
  # original order
#   image(x=1:nrow(m.plot), y=1:ncol(m.plot), z=m.plot, axes=FALSE, col=c(grey(0.9), 'RoyalBlue'), xlab='', ylab='')
#   axis(side=2, at=1:n.profiles, labels=m.id$pedon_id, las=1, cex.axis=0.75)
#   axis(side=3, at=1:n.vars, labels=dimnames(m)[[2]], las=2, cex.axis=0.75)
#   abline(h=1:(n.profiles+1)-0.5)
#   abline(v=1:(n.vars+1)-0.5)
  
  # plot profile dendrogram
  par(mar=c(1,1,6,1))
  plot(p, cex=0.75, label.offset=0.05, y.lim=c(1.5, n.profiles-0.5))
  tiplabels(pch=15, col=h.cut, cex=1.25)
  
  # get the last plot geometry
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  # vector of indices for plotting soil profiles below leaves of dendrogram
  o.profiles <- sapply(1:lastPP$Ntip, function(i) which(as.integer(lastPP$yy[1:lastPP$Ntip]) == i))
  
  # compute dissimilarity between variables
  d.vars <- daisy(as.data.frame(t(m)), metric='gower')
  h.vars <- as.hclust(diana(d.vars))
  
  # vector of variable names as plotted in dendrogram
  o.vars <- h.vars$order
  
  # plot image matrix, with rows re-ordered according to dendrogram
  par(mar=c(1,6,6,1))
  image(x=1:n.vars, y=1:n.profiles, z=m.plot[o.vars, o.profiles], axes=FALSE, col=c(grey(0.9), 'RoyalBlue'), xlab='', ylab='', ylim=c(0.5, n.profiles+0.5))
  axis(side=2, at=1:n.profiles, labels=m.id$pedon_id[o.profiles], las=1, cex.axis=0.75)
  axis(side=3, at=1:n.vars, labels=v[o.vars], las=2, cex.axis=0.75)
  abline(h=1:(n.profiles+1)-0.5)
  abline(v=1:(n.vars+1)-0.5)
  
}