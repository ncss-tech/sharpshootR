# f: SPC with diagnostic boolean variables
# v: named variables
# k: number of groups to highlight
# id: id to print next to dendrogram
diagnosticPropertyPlot <- function(f, v, k, id='pedon_id') {
  
  # extract site data
  s <- site(f)
  
  ## TODO: why would there be NA in here?
  # filter NA
  no.na.idx <- which(complete.cases(s[, v]))
  s <- s[no.na.idx, ]
  
  # save diagnostic properties
  m <- s[, v]
  
  # convert to factors
  m <- as.data.frame(lapply(m, factor))
  
  # make a copy of the matrix for plotting, as numerical data and transpose
  m.plot <- t(as.matrix(as.data.frame(lapply(m, as.numeric))))
  
  # compute dissimilarity between profiles
  d <- daisy(m, metric='gower')
  h.profiles <- as.hclust(diana(d))
  h.profiles$labels <- s[[id]]
  p <- as.phylo(h.profiles)
  
  # cut tree at user-specified number of groups
  h.cut <- cutree(h.profiles, k=k)
  
  # setup plot layout
  layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(1,1))
  
  # get number of vars + number of profiles
  n.vars <- ncol(m)
  n.profiles <- nrow(m)
    
  # plot profile dendrogram
  par(mar=c(1,1,6,1))
  plot(p, cex=0.75, label.offset=0.05, y.lim=c(1.5, n.profiles-0.5))
  tiplabels(pch=15, col=h.cut, cex=1.125, adj=0.52)
  
  # order of profiles in dendrogram
  o.profiles <- h.profiles$order
  
  # compute dissimilarity between variables
  d.vars <- daisy(as.data.frame(t(m)), metric='gower')
  h.vars <- as.hclust(diana(d.vars))
  
  # vector of variable names as plotted in dendrogram
  o.vars <- h.vars$order
#   o.vars <- 1:length(v)
  
  # plot image matrix, with rows re-ordered according to dendrogram
  par(mar=c(1,6,6,1))
  image(x=1:n.vars, y=1:n.profiles, z=m.plot[o.vars, o.profiles], axes=FALSE, col=c(grey(0.9), 'RoyalBlue'), xlab='', ylab='', ylim=c(0.5, n.profiles+0.5))
  axis(side=2, at=1:n.profiles, labels=s$pedon_id[o.profiles], las=1, cex.axis=0.75)
  axis(side=3, at=1:n.vars, labels=v[o.vars], las=2, cex.axis=0.75)
  abline(h=1:(n.profiles+1)-0.5)
  abline(v=1:(n.vars+1)-0.5)
  
  ## TODO: back-fill with any missing values?
  # return values
  rd <- cbind(s[, c('peiid', id)], g=h.cut)
  return(invisible(list(rd=rd, profile.order=o.profiles, var.order=o.vars)))
}