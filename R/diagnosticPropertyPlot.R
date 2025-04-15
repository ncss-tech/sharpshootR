## TODO: figure out a better approach for alignment of dendrogram / image axis labels
## TODO: fix sort.vars behavior...



#' @title Diagnostic Property Plot (base graphics)
#' 
#' @description Generate a graphical description of the presence/absence of soil diagnostic properties.
#'
#' @param f `SoilProfileCollection` object
#' @param v character vector of site-level attribute names of `logical` type
#' @param k an integer, number of groups to highlight
#' @param grid.label the name of a site-level attribute (usually unique) annotating the y-axis of the grid
#' @param dend.label the name of a site-level attribute (usually unique) annotating dendrogram terminal leaves
#' @param sort.vars sort variables according to natural clustering (`TRUE`), or use supplied ordering in `v`
#'
#' @details This function attempts to display several pieces of information within a single figure. First, soil profiles are sorted according to the presence/absence of diagnostic features named in `v`. Second, these diagnostic features are sorted according to their distribution among soil profiles. Third, a binary grid is established with row-ordering of profiles based on step 1 and column-ordering based on step 2. Blue cells represent the presence of a diagnostic feature. Soils with similar diagnostic features should 'clump' together. See examples below.
#' 
#' @return a `list` is silently returned by this function, containing:
#' \describe{
#'   \item{\code{rd}}{a \code{data.frame} containing IDs and grouping code}
#'   \item{\code{profile.order}}{a vector containing the order of soil profiles (row-order in figure), according to diagnostic property values}
#'   \item{\code{var.order}}{a vector containing the order of variables (column-order in figure), according to their distribution among profiles}
#' }
#' 
#' @seealso [multinominal2logical()]
#' 
#' @keywords hplots
#' 
#' @author D.E. Beaudette and J.M. Skovlin
#' 
#' @export
#'
diagnosticPropertyPlot <- function(f, v, k, grid.label = 'upedonid', dend.label = 'upedonid', sort.vars = TRUE) {
  
  # setup colors
  if(k <= 9 & k > 2) 
    cols <- brewer.pal(n = k, name='Set1') 
  if(k < 3) 
    cols <- brewer.pal(n = 3, name='Set1')
  if(k > 9)
    cols <- colorRampPalette(brewer.pal(n = 9, name = 'Set1'))(k)
  
  # get internal, unique ID
  id <- idname(f)
  
  # extract site data
  s <- site(f)
  
  # keep only those variables that exist
  v <- names(s)[na.omit(match(v, names(s)))]
  
  ## TODO: why would there be NA in here?
  # filter NA
  no.na.idx <- which(complete.cases(s[, v]))
  s <- s[no.na.idx, ]
  
  # save diagnostic properties
  m <- s[, v]
  
  # optionally check for any vars that are all FALSE and kick them out
  vars.not.missing <- apply(m, 2, any)
  
  # if any are all FALSE, then remove from m and v
  if(any(!vars.not.missing)) {
    not.missing <- which(vars.not.missing)
    m <- m[, not.missing]
    v <- v[not.missing]
  }
  
  # convert to factors, we have to specify the levels as there are cases with all TRUE or FALSE
  m <- as.data.frame(lapply(m, factor, levels = c('FALSE', 'TRUE')))
  
  # make a copy of the matrix for plotting, as numerical data and transpose
  m.plot <- t(as.matrix(as.data.frame(lapply(m, as.numeric))))
  
  # compute dissimilarity between profiles
  d <- daisy(m, metric = 'gower')
  h.profiles <- as.hclust(diana(d))
  # store text labels for dendrogram
  h.profiles$labels <- as.character(s[[dend.label]]) # factors will break tiplabels()
  p <- as.phylo(h.profiles)
  
  # cut tree at user-specified number of groups
  h.cut <- cutree(h.profiles, k = k)
  
  # setup plot layout
  layout(matrix(c(1, 2), nrow = 1, ncol = 2), widths = c(1, 1))
  
  # get number of vars + number of profiles
  n.vars <- ncol(m)
  n.profiles <- nrow(m)
    
  # device options are modified locally, reset when done
  # warning: this will reset the device coordinates!
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  # plot dendrogram
  par(mar = c(0.5, 1, 5.5, 1))
  
  ### possible fix?
  # setup plotting region
  # plot(1,1, type='n', axes=FALSE, xlab='', ylab='', ylim=c(0.5, n.profiles+0.5))
  # par(new=TRUE)
  
  plot(p, cex = 0.75, label.offset = 0.05, y.lim = c(1.125, n.profiles))
  tiplabels(pch = 15, col = cols[h.cut], cex = 1.125, adj = 0.52)
  
  ### debug:
#   par(xpd=TRUE)
#   abline(h=seq(1, n.profiles))
#   par(xpd=FALSE)
#   
  ## note: transpose converts logical -> character, must re-init factors
  # compute dissimilarity between variables
  d.vars <- daisy(data.frame(t(m), stringsAsFactors = TRUE), metric = 'gower')
  h.vars <- as.hclust(diana(d.vars))
  
  # order of profiles in dendrogram
  o.profiles <- h.profiles$order
  
  ## TODO this isn't working as expected
  # vector of variable names as plotted in dendrogram
  if(sort.vars)
    o.vars <- h.vars$order
  else
    o.vars <- 1:length(v)
  
  # plot image matrix, with rows re-ordered according to dendrogram
  par(mar = c(1, 6, 6, 1))
  image(x = 1:n.vars, y = 1:n.profiles, z = m.plot[o.vars, o.profiles], axes = FALSE, col = c(grey(0.9), 'RoyalBlue'), xlab = '', ylab = '', ylim = c(0.5, n.profiles + 0.5))
  axis(side = 2, at = 1:n.profiles, labels = s[[grid.label]][o.profiles], las = 1, cex.axis = 0.75, tick = FALSE)
  axis(side = 3, at = 1:n.vars, labels = v[o.vars], las = 2, cex.axis = 0.75)
  abline(h = 1:(n.profiles + 1) - 0.5)
  abline(v = 1:(n.vars + 1) - 0.5)
  
  # plot outside of plotting region
  par(xpd = TRUE)
  
  # this may require some tinkering
  points(x = rep(0.35, times = n.profiles), y = 1:n.profiles, col = cols[h.cut][o.profiles], pch = 15)
  par(xpd = FALSE)
  
  # return values
  rd <- cbind(s[, c(id, grid.label)], g = h.cut)
  return(invisible(list(rd = rd, profile.order = o.profiles, var.order = o.vars)))
}



#' @title Diagnostic Property Plot (lattice)
#' 
#' @description Generate a graphical description of the presence/absence of soil diagnostic properties.
#'
#' @param f `SoilProfileCollection` object
#' @param v character vector of site-level attribute names of `logical` type
#' @param k an integer, number of groups to highlight
#' @param grid.label the name of a site-level attribute (usually unique) annotating the y-axis of the grid
#' @param sort.vars sort variables according to natural clustering (`TRUE`), or use supplied ordering in `v`
#'
#' @details This function attempts to display several pieces of information within a single figure. First, soil profiles are sorted according to the presence/absence of diagnostic features named in `v`. Second, these diagnostic features are sorted according to their distribution among soil profiles. Third, a binary grid is established with row-ordering of profiles based on step 1 and column-ordering based on step 2. Blue cells represent the presence of a diagnostic feature. Soils with similar diagnostic features should 'clump' together. See examples below.
#' 
#' @return a `list` is silently returned by this function, containing:
#' \describe{
#'   \item{\code{rd}}{a \code{data.frame} containing IDs and grouping code}
#'   \item{\code{profile.order}}{a vector containing the order of soil profiles (row-order in figure), according to diagnostic property values}
#'   \item{\code{var.order}}{a vector containing the order of variables (column-order in figure), according to their distribution among profiles}
#' }
#' 
#' @seealso \code{\link{multinominal2logical}}
#' 
#' @keywords hplots
#' 
#' @author D.E. Beaudette and J.M. Skovlin
#' 
#' @export
#'
#' 
diagnosticPropertyPlot2 <- function(f, v, k, grid.label='upedonid', sort.vars=TRUE) {
  
  # sanity check: package requirements
  if(!requireNamespace('latticeExtra', quietly=TRUE))
    stop('please install the `latticeExtra` package', call. = FALSE)
  
  # setup colors
  if(k <= 9 & k > 2) 
    cols <- brewer.pal(n=k, name='Set1') 
  if(k < 3) 
    cols <- brewer.pal(n=3, name='Set1')
  if(k > 9)
    cols <- colorRampPalette(brewer.pal(n=9, name='Set1'))(k)
  
  
  # get internal, unique ID
  id <- idname(f)
  
  # extract site data
  s <- site(f)
  
  # keep only those variables that exist
  v <- names(s)[na.omit(match(v, names(s)))]
  
  ## TODO: why would there be NA in here?
  # filter NA
  no.na.idx <- which(complete.cases(s[, v]))
  s <- s[no.na.idx, ]
  
  # save grid labels
  s.gl <- as.character(s[[grid.label]])
  
  # save diagnostic properties
  m <- s[, v]
  
  # optionally check for any vars that are all FALSE and kick them out
  vars.not.missing <- apply(m, 2, any)
  
  # if any are all FALSE, then remove from m and v
  if(any(!vars.not.missing)) {
    not.missing <- which(vars.not.missing)
    m <- m[, not.missing]
    v <- v[not.missing]
  }
  
  # convert to factors, we have to specify the levels as there are cases with all TRUE or FALSE
  m <- as.data.frame(lapply(m, factor, levels=c('FALSE', 'TRUE')))
  
  # get number of vars + number of profiles
  n.vars <- ncol(m)
  n.profiles <- nrow(m)
  
  # compute dissimilarity between profiles
  d <- daisy(m, metric='gower')
  h.profiles <- as.hclust(diana(d))
  
  ## note: transpose converts logical -> character, must re-init factors
  # compute dissimilarity between variables
  d.vars <- daisy(data.frame(t(m), stringsAsFactors=TRUE), metric='gower')
  h.vars <- as.hclust(diana(d.vars))
  
  # cut tree at user-specified number of groups
  h.cut <- cutree(h.profiles, k=k)
  
  # format for plotting
  m.plot <- data.frame(id=s[[id]], m, stringsAsFactors=FALSE)
  m.plot.long <- melt(m.plot, id.vars='id')
  # convert TRUE/FALSE into factor
  m.plot.long$value <- factor(m.plot.long$value, levels=c('FALSE', 'TRUE'))
  
  # order of profiles in dendrogram
  o.profiles <- h.profiles$order
  
  ## TODO this isn't working as expected
  # vector of variable names as plotted in dendrogram
  if(sort.vars)
    o.vars <- h.vars$order
  else
    o.vars <- 1:length(v)
  
  # set factor levels for ordering of level plot
  m.plot.long$id <- factor(m.plot.long$id, levels=m.plot$id[o.profiles])
  m.plot.long$variable <- factor(m.plot.long$variable, levels=v[o.vars])
  
  # lattice plot
  p <- levelplot(value ~ variable * id, data=m.plot.long,
  col.regions=c(grey(0.9), 'RoyalBlue'), cuts=1, xlab='', ylab='', 
  colorkey = FALSE, 
  scales=list(tck=0, x=list(rot=90), y=list(at=1:length(o.profiles), labels=s.gl[o.profiles])),
  legend=list(
      right=list(fun=latticeExtra::dendrogramGrob, args=list(x = as.dendrogram(h.profiles), side="right", size=10, add=list(
        rect=list(fill=cols[h.cut])))),
      top=list(fun=latticeExtra::dendrogramGrob, args=list(x=as.dendrogram(h.vars), side="top", size=4))
      ),
  panel=function(...) {
    panel.levelplot(...)
    # horizontal lines
    panel.segments(x0=0.5, y0=1:(n.profiles+1)-0.5, x1=n.vars+0.5, y1=1:(n.profiles+1)-0.5)
    # vertical lines
    panel.segments(x0=1:(n.vars+1)-0.5, y0=0.5, x1=1:(n.vars+1)-0.5, y1=n.profiles + 0.5)
  }
  )
  
  # print to graphics device
  print(p)
  
  # return values
  rd <- cbind(s[, c(id, grid.label)], g=h.cut)
  return(invisible(list(rd=rd, profile.order=o.profiles, var.order=o.vars)))
}


## failed attempt to include multi-nominal variables
## not going to work with current implementation, mostly due to how colors are mapped to values in image()

# diagnosticPropertyPlot3 <- function(f, v, k, grid.label='upedonid', dend.label='pedon_id') {
#   
#   # get internal, unique ID
#   id <- idname(f)
#   
#   # extract site data
#   s <- site(f)
#   
#   # keep only those variables that exist
#   v <- names(s)[na.omit(match(v, names(s)))]
#   
#   ## TODO: why would there be NA in here?
#   # filter NA
#   no.na.idx <- which(complete.cases(s[, v]))
#   s <- s[no.na.idx, ]
#   
#   # save diagnostic properties
#   m <- s[, v]
#   
#   # keep track of binary / multinominal variables
#   binary.vars <- which(sapply(m, class) == 'logical')
#   multinom.vars <- which(sapply(m, class) == 'factor')
#   
#   # setup colors:
#   # binary colors, then 'NA' repeated for number of levels in any multinominal data
#   binary.cols <- c(grey(0.9), 'RoyalBlue')
#   multinom.cols <- rep(NA, times=length(levels(m[, multinom.vars])))
#   cols <- c(binary.cols, multinom.cols)
#   
#   # split: multinominal data are assumed to be a factor
#   m.binary <- m[, binary.vars, drop=FALSE]
#   if(length(multinom.vars) > 0)
#     m.multinom <- m[, multinom.vars, drop=FALSE]
#   
#   # optionally check for any vars that are all FALSE and kick them out
#   vars.not.missing <- sapply(m.binary, any)
#   
#   # if any are all FALSE, then remove from m and v
#   if(any(!vars.not.missing)) {
#     not.missing <- which(vars.not.missing)
#     m.binary <- m.binary[, not.missing]
#   }
#   
#   # convert binary data into factors, we have to specify the levels as there are cases with all TRUE or FALSE
#   m.binary <- as.data.frame(lapply(m.binary, factor, levels=c('FALSE', 'TRUE')))
#   
#   # merge binary + multinominal data
#   if(length(multinom.vars) > 0)
#     m <- cbind(m.binary, m.multinom)
#   else
#     m <- m.binary
#   
#   # update variable names, in case any were removed due to missingness
#   v <- names(m)
#   
#   # make a copy of the matrix for plotting, as numerical data and transpose
#   m.plot <- t(as.matrix(as.data.frame(lapply(m, as.numeric))))
#   
#   # compute dissimilarity between profiles
#   d <- daisy(m, metric='gower')
#   h.profiles <- as.hclust(diana(d))
#   # store text labels for dendrogram
#   h.profiles$labels <- as.character(s[[dend.label]]) # factors will break tiplabels()
#   p <- as.phylo(h.profiles)
#   
#   # cut tree at user-specified number of groups
#   h.cut <- cutree(h.profiles, k=k)
#   
#   # setup plot layout
#   layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(1,1))
#   
#   # get number of vars + number of profiles
#   n.vars <- ncol(m)
#   n.profiles <- nrow(m)
#   
#   # plot profile dendrogram
#   par(mar=c(1,1,6,1))
#   plot(p, cex=0.75, label.offset=0.05, y.lim=c(1.125, n.profiles))
#   tiplabels(pch=15, col=h.cut, cex=1.125, adj=0.52)
#   
#   ## note: transpose converts logical -> character, must re-init factors
#   # compute dissimilarity between variables
#   d.vars <- daisy(data.frame(t(m), stringsAsFactors=TRUE), metric='gower')
#   h.vars <- as.hclust(diana(d.vars))
#   
#   # order of profiles in dendrogram
#   o.profiles <- h.profiles$order
#   
#   # vector of variable names as plotted in dendrogram
#   o.vars <- h.vars$order
#   
#   # plot image matrix, with rows re-ordered according to dendrogram
#   par(mar=c(1,6,6,1))
#   image(x=1:n.vars, y=1:n.profiles, z=m.plot[o.vars, o.profiles], axes=FALSE, col=cols, xlab='', ylab='', ylim=c(0.5, n.profiles+0.5))
#   axis(side=2, at=1:n.profiles, labels=s[[grid.label]][o.profiles], las=1, cex.axis=0.75)
#   axis(side=3, at=1:n.vars, labels=v[o.vars], las=2, cex.axis=0.75)
#   abline(h=1:(n.profiles+1)-0.5)
#   abline(v=1:(n.vars+1)-0.5)
#   
#   # return values
#   rd <- cbind(s[, c(id, grid.label)], g=h.cut)
#   return(invisible(list(rd=rd, profile.order=o.profiles, var.order=o.vars)))
# }

