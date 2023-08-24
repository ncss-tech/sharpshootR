

#' @title Create a Venn Diagram of Simulated Color Mixtures
#' 
#' @param chips character vector of standard Munsell color notation (e.g. "10YR 3/4") 
#' 
#' @param w vector of proportions, can sum to any number, must be same length as `chips`
#' 
#' @param mixingMethod approach used to simulate a mixture: see [aqp::mixMunsell()] for details
#' 
#' @param ellipse logical, use alternative ellipse-style (4 or 5 colors only) 
#' @param labels logical, print mixture labels
#' @param names logical, print names outside of the "sets"
#' @param sncs scaling factor for set names
#'
#' @export
#'
#' @return nothing returned, function is called to create graphical output
#'
#' @examples
#' 
#' \dontrun{
#' if(requireNamespace("venn") & requireNamespace("gower")) {
#' 
#' chips <- c('10YR 8/1', '2.5YR 3/6', '10YR 2/2')
#' names(chips) <- c("tan", "dark red", "dark brown")
#' 
#' colorMixtureVenn(chips)
#' colorMixtureVenn(chips, names = TRUE)
#' 
#' colorMixtureVenn(chips, w = c(1, 1, 1), names = TRUE)
#' colorMixtureVenn(chips, w = c(10, 5, 1), names = TRUE)
#' 
#' }
#' }
#' 
#' 
colorMixtureVenn <- function(chips, w = rep(1, times = length(chips))/length(chips), mixingMethod = 'exact', ellipse = FALSE, labels = TRUE, names = FALSE, sncs = 0.85) {
  
  # required package
  if(!requireNamespace('venn') | !requireNamespace("gower"))
    stop('please install the `venn` and `gower` packages', call.=FALSE)
  
  # sanity checks
  n.chips <- length(chips)
  if(n.chips < 2) {
    stop('must supply more than 2 Munsell colors', call. = FALSE)
  }
  
  # sanity check on flags
  stopifnot(inherits(labels, 'logical'))
  stopifnot(inherits(ellipse, 'logical'))
  stopifnot(inherits(names, 'logical'))
  
  # w should have same length as chips
  stopifnot(length(w) == length(chips))
  
  # sanity check on `mixingMethod` and performed by aqp::mixMunsell()
  
  # base colors
  cols <- parseMunsell(chips)
  
  # revert to standard diagram if not 4 or 5 groups
  if(! n.chips %in% c(4, 5)) {
    ellipse <- FALSE
  }
  
  # Venn diagram outlines
  if(names) {
    nm <- names(chips)
    if(is.null(nm)) {
      message('`chips` does not have a "names" attribute')
      nm <- rep('', times = n.chips)
    } 
    # include set names
    venn::venn(n.chips, snames = nm, zcolor = 'bw', box = FALSE, sncs = sncs, ellipse = ellipse)
  } else {
    # suppress default set names
    nm <- rep('', times = n.chips)
    venn::venn(n.chips, snames = nm, zcolor = 'bw', box = FALSE, ellipse = ellipse)
  }
  
  
  # add outer-most colors / no mixing
  .fillOuter(chips = chips, cols = cols, e = ellipse, labels = labels)
  
  # all combinations
  if(n.chips > 2) {
    for(i in 1:(max(n.chips) - 2)) {
      .fillCombinations(chips = chips, w = w, e = ellipse, labels = labels, degree = i, mixingMethod = mixingMethod)
    }
  }
  
  # center
  .fillCenter(chips = chips, w = w, e = ellipse, labels = labels, mixingMethod = mixingMethod)
  
}


.fillOuter <- function(chips, cols, e, labels) {
  
  # local copy
  n.chips <- length(chips)
  
  # init outer zones
  outer.zones <- vector(mode = 'character', length = n.chips)
  for(i in 1:n.chips) {
    zi <- rep('0', times = n.chips)
    zi[i] <- 1
    outer.zones[i] <- paste(zi, collapse = '')
  }
  
  # outer zones: n
  oz <- lapply(outer.zones, venn::getZones, ellipse = e)
  
  # fill outer zones
  for(i in seq_along(oz)){
    zi <- oz[[i]]
    polygon(zi[[1]], col = cols[i])
    
    # label
    if(labels) {
      ci <- venn::getCentroid(zi)[[1]]
      text(ci[1], ci[2], labels = chips[i], cex = 0.66, col = invertLabelColor(cols[i])) 
    }
    
  }
  
  
}

.fillCenter <- function(chips, w, e, labels, mixingMethod) {
  
  # local copy
  n.chips <- length(chips)
  
  # create center zone code
  center.zone <- paste(rep('1', times = n.chips), collapse = '')
  
  # center zone
  cz <- venn::getZones(center.zone, ellipse = e)
  
  # mix all colors
  all <- mixMunsell(chips, w = w, mixingMethod = mixingMethod)
  all.color <- parseMunsell(all$munsell)
  
  # fill center
  polygon(cz[[1]], col = all.color)
  
  if(labels) {
    all.centroid <- venn::getCentroid(cz)[[1]]
    text(
      x = all.centroid[1], 
      y = all.centroid[2], 
      labels = all$munsell, 
      cex = 0.66, 
      col = invertLabelColor(all.color)
    )
    
  }
  
}


.fillCombinations <- function(chips, w, e, labels, degree, mixingMethod) {
  
  # local copy
  n.chips <- length(chips)
  
  # combination table
  combinations <- t(combn(n.chips, n.chips - degree))
  chip.combinations <- t(combn(chips, n.chips - degree))
  chip.combinations <- data.frame(chip.combinations)
  
  # weight matrix for each combination
  w.idx <- t(apply(chip.combinations, 1, match, chips))
  w.mat <- t(apply(w.idx, 1, function(i) {
    w[i]
  }))
  
  # # testing:
  # cat(sprintf("Degree: %s\n", degree))
  # print(
  #   data.frame(
  #     chips,
  #     w
  #   )
  # )
  # 
  # print(chip.combinations)
  # print(w.mat)
  # cat('\n')
  
  # add mixtures
  chip.combinations$mix <- NA
  for(i in 1:nrow(chip.combinations)) {
    mi <- mixMunsell(
      x = unlist(chip.combinations[i, 1:(n.chips-degree)]), 
      w = w.mat[i, ],
      mixingMethod = mixingMethod
    )
    chip.combinations$mix[i] <- mi$munsell
  }
  
  # mixtures -> colors
  chip.combinations$col <- parseMunsell(chip.combinations$mix)
  
  # init mixed zones
  mixed.zones.idx <- t(combn(1:n.chips, m = n.chips - degree))
  mixed.zones.template <- rep("0", times = n.chips)
  
  mixed.zones <- vector(mode = 'character', length = n.chips)
  for(i in 1:nrow(mixed.zones.idx)) {
    idx.i <- mixed.zones.idx[i, ]
    template.i <- mixed.zones.template
    template.i[idx.i] <- '1'
    mixed.zones[i] <- paste(template.i, collapse = '')
  }
  
  
  # mixed zones
  mz <- lapply(mixed.zones, venn::getZones, ellipse = e)
  
  # fill mixed zones
  for(i in seq_along(mz)){
    zi <- mz[[i]]
    col.i <- chip.combinations$col[i]
    polygon(zi[[1]], col = col.i)
    
    # label
    if(labels) {
      ci <- venn::getCentroid(zi)[[1]]
      text(
        x = ci[1], 
        y = ci[2], 
        labels = chip.combinations$mix[i], 
        cex = 0.66, 
        col = invertLabelColor(col.i)
      )
    }
    
  }
  
  
}

