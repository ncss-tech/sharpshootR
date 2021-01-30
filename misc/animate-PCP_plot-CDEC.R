library(sharpshootR)
library(av)

# https://docs.ropensci.org/av/

# define station of interest
s <- 'SPW'
# get metadata
s.info <- CDEC_StationInfo(s)
# format title for cumulative PPT
title.text <- sprintf("%s [%s]", s.info$site.meta$Name, s)

# get data
x <- CDECquery(id=s, sensor=45, interval='D', start='2000-01-01', end='2030-01-01')

# plot

m <- function() {
  lapply(1:365, function(i) {
    par(mar=c(4.5, 4.5, 2.5, 1.5))
    PCP_plot(x, ylab='Cumulative PPT (inches)', main=title.text, this.year = 2019, this.day = i)
  })
  
}


video_file <- file.path(tempdir(), 'output.mp4')
av::av_capture_graphics(m(), video_file, 1200, 650, framerate = 30, res = 90, vfilter = 'framerate=fps=30')
av::av_media_info(video_file)
utils::browseURL(video_file)
