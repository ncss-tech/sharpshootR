
# requires rvest
# note: get argument names from report HTML source
#
# examples:
# url = 'https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-PROJECT_MUKEY_BY_GOAL_YEAR'
# args = list(msso='2-MIN', fy='2018', asym='%', proj='0')
parseWebReport <- function(url, args, index=1) {
  
  # sanity check: package requirements
  if(!requireNamespace('rvest'))
    stop('please install the package: rvest', call. = FALSE)
  
  # parse args and create final URL
  URLargs <- paste0('&', paste(names(args), unlist(args), sep='='), collapse='')
  url <- URLencode(paste0(url, URLargs))
  
  # get HTML
  x <- xml2::read_html(url)
  
  # sanity check: error 400 (?) -- probably bogus arguments
  
  # sanity check empty list = no data
  if(length(x) < 1)
    return(NULL)
  
  if(is.null(index)) {
    # NULL index = all tables
    d <- rvest::html_table(x, header=TRUE)
  } else {
    # extract tables if index if specified
    d <- rvest::html_table(x, header=TRUE)[[index]]
  }
  
  # TODO: col names aren't legal data.frame names
  
  # done
  return(d)
}
