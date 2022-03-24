d <- NULL

test_that("formatPLSS() works", {
  d <- data.frame(id = 1:6, plssid = formatPLSS(data.frame(
    id = 1:6,
    qq = c("", "", "NE", "NW", "SE", "SW"),
    q = c("", "", "NE", "NE", "NE", "NE"),
    s = c("", 11, 11, 11, 11, 11), 
    t = "T25S",
    r = "R38E",
    type = "SN",
    m = "CA21",
    stringsAsFactors = FALSE
  )))
  
  expect_equal(d, data.frame(
    id = 1:6,
    plssid = c(
      "CA210250S0380E0",
      "CA210250S0380E0SN110A",
      "CA210250S0380E0SN110ANENE",
      "CA210250S0380E0SN110ANWNE",
      "CA210250S0380E0SN110ASENE",
      "CA210250S0380E0SN110ASWNE"
    )
  ))
  
  # if qq is supplied without q, expect that the qq is dropped and warning about length
  expect_message(expect_warning({
    data.frame(id = 1, plssid = formatPLSS(data.frame(
      id = 1,
      qq = "SW",
      q = "",
      s = 11, 
      t = "T25S",
      r = "R38E",
      type = "SN",
      m = "CA21",
      stringsAsFactors = FALSE
    )
  ))}))
  
  d <<- d
})

test_that("PLSS2LL() works", {
  
  skip_if_offline()
  skip_on_cran()
  
  # center of township, center of section
  # center of NE,NW,SE,SW corner of northeast corner
  expect_equal(PLSS2LL(d), data.frame(
    id = c("1", "2", "3", "4", "5", "6"),
    plssid = c(
      "CA210250S0380E0", 
      "CA210250S0380E0SN110A",
      # "CA210250S0380E0SN110ANE", # doesn't work
      "CA210250S0380E0SN110ANENE",
      "CA210250S0380E0SN110ANWNE",
      "CA210250S0380E0SN110ASENE",
      "CA210250S0380E0SN110ASWNE"),
    lat = c(35.7536314968672, 35.7763284345201, 35.7818162070088, 
            35.7818389778208, 35.7781355594337, 35.7781460719862),
    lon = c(-117.883828112581, -117.857196264745, -117.85049298053, 
            -117.854957052084, -117.850511838394, -117.854968737876)
  ))
})

# inspect
# foo <- sf::st_as_sf(PLSS2LL(data.frame(
#   id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
#   plssid = c(
#     "CA210250S0380E0", 
#     "CA210250S0380E0SN010A",
#     "CA210250S0380E0SN060A",
#     "CA210250S0380E0SN310A",
#     "CA210250S0380E0SN360A",
#     # "CA210250S0380E0SN110ANE", # doesn't work
#     "CA210250S0380E0SN110ANENE",
#     "CA210250S0380E0SN110ANWNE",
#     "CA210250S0380E0SN110ASENE",
#     "CA210250S0380E0SN110ASWNE"))
# ), coords=c("lon","lat"), crs=4326)
# plot(foo$geometry)
