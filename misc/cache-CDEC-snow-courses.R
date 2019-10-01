library(rvest)

# get latest
x <- read_html('http://cdec.water.ca.gov/misc/SnowCourses.html')

# one big table
# note that there are rows used to define the river system, will have to clean
ht <- html_table(x)[[1]]

# adjust names
names(ht) <- c("course_number", "name", "id", "elev_feet", "latitude", "longitude", "april.1.Avg.inches", "agency")

# find watershed names
all.same <- apply(ht, 1, function(i) {
  length(unique(i)) == 1
})
idx <- which(all.same)

# name vector
ws.names <- ht[idx, 1]

# RLE analysis, determine replication required to copy watershed labels
ws.rle <- rle(as.numeric(! all.same))

# insert watershed names at known rows
# looks good
ht$watershed <- rep(rep(ws.names, each=2), times=ws.rle$lengths)

# remove rows with watershed labels only
ht <- ht[-idx, ]

# fix column types
ht$course_number <- as.integer(ht$course_number)
ht$elev_feet <- as.numeric(ht$elev_feet)
ht$latitude <- as.numeric(ht$latitude)
ht$longitude <- as.numeric(ht$longitude)
ht$april.1.Avg.inches <- as.numeric(ht$april.1.Avg.inches)

# save a copy
CDEC.snow.courses <- ht


# discontinued snow courses
x <- read_html('http://cdec.water.ca.gov/reportapp/javareports?name=SNOWTAB5')

# this will take some effort to clean-up
ht <- html_table(x, fill=TRUE, header = TRUE)

## finish this



save(CDEC.snow.courses, file='../data/CDEC.snow.courses.rda')
