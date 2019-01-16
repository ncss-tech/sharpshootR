# needs testing!!!

site_photos_kml <- function(data, filename='photos.kml', make.image.grid=FALSE, file.source = c("local", "relative")) {

  start_kml <- function(filename) {
  kml_head <- '<?xml version="1.0" encoding="UTF-8"?>
  <kml xmlns="http://earth.google.com/kml/2.2">
  <Document>
  <name>Site_photos.kml</name>
  <Style id="redicon">
  <LabelStyle>
  <scale>0.8</scale>
  </LabelStyle>
  <IconStyle>
  <color>#ff00ffff</color>
  <scale>0.8</scale>
  <Icon>
  <href>http://plotkml.r-forge.r-project.org/circle.png</href>
  </Icon>
  <hotSpot x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>
  </IconStyle>
  </Style>
  <Style id="blueicon">
  <LabelStyle>
  <scale>0.8</scale>
  </LabelStyle>
  <IconStyle>
  <scale>0.8</scale>
  <color>#ffba832b</color>
  <Icon>
  <href>http://plotkml.r-forge.r-project.org/circle.png</href>
  </Icon>
  </IconStyle>
  </Style>'
  write(kml_head, file=filename, append=FALSE)
}

# apply to rows assoc. with single user site ID
make_placemark <- function(data) {
  # extract site level data
  s.info <- data[1, c('site_id', 'pedon_id', 'x_std', 'y_std')]

  # placemark color assignment
  	if (!is.na(data$imagepath[1])) {
  	  icon <- '#blueicon' 
  	  } else { 
  	    icon <- '#redicon'
  	  }
  
  # assemble placemark
  p.1 <- paste('<Placemark>
               <name>', s.info$site_id, '</name> 
               <styleUrl>', icon, '</styleUrl>
               <Point>
               <coordinates>' , s.info$x_std, ',' , s.info$y_std, '</coordinates>
               </Point>
               <description><![CDATA[<tr><td width="100%"><p align="Left"></p></td></tr><a href="https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Pedon+Description+html+(userpedid)&pedon_id=' , s.info$pedon_id, '">Pedon Description Report</a>]]>
               <tr>
               <Snippet maxLines="0" id ="khSnippet701_copy11"></Snippet>
               </tr>
               <table>
               <tr>
               <Snippet maxLines="0" id ="khSnippet701_copy11"></Snippet>
               </tr>
               <tr>', sep='')
  
 # add closing tags
  f.1 <- '  </tr>
  </table>
  </description>
  </Placemark>' 
  
    if(make.image.grid == TRUE) {
  p.2 <- make_image_grid(data$imagepath, data$imagename)
    return(paste(p.1, p.2, f.1, collapse='\n'))
  } else {
    return(paste(p.1, f.1, collapse='\n')) 
  }
  
}

make_image_grid <- function(image.paths, image.names) {
#make_image_grid <- function(image.paths, image.names, w=3) {  
  
  #i.grp <- as.list(1:nrow(data))
  #i.grp <- as.list(1:nrow(data) %% w) #TODO: use '%% w' to index for 3x3 image grid
  
  #if(i.grp[1] == 1)

  if(file.source == 'relative') {	
  # source to relative path     
  kml_photos <- paste('<td><![CDATA[<img src="files/' , image.names, '" width="230" height="180"]]></td>', sep='')
  } else { 
  # source to specific local system path       
  kml_photos <- paste('<td><![CDATA[<img src="files:///' , image.paths, '" width="230" height="180"]]></td>', sep='')
  }
  kml_row <- '<tr>
  <tr>'
  #else kml_photos <- paste('<td><![CDATA[<img src="files/' , image.paths, '" width="230" height="180"]]></td>', sep='')
  kml_spacer <- '</tr>
  <tr>
  <Snippet maxLines="0" id ="khSnippet701_copy11"></Snippet>
  </tr>
  <tr>'
  if(file.source == 'relative') {
  # these links will appear in the placemark, but won't work unless the source is from a location on the web - URL, could add as another option
  kml_link <- paste('<td><p><font size="2"><![CDATA[<a href="', image.names, '" ALT ="No photo - check path">' , image.names, '</a]]></font></p></td>', sep='')
  } else {
  kml_link <- paste('<td><p><font size="2"><![CDATA[<a href="', image.paths, '" ALT ="No photo - check path">' , image.names, '</a]]></font></p></td>', sep='')
  }
  res <- paste(c(paste(kml_photos, collapse = "\n"), kml_spacer, paste(kml_link, collapse = "\n")), collapse = "\n")
  return(res)
}

stop_kml <- function(filename) {
  kml_foot <- '</Document>
  </kml>'  
  write(kml_foot, file=filename, append=TRUE)
}

start_kml(filename)
kml_placemarks <- plyr::dlply(data, 'site_id', .fun = make_placemark)
kml_placemarks <- paste(kml_placemarks, collapse="\n")
cat(kml_placemarks, file=filename, append = TRUE)
stop_kml(filename)

}


