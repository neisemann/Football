Describe <- function(fieldID='f2091'){
  
  urlLatLon<-"http://dev.maxmind.com/static/csv/codes/country_latlon.csv"
  latlonDF <- read.csv(urlLatLon, stringsAsFactors = FALSE)
  names(latlonDF)[1] <- "iso"
  
  
  if(file.exists("factbook.xml")==FALSE){
    factbookURL <- "http://jmatchparser.sourceforge.net/factbook/data/factbook.xml.gz"
    download.file(factbookURL, paste(getwd(), "/factbook.xml.gz", sep=""), cacheOK=TRUE)
    gunzip("factbook.xml.gz", overwrite=TRUE)
  }
  
  
  factbookDoc <- xmlParse("factbook.xml")
  factbookRoot <- xmlRoot(factbookDoc)
  
  DescriptionPaste <- paste("//field[@id='", fieldID, "']/description", sep = "")
  Dnodes<- sapply(getNodeSet(factbookDoc, DescriptionPaste), xmlValue)
  Dnodes
}