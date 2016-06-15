Describe <- function(fieldID='f2091'){
  
  urlLatLon1<-"http://dev.maxmind.com/static/csv/codes/country_latlon.csv"
  latlonDF1 <- read.csv(urlLatLon1, stringsAsFactors = FALSE)
  names(latlonDF1)[1] <- "iso"
  
  
  if(file.exists("/factbook.xml")==FALSE){
    factbookURL1 <- "http://jmatchparser.sourceforge.net/factbook/data/factbook.xml.gz"
    download.file(factbookURL1, paste(getwd(), "/factbook.xml.gz", sep=""), cacheOK=TRUE)
    gunzip("factbook.xml.gz", overwrite=TRUE)
  }
  
  
  factbookDoc1 <- xmlParse("factbook.xml")
  factbookRoot1 <- xmlRoot(factbookDoc1)
  
  DescriptionPaste <- paste("//field[@id='", fieldID, "']/description", sep = "")
  Dnodes<- sapply(getNodeSet(factbookDoc, DescriptionPaste), xmlValue)
  Dnodes
}