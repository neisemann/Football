map_it <- function(fieldID='f2091', User_Define_Color = 'YlOrRd'){
  
  # Data Acquisition
  
  ## Geographic Boundaries
  
  ##Define the file location
  
  urlLatLon <- "http://dev.maxmind.com/static/csv/codes/country_latlon.csv"
  latLonDF <- read.csv(urlLatLon, stringsAsFactors = FALSE)
  names(latLonDF)[1] <- "iso"

  ## Country Level Demographics
  
  ##Define the file location
  ##Download the data and Unzip the file
  
  if(file.exists("data/factbook.xml")==FALSE){
    factbookURL <- "http://jmatchparser.sourceforge.net/factbook/data/factbook.xml.gz"
    download.file(factbookURL, paste(getwd(), "/factbook.xml.gz", sep = ""), cacheOK = TRUE)
    gunzip("factbook.xml.gz", overwrite = TRUE)
  }
  
  # Extracting XML Info
  

  ##Use xmlParse to create an XML tree object & define object as root of the XML tree.
  factbookDoc <- xmlParse("data/factbook.xml")
  def_field <- paste("//field[@id='", fieldID, "']/rank", sep="")
  rankNodes <- getNodeSet(factbookDoc, def_field)
  Legend_Title <- paste("//field[@id='", fieldID, "']", sep="")
  rankNodes1 <- getNodeSet(factbookDoc, Legend_Title)
  LegendTitle <- sapply(rankNodes1, xmlGetAttr, "name")
  
  ## retrieve the value of the number and country attributes
  infNum <- as.numeric(sapply(rankNodes, xmlGetAttr, "number"))
  infCtry <- sapply(rankNodes, xmlGetAttr, "country")

  ##Create a Data Frame
  infMortDF <- data.frame(infMort = infNum, ctry=infCtry, stringsAsFactors = FALSE)

  ##Extract Population Size
  popNodes <- getNodeSet(factbookDoc, "//field[@id='f2119']/rank")
  popNum <- as.numeric(sapply(popNodes, xmlGetAttr, "number"))
  popCtry <- sapply(popNodes, xmlGetAttr, "country")
  popDF <- data.frame(pop=popNum ,ctry=popCtry, stringsAsFactors = FALSE)


  ##Merge the two data frames together
  IMPop <- merge(infMortDF, popDF, by="ctry", all=TRUE)

  ## Merging (correct) geographic location
  ##First stored entire table into isotable object
  isotable <- getNodeSet(factbookDoc, "//appendix[@letter='d']/table/row/cell")

  ##Extract the list, and remove the nesting:
  country <- sapply(isotable, xmlGetAttr, "country")
  country <- unlist(country)

  ##Extract all "content" tags into a single object. 
  # (because the country name and ISO code are located in the first, 
  # and third instance of the content tag)

  content <- sapply(isotable, xmlGetAttr, "content")
  
  ##Create empty dataframe as placeholder, w/ # of rows = to # of cntrys & 3cols
  codeMapDF <- data.frame(matrix(NA, nrow=length(country), ncol=3))
  colnames(codeMapDF) <- c("ctry", "name", "iso")

  ##First row is CIA country code, can be directly entered using "country" vector
  codeMapDF[,1] <- country

  ##Then, looping over each row in the data frame codeMapDF, extract data from
  ##the first and third elements (out of the 8) from the "content" tag
  for(i in 1:NROW(codeMapDF)) {
    r <- 8*i-7
    codeMapDF[i,2] <- content[r]
    j <- r+2
    codeMapDF[i,3] <- content[j]
  }

  ## Merging geographic and demographic data together
  allCtryData <- IMPop %>%
    left_join(codeMapDF) %>%
    left_join(latLonDF) %>%
    arrange(iso)

  #Preparing the data for Plotting
  ##Choose a set of colors
  cols <- brewer.pal(9, User_Define_Color)[c(1,2,4,6,7)]

  ##Categorize the infant mortality
  newbreaks <- quantile(allCtryData$infMort, seq(0, 1, .2), na.rm=TRUE)
  allCtryData$infMortDiscrete <- cut(allCtryData$infMort, breaks=newbreaks)

  ##Assign the 5 colors to each of the 5 mortality categories
  allCtryData$colr <- cols[allCtryData$infMortDiscrete]
  
  ##Creating the Map
  world <- map(database="world", col="light grey", fill=TRUE)
  with(allCtryData, symbols(longitude, latitude, circles=sqrt(pop)/4000, inches=FALSE, add=TRUE, fg=colr, bg=colr))
  with(allCtryData, title(LegendTitle))
  legend(x= -150, y=0, 
       legend = levels(allCtryData$infMortDiscrete), fill=cols, cex =.8)

}