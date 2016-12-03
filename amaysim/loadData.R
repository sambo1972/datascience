require(digest)

numberMappingFileName <- 'data/number_map.csv'

loadDataSet <- function(dataFileName){
  # load raw Amaysim data skipping summary lines
  skipLines <- 4
  ds <- read.table(dataFileName, skip = skipLines, header=F, 
                   sep=',', fill=T, na.strings = c(""), stringsAsFactors = F)
  # assign column names
  names(ds) <- c("Type","Time","Number","Duration","QuantityMB","Cost")
  
  # build Date column from Type column
  ds$Date <- ds$Type
  ds[ ds$Date %in% c("Type", "Data", "Text", "Talk") ,"Date"] <- NA
  
  # fill in gaps between dates
  ds$Date <- fillDates(ds)
  
  # convert to posix
  ds$Timestamp <- paste(ds$Date, ds$Time, sep=" ")
  ds$Timestamp <- as.POSIXlt(strptime(ds$Timestamp, format="%d %b %Y %H:%M"))
  
  # add a date column back as a factor to aid daily calcs
  ds$Date <- as.factor(as.Date(ds$Timestamp))
  
  # only keep usage records and basic columns
  ds <- ds[ds$Type %in% c("Data","Text","Talk"), c("Date", "Timestamp", "Type", "Number", "Duration", "QuantityMB")]
  ds$Type <- as.factor(ds$Type)
  
  # anonymise PII
  numberHashes <- anonymize(ds$Number, seed=538)
  # save a mapping for reference
  mappingDF <- buildMappingDF(numberHashes)
  write.csv(mappingDF, numberMappingFileName, row.names = F)
  # replace original numbers
  ds$Number <- unname(vapply(ds$Number, function(x) unname(numberHashes[x]), FUN.VALUE=""))
  
  # remove MB from quantity column and convert to numeric
  ds$QuantityMB <- as.numeric(sapply(ds$QuantityMB, function(x) gsub("MB", "", x)))
  
  # convert duration from HH:MM:SS to total seconds
  ds$Duration <- sapply(ds$Duration, convertToSeconds)
  names(ds)[grep("Duration", colnames(ds))] <- 'DurationSecs'
  return(ds)
}

convertToSeconds <- function(hhmmss){
  parts <- strsplit(hhmmss, ":")
  hours <- as.numeric(parts[[1]][1])
  mins <- as.numeric(parts[[1]][2])
  secs <- as.numeric(parts[[1]][3])
  totalSecs <- (hours * 3600) + (mins * 60) + secs
  return(totalSecs)
}

fillDates <- function(dataSet){
  lastDateVal <- NA
  
  dateCol <- dataSet[,7]
  
  cnt <- length(dateCol)
  for(i in 1:cnt){
    di <- dateCol[i]
    if(is.na(di)){
      dateCol[i] <- lastDateVal
    }
    else{
      lastDateVal <- dateCol[i]
    }
  }
  return(dateCol)
}

loadAllDataSets <- function(directory){
  mainDS <- NULL
  files <- list.files(directory)
  for(i in 1 : length(files)){
    dsi <- loadDataSet(paste0(directory, files[i]))
    if(i == 1){
      mainDS <- dsi
    }
    else{
      mainDS <- rbind(mainDS, dsi)
    }
  }
  return(mainDS[order(mainDS$Timestamp),])
}

# return a vector of hashes for unique values of x named using the x values
anonymize <- function(x, algo="sha256", seed=0){
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo, seed=seed), FUN.VALUE="", USE.NAMES=TRUE)
  return(unq_hashes)
}

# given a named vector of hashes separate out into a data frame
buildMappingDF <- function(hashes){
  src <- names(hashes)
  hash <- unname(hashes)
  mapdf <- data.frame(hash=hash, original=src)
  return (mapdf)
}