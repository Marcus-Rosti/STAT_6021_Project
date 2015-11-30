#' This function takes in a file location and cleans the data for relevant entries
#'
#'  @param path_to_csv is the the file locatio
#'  @return clean_df the clean file location
#'
cleanData <- function(path_to_csv) {
  #read in data - set NULL as NA
  year13 <- read.csv(path_to_csv, na.strings=c("NULL", "PrivacySuppressed"))

  # keep only 4 year colleges - see below
  # Trying to just keep CCBASIC 15-23, which is:
     # 15,Research Universities (very high research activity)
     # 16,Research Universities (high research activity)
     # 17,Doctoral/Research Universities
     # 18,Master's Colleges and Universities (larger programs)
     # 19,Master's Colleges and Universities (medium programs)
     # 20,Master's Colleges and Universities (smaller programs)
     # 21,Baccalaureate Colleges--Arts & Sciences
     # 22,Baccalaureate Colleges--Diverse Fields
     # 23,Baccalaureate/Associate's Colleges
  year13 <- year13[year13$CCBASIC %in% c(15, 16, 17, 18, 19, 20, 21, 22, 23),]

  year13 <- subset(year13, year13$CONTROL != 3)
  year13$CONTROL <- as.factor(year13$CONTROL)

  #remove columns with more than 500 na values, but keet logitude and latitude
  nafind <- function(x){sum(is.na(x))}
  keep <- c("LONGITUDE", "LATITUDE", "DEBT_MDN")
  nacount <- apply(year13, 2, "nafind")
  varWna <- which(nacount > 500)

  for (i in keep) {
    if (any(colnames(year13)[varWna] == i)) {
      varWna <- varWna[-which(colnames(year13)[varWna] == i)]
    }
  }

  clean_df <- year13[,-varWna]

  # remove variables that have all the same value
  clean_df <- clean_df[sapply(clean_df, function(x) length(unique(x)) > 1)]

  # remove schools that have NA DEBT_MDN
  na_debt <- which(is.na(clean_df$DEBT_MDN))
  clean_df <- clean_df[-na_debt, ]


  #remove ID columns only the first 3
  clean_df <- clean_df[,c(-1, -2, -3)]

  # make sure debt is numeric
  clean_df$DEBT_MDN <- as.numeric(clean_df$DEBT_MDN)

  # Save the debt column
  y_debt <- clean_df$DEBT_MDN
  clean_df$DEBT_MDN <- NULL

  #remove variables with DEBT in variable name
  columns <- names(clean_df)[grepl("DEBT", names(clean_df))]
  clean_df <- clean_df[, !(colnames(clean_df) %in% columns)]

  #remove variables with CIP in variable name
  columns <- names(clean_df)[grepl("CIP", names(clean_df))]
  clean_df <- clean_df[, !(colnames(clean_df) %in% columns)]

  #### Fix Factors
  # these variables need to be converted to factors
  ### Removed TRIBAL when it now is removed above
  factors <- c("HCM2", "main", "HIGHDEG", "st_fips", "region", "LOCALE", "CCBASIC", "CCUGPROF", "CCSIZSET", "HBCU", "PBI",
              "ANNHI", "AANAPII", "HSI", "NANTI", "MENONLY", "WOMENONLY", "DISTANCEONLY", "CURROPER")
  other_factors <- as.data.frame(apply(clean_df[, factors], 2, as.factor))

  # collect variables that aren't factors
  not_factors <- clean_df[, !(colnames(clean_df) %in% factors)]

  # combine all back together
  clean_df <- data.frame(other_factors, not_factors)

  # remove variables that are only one value
  # when using str() it will say these variables have 2 levels, but 1 is NA
  constants <- c("TRIBAL", "poolyrs200", "poolyrs", "MENONLY", "DISTANCEONLY", "CURROPER", "HCM2", "CIP29CERT2")
  clean_df <- clean_df[,!(colnames(clean_df) %in% constants)]

  clean_df <- cbind(clean_df, y_debt)

  return(clean_df)
}

cleanData2 <- function(path_to_csv) {
  #read in data - set NULL as NA
  year13 <- read.csv(path_to_csv, na.strings=c("NULL", "PrivacySuppressed"))
  
  #2 year colleges only
  year13 <- year13[year13$CCBASIC %in% c(1:9,11,12,13),]
  
  year13 <- subset(year13, year13$CONTROL != 3)
  year13$CONTROL <- as.factor(year13$CONTROL)
  
  #remove columns with more than 500 na values, but keet logitude and latitude
  nafind <- function(x){sum(is.na(x))}
  keep <- c("LONGITUDE", "LATITUDE", "DEBT_MDN")
  nacount <- apply(year13, 2, "nafind")
  varWna <- which(nacount > 500)
  
  for (i in keep) {
    if (any(colnames(year13)[varWna] == i)) {
      varWna <- varWna[-which(colnames(year13)[varWna] == i)]
    }
  }
  
  clean_df <- year13[,-varWna]
  
  # remove variables that have all the same value
  clean_df <- clean_df[sapply(clean_df, function(x) length(unique(x)) > 1)]
  
  # remove schools that have NA DEBT_MDN
  na_debt <- which(is.na(clean_df$DEBT_MDN))
  clean_df <- clean_df[-na_debt, ]
  
  
  #remove ID columns only the first 3
  clean_df <- clean_df[,c(-1, -2, -3)]
  
  # make sure debt is numeric
  clean_df$DEBT_MDN <- as.numeric(clean_df$DEBT_MDN)
  
  # Save the debt column
  y_debt <- clean_df$DEBT_MDN
  clean_df$DEBT_MDN <- NULL
  
  #remove variables with DEBT in variable name
  columns <- names(clean_df)[grepl("DEBT", names(clean_df))]
  clean_df <- clean_df[, !(colnames(clean_df) %in% columns)]
  
  #remove variables with CIP in variable name
  columns <- names(clean_df)[grepl("CIP", names(clean_df))]
  clean_df <- clean_df[, !(colnames(clean_df) %in% columns)]
  
  #### Fix Factors
  # these variables need to be converted to factors
  ### Removed TRIBAL when it now is removed above
  factors <- c("HCM2", "main", "HIGHDEG", "st_fips", "region", "LOCALE", "CCBASIC", "CCUGPROF", "CCSIZSET", "HBCU", "PBI",
               "ANNHI", "AANAPII", "HSI", "NANTI", "MENONLY", "WOMENONLY", "DISTANCEONLY", "CURROPER")
  other_factors <- as.data.frame(apply(clean_df[, factors], 2, as.factor))
  
  # collect variables that aren't factors
  not_factors <- clean_df[, !(colnames(clean_df) %in% factors)]
  
  # combine all back together
  clean_df <- data.frame(other_factors, not_factors)
  
  # remove variables that are only one value
  # when using str() it will say these variables have 2 levels, but 1 is NA
  constants <- c("TRIBAL", "poolyrs200", "poolyrs", "MENONLY", "DISTANCEONLY", "CURROPER", "HCM2", "CIP29CERT2")
  clean_df <- clean_df[,!(colnames(clean_df) %in% constants)]
  
  clean_df <- cbind(clean_df, y_debt)
  
  return(clean_df)
}