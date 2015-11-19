#' This function takes in a file location and cleans the data for relevant entries
#'
#'  @param path_to_csv is the the file locatio
#'  @return clean_df the clean file location
#'
cleanData <- function(path_to_csv) {
  #read in data - set NULL as NA
  year13 <- read.csv(path_to_csv, na.strings=c("NULL", "PrivacySuppressed"))

  #remove private for-profit colleges (3)
  #1 is public, 2 is private non-profit
  year13 <- year13[year13$PREDDEG == 3,]
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
  factors <- c("HCM2", "main", "HIGHDEG", "st_fips", "region", "LOCALE", "CCBASIC", "CCUGPROF", "CCSIZSET", "HBCU", "PBI",
              "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI", "MENONLY", "WOMENONLY", "DISTANCEONLY", "CURROPER")
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

cleanDataIncome <- function(path_to_csv) {
  #read in data - set NULL as NA
  year13 <- read.csv(path_to_csv, na.strings=c("NULL", "PrivacySuppressed"))
  
  #remove private for-profit colleges (3)
  #1 is public, 2 is private non-profit
  year13 <- year13[year13$PREDDEG == 3,]
  year13 <- subset(year13, year13$CONTROL != 3)
  year13$CONTROL <- as.factor(year13$CONTROL)
  
  #remove columns with more than 500 na values, but keet logitude and latitude
  nafind <- function(x){sum(is.na(x))}
  keep <- c("LONGITUDE", "LATITUDE", "DEBT_MDN", "md_earn_wne_p6")
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
  
  #remove ID columns only the first 3
  clean_df <- clean_df[,c(-1, -2, -3)]
  
  # remove schools that have NA DEBT_MDN
  na_debt <- which(is.na(clean_df$DEBT_MDN))
  clean_df <- clean_df[-na_debt, ]
  
  # make sure debt and income are numeric
  clean_df$DEBT_MDN <- as.numeric(clean_df$DEBT_MDN)
  clean_df$md_earn_wne_p6 <- as.numeric(clean_df$md_earn_wne_p6)
  
  # Save the income column
  y_income <- clean_df$md_earn_wne_p6
  clean_df$md_earn_wne_p6 <- NULL
  
  #remove variables with CIP in variable name
  columns <- names(clean_df)[grepl("CIP", names(clean_df))]
  clean_df <- clean_df[, !(colnames(clean_df) %in% columns)]
  
  #### Fix Factors
  # these variables need to be converted to factors
  factors <- c("HCM2", "main", "HIGHDEG", "st_fips", "region", "LOCALE", "CCBASIC", "CCUGPROF", "CCSIZSET", "HBCU", "PBI",
               "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI", "MENONLY", "WOMENONLY", "DISTANCEONLY", "CURROPER")
  other_factors <- as.data.frame(apply(clean_df[, factors], 2, as.factor))
  
  # collect variables that aren't factors
  not_factors <- clean_df[, !(colnames(clean_df) %in% factors)]
  
  # combine all back together
  clean_df <- data.frame(other_factors, not_factors)
  
  # remove variables that are only one value
  # when using str() it will say these variables have 2 levels, but 1 is NA
  constants <- c("TRIBAL", "poolyrs200", "poolyrs", "MENONLY", "DISTANCEONLY", "CURROPER", "HCM2", "CIP29CERT2")
  clean_df <- clean_df[,!(colnames(clean_df) %in% constants)]
  
  clean_df <- cbind(clean_df, y_income)
  
  return(clean_df)
}
