source('utils.R')

processLaterals <- function(featurestable, lgnames, warnings, errors) {
  
  #Lat-01a
  stateCol <- ifelse(featurestable$`Lat-01` == '?', '?', 
              ifelse(featurestable$`Lat-01` == "no", "no", "yes"))
  sourceCol <- featurestable$`Lat-01.Source`
  coderCol  <- featurestable$`Lat-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Lat-01a', stateCol, sourceCol, coderCol)
  
  #Lat-01b
  stateCol <- ifelse(featurestable$`Lat-01` == '?', '?', 
              ifelse(featurestable$`Lat-01` == "laterals or glides", "yes", "no"))
  sourceCol <- featurestable$`Lat-01.Source`
  coderCol  <- featurestable$`Lat-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Lat-01b', stateCol, sourceCol, coderCol)
  
  #Lat-01c
  stateCol <- ifelse(featurestable$`Lat-01` == '?', '?', 
              ifelse(featurestable$`Lat-01` == "laterals or glides", "laterals or glides",
              ifelse(featurestable$`Lat-01` == "rhotic", "rhotic",
              ifelse(featurestable$`Lat-01` == "n or d", "n or d",
              ifelse(featurestable$`Lat-01` == "n or d and rhotic", "?",
              ifelse(featurestable$`Lat-01` == "no" | featurestable$`Lat-01` == "NA", "NA", "ERROR: Impossible state"))))))
  sourceCol <- featurestable$`Lat-01.Source`
  coderCol  <- featurestable$`Lat-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Lat-01c', stateCol, sourceCol, coderCol)
  
  #Lat-02a
  stateCol <- ifelse(featurestable$`Lat-02` == '?' | featurestable$`Lat-03` == '?', '?', 
              ifelse(featurestable$`Lat-02` == "yes" | featurestable$`Lat-03` == "yes", "yes", "no"))
  sourceCol <- collapseSourceColumns(list(featurestable$`Lat-02.Source`, featurestable$`Lat-03.Source`))
  coderCol  <- sapply(paste0(featurestable$`Lat-02.Coder`, ';', featurestable$`Lat-03.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Lat-02a', stateCol, sourceCol, coderCol)
  
  #Lat-02b
  stateCol <- ifelse(featurestable$`Lat-02a` == '?', '?', 
              ifelse(featurestable$`Lat-02a` == "no", "NA", 
              ifelse(featurestable$`Lat-02` == "yes", "voiceless-approximant", 
              ifelse(featurestable$`Lat-03` == "yes", "fricative", "ERROR: Impossible state!"))))
  sourceCol <- collapseSourceColumns(list(featurestable$`Lat-02.Source`, featurestable$`Lat-03.Source`))
  coderCol  <- sapply(paste0(featurestable$`Lat-02.Coder`, ';', featurestable$`Lat-03.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Lat-02b', stateCol, sourceCol, coderCol)
  
  #Lat-04a
  stateCol <- ifelse(featurestable$`Lat-03` == 'no', 'NA',
              ifelse((featurestable$`Lat-04` == '?' & featurestable$`Lat-05` != 'yes') | (featurestable$`Lat-04` != 'yes' & featurestable$`Lat-05` == '?'), '?', 
              ifelse(featurestable$`Lat-04` == "yes" | featurestable$`Lat-05` == "yes", "yes", "no")))
  sourceCol <- collapseSourceColumns(list(featurestable$`Lat-04.Source`, featurestable$`Lat-05.Source`))
  coderCol  <- sapply(paste0(featurestable$`Lat-04.Coder`, ';', featurestable$`Lat-05.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Lat-04a', stateCol, sourceCol, coderCol)
  
  #Lat-04b
  stateCol <- ifelse(featurestable$`Lat-04a` == "no" | featurestable$`Lat-04a` == "NA", 'NA', 
              ifelse(featurestable$`Lat-04a` =='?', '?', 
              ifelse(featurestable$`Lat-04` == "yes", "yes", "no")))
  sourceCol <- featurestable$`Lat-04.Source`
  coderCol  <- featurestable$`Lat-04.Coder`
  featurestable <- addDerivedState(featurestable, 'Lat-04b', stateCol, sourceCol, coderCol)
  
  #Lat-05a
  stateCol <- ifelse(featurestable$`Lat-04a` == "no" | featurestable$`Lat-04a` == "NA", "NA", 
              ifelse(featurestable$`Lat-04a` == '?', '?', 
              ifelse(featurestable$`Lat-05` == "yes", "yes", "no")))
  sourceCol <- collapseSourceColumns(list(featurestable$`Lat-04.Source`, featurestable$`Lat-05.Source`))
  coderCol  <- lapply(paste0(featurestable$`Lat-04.Coder`, ';', featurestable$`Lat-05.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Lat-05a', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  errorsList <- dplyr::filter(featurestable, (`Lat-02` == 'yes' & `Lat-01a` != 'yes'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Lat', 'Lat-02 = yes => Lat-01a = yes', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Lat-05` == 'yes' & `Glot-02` != 'yes'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Lat', 'Lat-05 = yes => Glot-02 = yes', lgnames)
  
  #write warnings, errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}