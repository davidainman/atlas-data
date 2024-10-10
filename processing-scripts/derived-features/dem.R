source('utils.R')

processDemonstratives <- function(featurestable, lgnames, warnings, errors) {

  #Dem.01a
  stateCol <- ifelse(featurestable$`Dem-01` == '?', '?', 
              ifelse(featurestable$`Dem-01` >= 2, 'yes', 
              ifelse(featurestable$`Dem-01` == 0, 'no', 'ERROR: Imposisble state!')))
  sourceCol <- featurestable$`Dem-01.Source`
  coderCol <- featurestable$`Dem-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Dem-01a', stateCol, sourceCol, coderCol)
  
  #Dem.01b
  stateCol <- ifelse(featurestable$`Dem-01` == '?', '?', 
              ifelse(featurestable$`Dem-01` == 2, '2', 
              ifelse(featurestable$`Dem-01` >= 3, 'more than 2', 
              ifelse(featurestable$`Dem-01` == 0, 'NA', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Dem-01.Source`
  coderCol <- featurestable$`Dem-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Dem-01b', stateCol, sourceCol, coderCol)
  
  #Dem.01c
  stateCol <- ifelse(featurestable$`Dem-01` == '?', '?', 
              ifelse(featurestable$`Dem-01` == 2 | featurestable$`Dem-01` == 3, 'up to 3', 
              ifelse(featurestable$`Dem-01` >= 4, 'more than 3', 
              ifelse(featurestable$`Dem-01` == 0, 'NA', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Dem-01.Source`
  coderCol <- featurestable$`Dem-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Dem-01c', stateCol, sourceCol, coderCol)
  
  #Dem.01d
  stateCol <- ifelse(featurestable$`Dem-01` == '?', '?', 
              ifelse(featurestable$`Dem-01` == 2 | featurestable$`Dem-01` == 3 | featurestable$`Dem-01` == 4, 'up to 4', 
              ifelse(featurestable$`Dem-01` >= 5, 'more than 4', 
              ifelse(featurestable$`Dem-01` == 0, 'NA', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Dem-01.Source`
  coderCol <- featurestable$`Dem-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Dem-01d', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, ((`Dem-001` == 'pronominal' & `Dem-002` != 'NA') | (`Dem-001` != 'pronominal' & `Dem-002` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Dem', 'Dem-001 == "pronominal" <-> Dem-002 == "NA"', lgnames)
  
  errorsList <- dplyr::filter(featurestable, ((`Dem-002` == 'no' & `Dem-002` == 'NA' & `Dem-003` != 'NA') | (`Dem-002` == 'yes' & `Dem-003` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Dem', 'Dem-002 == "no" | "NA" <-> Dem-003 != NA', lgnames)
  
  #write warnings, errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}
