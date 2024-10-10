source('utils.R')

processPronouns <- function(featurestable, lgnames, warnings, errors) {
  
  #Ppron-01a
  stateCol <- ifelse(featurestable$`Ppron-01` == 'NA' & featurestable$`Ppron-003` == 'NA', 'NA',
              ifelse((featurestable$`Ppron-01` == '?' & featurestable$`Ppron-003` != 'yes') | (featurestable$`Ppron-01` != 'yes' & featurestable$`Ppron-003` == '?'), '?', 
              ifelse(featurestable$`Ppron-01` == 'yes' | featurestable$`Ppron-003` == 'yes', 'yes', 
              ifelse(featurestable$`Ppron-01` == 'no' & featurestable$`Ppron-003` == 'no', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- collapseSourceColumns(list(featurestable$`Ppron-01.Source`, featurestable$`Ppron-003.Source`))
  coderCol <- lapply(paste0(featurestable$`Ppron-01.Coder`, ';', featurestable$`Ppron-003.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Ppron-01a', stateCol, sourceCol, coderCol)
  
  #Ppron-04a
  stateCol <- ifelse(featurestable$`Ppron-04` == 'NA', 'NA',
              ifelse(featurestable$`Ppron-04` == '?', '?', 
              ifelse(featurestable$`Ppron-04` == 'inclusive' | featurestable$`Ppron-04` == 'unified we', 'yes', 
              ifelse(featurestable$`Ppron-04` == 'no', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Ppron-04.Source`
  coderCol <- featurestable$`Ppron-04.Coder`
  featurestable <- addDerivedState(featurestable, 'Ppron-04a', stateCol, sourceCol, coderCol)
  
  #Ppron-06a
  stateCol <- ifelse(featurestable$`Ppron-06` == 'NA', 'NA',
              ifelse(featurestable$`Ppron-06` == '?', '?', 
              ifelse(featurestable$`Ppron-06` == '1excl or pl is 1sg plus pl' | featurestable$`Ppron-06` == '1excl or pl includes 1sg', 'yes', 
              ifelse(featurestable$`Ppron-06` == 'no relation', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Ppron-06.Source`
  coderCol <- featurestable$`Ppron-06.Coder`
  featurestable <- addDerivedState(featurestable, 'Ppron-06a', stateCol, sourceCol, coderCol)
  
  #Ppron-06b
  stateCol <- ifelse(featurestable$`Ppron-06` == 'NA' | featurestable$`Ppron-06` == 'no relation', 'NA',
              ifelse(featurestable$`Ppron-06` == '?', '?', 
              ifelse(featurestable$`Ppron-06` == '1excl or pl is 1sg plus pl', 'yes', 
              ifelse(featurestable$`Ppron-06` == '1excl or pl includes 1sg', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Ppron-06.Source`
  coderCol <- featurestable$`Ppron-06.Coder`
  featurestable <- addDerivedState(featurestable, 'Ppron-06b', stateCol, sourceCol, coderCol)
  
  #Ppron-07a
  stateCol <- ifelse(featurestable$`Ppron-07` == 'NA', 'NA',
              ifelse(featurestable$`Ppron-07` == '?', '?', 
              ifelse(featurestable$`Ppron-07` == '1incl is 1sg plus pl' | featurestable$`Ppron-07` == '1incl includes 1sg', 'yes', 
              ifelse(featurestable$`Ppron-07` == 'no relation', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Ppron-07.Source`
  coderCol <- featurestable$`Ppron-07.Coder`
  featurestable <- addDerivedState(featurestable, 'Ppron-07a', stateCol, sourceCol, coderCol)
  
  #Ppron-07b
  stateCol <- ifelse(featurestable$`Ppron-07` == 'NA' | featurestable$`Ppron-07` == 'no relation', 'NA',
              ifelse(featurestable$`Ppron-07` == '?', '?', 
              ifelse(featurestable$`Ppron-07` == '1incl is 1sg plus pl', 'yes', 
              ifelse(featurestable$`Ppron-07` == '1incl includes 1sg', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Ppron-07.Source`
  coderCol <- featurestable$`Ppron-07.Coder`
  featurestable <- addDerivedState(featurestable, 'Ppron-07b', stateCol, sourceCol, coderCol)
  
  #Ppron-08a
  stateCol <- ifelse(featurestable$`Ppron-08` == 'NA' | featurestable$`Ppron-02` == 'NA' | featurestable$`Ppron-02` == 'yes', 'NA',
              ifelse((featurestable$`Ppron-02` == 'no' & featurestable$`Ppron-08` == '?') | featurestable$`Ppron-02` == '?', '?', 
              ifelse(featurestable$`Ppron-02` == 'no' & featurestable$`Ppron-08` == 'yes', 'yes',
              ifelse(featurestable$`Ppron-02` == 'no' & featurestable$`Ppron-08` == 'no', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Ppron-08.Source`
  coderCol <- featurestable$`Ppron-08.Coder`
  featurestable <- addDerivedState(featurestable, 'Ppron-08a', stateCol, sourceCol, coderCol)
  
  #Ppron-09a
  stateCol <- ifelse(featurestable$`Ppron-09` == 'NA' | featurestable$`Ppron-08a` == 'yes' | featurestable$`Ppron-08a` == 'NA', 'NA',
              ifelse(featurestable$`Ppron-09` == '?' | featurestable$`Ppron-08a` == '?', '?', 
              ifelse(featurestable$`Ppron-09` == 'yes' & featurestable$`Ppron-08a` == 'no', 'yes',
              ifelse(featurestable$`Ppron-09` == 'no' & featurestable$`Ppron-08a` == 'no', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- featurestable$`Ppron-09.Source`
  coderCol <- featurestable$`Ppron-09.Coder`
  featurestable <- addDerivedState(featurestable, 'Ppron-09a', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, (`Ppron-01` == 'yes' & `Ppron-03` != 'NA') | (`Ppron-03` == 'NA' & (`Ppron-01` != 'yes' & `Ppron-01` != 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Ppron', 'Ppron-01 == yes <=> Ppron-03 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Ppron-01` == 'no' & `Ppron-05` != 'NA') | (`Ppron-05` == 'NA' & (`Ppron-01` != 'no' & `Ppron-01` != 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Ppron', 'Ppron-01 == no <=> Ppron-05 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, ((`Ppron-02` == 'yes' | `Ppron-02` == 'NA') & `Ppron-06` != 'NA') | (`Ppron-06` == 'NA' & `Ppron-02` != 'yes' & `Ppron-02` != 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Ppron', 'Ppron-02 == yes | Ppron-02 == NA <=> Ppron-06 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, ((`Ppron-05` == 'yes' | `Ppron-05` == 'NA') & `Ppron-07` != 'NA') | (`Ppron-07` == 'NA' & `Ppron-05` != 'yes' & `Ppron-05` != 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Ppron', 'Ppron-05 == yes | Ppron-05 == NA <=> Ppron-07 == NA', lgnames)
  
  #write warnings, errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}
