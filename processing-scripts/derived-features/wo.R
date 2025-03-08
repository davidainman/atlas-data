source('utils.R')

processWordOrder <- function(featurestable, lgnames, warnings, errors) {
  
  #WO-01a
  stateCol <- ifelse(featurestable$`WO-01` == '?', '?', 
              ifelse(featurestable$`WO-01` == 'no dominant order', 'no', 'yes'))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01a', stateCol, sourceCol, coderCol)
  
  #WO-01b
  stateCol <- ifelse(featurestable$`WO-01` == '?', '?', 
              ifelse(featurestable$`WO-01` == 'no dominant order', 'NA',
              ifelse(featurestable$`WO-01` == 'SVO' | featurestable$`WO-01` == 'SOV' | featurestable$`WO-01` == 'OSV' | featurestable$`WO-01` == 'V-last', 'S>V',
              ifelse(featurestable$`WO-01` == 'VSO' | featurestable$`WO-01` == 'VOS' | featurestable$`WO-01` == 'OVS' | featurestable$`WO-01` == 'V-first' | featurestable$`WO-01` == 'S-last', 'V>S', 'ERROR: IMPOSSIBLE STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01b', stateCol, sourceCol, coderCol)
  
  #WO-01c
  stateCol <- ifelse(featurestable$`WO-01` == '?' | featurestable$`WO-01` == 'V-first' | featurestable$`WO-01` == 'V-last', '?', 
              ifelse(featurestable$`WO-01` == 'no dominant order', 'NA',
              ifelse(featurestable$`WO-01` == 'SVO' | featurestable$`WO-01` == 'SOV' | featurestable$`WO-01` == 'VSO', 'S>O', 
              ifelse(featurestable$`WO-01` == 'OSV' | featurestable$`WO-01` == 'OVS' | featurestable$`WO-01` == 'VOS'  | featurestable$`WO-01` == 'S-last', 'O>S', 'ERROR: IMPOSSIBLE STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01c', stateCol, sourceCol, coderCol)
  
  #WO-01d
  stateCol <- ifelse(featurestable$`WO-01` == '?' | featurestable$`WO-01` == 'S-last', '?', 
              ifelse(featurestable$`WO-01` == 'no dominant order', 'NA',
              ifelse(featurestable$`WO-01` == 'SVO' | featurestable$`WO-01` == 'VSO' | featurestable$`WO-01` == 'VOS' | featurestable$`WO-01` == 'V-first', 'V>O', 
              ifelse(featurestable$`WO-01` == 'SOV' | featurestable$`WO-01` == 'OVS' | featurestable$`WO-01` == 'OSV' | featurestable$`WO-01` == 'V-last', 'O>V', 'ERROR: IMPOSSIBLE STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01d', stateCol, sourceCol, coderCol)
  
  #WO-01e
  stateCol <- ifelse(featurestable$`WO-01` == '?' | featurestable$`WO-01` == 'S-last' | featurestable$`WO-01` == 'V-last', '?', 
              ifelse(featurestable$`WO-01` == 'no dominant order', 'no dominant order', 
              ifelse(featurestable$`WO-01` == 'SVO' | featurestable$`WO-01` == 'SOV', 'S-first',
              ifelse(featurestable$`WO-01` == 'VSO' | featurestable$`WO-01` == 'VOS' | featurestable$`WO-01` == 'V-first', 'V-first',
              ifelse(featurestable$`WO-01` == 'OSV' | featurestable$`WO-01` == 'OVS', 'O-first', 'ERROR: INVALID STATE')))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01e', stateCol, sourceCol, coderCol)
  
  #WO-01f
  stateCol <- ifelse(featurestable$`WO-01e` == '?', '?', 
              ifelse(featurestable$`WO-01e` != 'S-first', 'NA', 
              ifelse(featurestable$`WO-01` == 'SVO', 'SVO',
              ifelse(featurestable$`WO-01` == 'SOV' | featurestable$`WO-01` == 'V-last', 'SOV', 'ERROR: INVALID STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01f', stateCol, sourceCol, coderCol)
  
  #WO-01g
  stateCol <- ifelse(featurestable$`WO-01e` == '?' | featurestable$`WO-01` == 'V-first', '?', 
              ifelse(featurestable$`WO-01e` != 'V-first', 'NA', 
              ifelse(featurestable$`WO-01` == 'VSO', 'VSO',
              ifelse(featurestable$`WO-01` == 'VOS', 'VOS', 'ERROR: INVALID STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01g', stateCol, sourceCol, coderCol)
  
  #WO-01h
  stateCol <- ifelse(featurestable$`WO-01e` == '?', '?', 
              ifelse(featurestable$`WO-01e` != 'O-first', 'NA', 
              ifelse(featurestable$`WO-01` == 'OVS', 'OVS',
              ifelse(featurestable$`WO-01` == 'OSV', 'OSV', 'ERROR: INVALID STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01h', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, ((`NounPoss-08` == 'CLAUSE' | `NounPoss-08` == 'NULL') & `WO-03` != 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'WO', 'NounPoss-08 == CLAUSE | NounPoss-08 == NULL => WO-03 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, ((`NounPoss-08` != 'CLAUSE' & `NounPoss-08` != 'NULL') & `WO-03` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'WO', 'WO-03 == NA => NounPoss-08 == CLAUSE | NounPoss-08 == NULL', lgnames)
  
  #write warnings, errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}
