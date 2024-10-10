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
              ifelse(featurestable$`WO-01` == 'svo' | featurestable$`WO-01` == 'sov' | featurestable$`WO-01` == 'osv' | featurestable$`WO-01` == 'v-last', 'S>V',
              ifelse(featurestable$`WO-01` == 'vso' | featurestable$`WO-01` == 'vos' | featurestable$`WO-01` == 'ovs' | featurestable$`WO-01` == 'v-first' | featurestable$`WO-01` == 's-last', 'V>S', 'ERROR: IMPOSSIBLE STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01b', stateCol, sourceCol, coderCol)
  
  #WO-01c
  stateCol <- ifelse(featurestable$`WO-01` == '?' | featurestable$`WO-01` == 'v-first' | featurestable$`WO-01` == 'v-last', '?', 
              ifelse(featurestable$`WO-01` == 'no dominant order', 'NA',
              ifelse(featurestable$`WO-01` == 'svo' | featurestable$`WO-01` == 'sov' | featurestable$`WO-01` == 'vso', 'S>O', 
              ifelse(featurestable$`WO-01` == 'osv' | featurestable$`WO-01` == 'ovs' | featurestable$`WO-01` == 'vos'  | featurestable$`WO-01` == 's-last', 'O>S', 'ERROR: IMPOSSIBLE STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01c', stateCol, sourceCol, coderCol)
  
  #WO-01d
  stateCol <- ifelse(featurestable$`WO-01` == '?' | featurestable$`WO-01` == 's-last', '?', 
              ifelse(featurestable$`WO-01` == 'no dominant order', 'NA',
              ifelse(featurestable$`WO-01` == 'svo' | featurestable$`WO-01` == 'vso' | featurestable$`WO-01` == 'vos' | featurestable$`WO-01` == 'v-first', 'V>O', 
              ifelse(featurestable$`WO-01` == 'sov' | featurestable$`WO-01` == 'ovs' | featurestable$`WO-01` == 'osv' | featurestable$`WO-01` == 'v-last', 'O>V', 'ERROR: IMPOSSIBLE STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01d', stateCol, sourceCol, coderCol)
  
  #WO-01e
  stateCol <- ifelse(featurestable$`WO-01` == '?' | featurestable$`WO-01` == 's-last' | featurestable$`WO-01` == 'v-last', '?', 
              ifelse(featurestable$`WO-01` == 'no dominant order', 'no dominant order', 
              ifelse(featurestable$`WO-01` == 'svo' | featurestable$`WO-01` == 'sov', 's-first',
              ifelse(featurestable$`WO-01` == 'vso' | featurestable$`WO-01` == 'vos' | featurestable$`WO-01` == 'v-first', 'v-first',
              ifelse(featurestable$`WO-01` == 'osv' | featurestable$`WO-01` == 'ovs', 'o-first', 'ERROR: INVALID STATE')))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01e', stateCol, sourceCol, coderCol)
  
  #WO-01f
  stateCol <- ifelse(featurestable$`WO-01e` == '?', '?', 
              ifelse(featurestable$`WO-01e` != 's-first', 'NA', 
              ifelse(featurestable$`WO-01` == 'svo', 'svo',
              ifelse(featurestable$`WO-01` == 'sov' | featurestable$`WO-01` == 'v-last', 'sov', 'ERROR: INVALID STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01f', stateCol, sourceCol, coderCol)
  
  #WO-01g
  stateCol <- ifelse(featurestable$`WO-01e` == '?' | featurestable$`WO-01` == 'v-first', '?', 
              ifelse(featurestable$`WO-01e` != 'v-first', 'NA', 
              ifelse(featurestable$`WO-01` == 'vso', 'vso',
              ifelse(featurestable$`WO-01` == 'vos', 'vos', 'ERROR: INVALID STATE'))))
  sourceCol <- featurestable$`WO-01.Source`
  coderCol <- featurestable$`WO-01.Coder`
  featurestable <- addDerivedState(featurestable, 'WO-01g', stateCol, sourceCol, coderCol)
  
  #WO-01h
  stateCol <- ifelse(featurestable$`WO-01e` == '?', '?', 
              ifelse(featurestable$`WO-01e` != 'o-first', 'NA', 
              ifelse(featurestable$`WO-01` == 'ovs', 'ovs',
              ifelse(featurestable$`WO-01` == 'osv', 'osv', 'ERROR: INVALID STATE'))))
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
