source('utils.R')

processGlottalized <- function(featurestable, lgnames, warnings, errors) {
  
  #Glot-01a
  stateCol <- ifelse(featurestable$`Glot-01` == 'yes' | featurestable$`Glot-02` == 'yes' | featurestable$`Glot-03` == 'yes', 'yes',
              ifelse(featurestable$`Glot-01` == 'no' & featurestable$`Glot-02` == 'no' & featurestable$`Glot-03` == 'no', 'no',
              ifelse(featurestable$`Glot-01` != 'yes' & featurestable$`Glot-02` != 'yes' & featurestable$`Glot-03` != 'yes' & (featurestable$`Glot-01` == '?' | featurestable$`Glot-02` == '?' | featurestable$`Glot-03` == '?'), '?', 'ERROR: IMPOSSIBLE STATE')))
  sourceCol <- collapseSourceColumns(list(featurestable$`Glot-01.Source`, featurestable$`Glot-02.Source`, featurestable$`Glot-03.Source`))
  coderCol <- sapply(paste0(featurestable$`Glot-01.Coder`, ';', featurestable$`Glot-02.Coder`, ';', featurestable$`Glot-03.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Glot-01a', stateCol, sourceCol, coderCol)
  
  #Glot-04a
  stateCol <- ifelse(featurestable$`Glot-04` == 'yes' | featurestable$`Glot-05` == 'yes' | featurestable$`Glot-06` == 'yes', 'yes',
              ifelse(featurestable$`Glot-04` == 'no' & featurestable$`Glot-05` == 'no' & featurestable$`Glot-06` == 'no', 'no',
              ifelse(featurestable$`Glot-04` != 'yes' & featurestable$`Glot-05` != 'yes' & featurestable$`Glot-06` != 'yes' & (featurestable$`Glot-04` == '?' | featurestable$`Glot-05` == '?' | featurestable$`Glot-06` == '?'), '?', 'ERROR: IMPOSSIBLE STATE')))
  sourceCol <- collapseSourceColumns(list(featurestable$`Glot-04.Source`, featurestable$`Glot-05.Source`, featurestable$`Glot-06.Source`))
  coderCol <- sapply(paste0(featurestable$`Glot-04.Coder`, ';', featurestable$`Glot-05.Coder`, ';', featurestable$`Glot-06.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Glot-04a', stateCol, sourceCol, coderCol)
  
  #Glot-04b
  stateCol <- ifelse(featurestable$`Glot-01a` == 'no', 'NA',
              ifelse((featurestable$`Glot-04` == 'yes' | featurestable$`Glot-05` == 'yes' | featurestable$`Glot-06` == 'yes') & featurestable$`Glot-01a` == 'yes', 'yes',
              ifelse(featurestable$`Glot-04` == 'no' & featurestable$`Glot-05` == 'no' & featurestable$`Glot-06` == 'no' & featurestable$`Glot-01a` == 'yes', 'no',
              ifelse(featurestable$`Glot-04` != 'yes' & featurestable$`Glot-05` != 'yes' & featurestable$`Glot-06` != 'yes' & (featurestable$`Glot-04` == '?' | featurestable$`Glot-05` == '?' | featurestable$`Glot-06` == '?') & featurestable$`Glot-01a` == 'yes', '?', 'ERROR: IMPOSSIBLE STATE'))))
  sourceCol <- collapseSourceColumns(list(featurestable$`Glot-04.Source`, featurestable$`Glot-05.Source`, featurestable$`Glot-06.Source`))
  coderCol <- sapply(paste0(featurestable$`Glot-04.Coder`, ';', featurestable$`Glot-05.Coder`, ';', featurestable$`Glot-06.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Glot-04b', stateCol, sourceCol, coderCol)
  
  #Glot-03a
  stateCol <- ifelse(featurestable$`Glot-01a` == '?' | featurestable$`Glot-03` == '?', '?', 
              ifelse(featurestable$`Glot-01a` == 'yes' & featurestable$`Glot-03` == 'yes', 'yes', 
              ifelse(featurestable$`Glot-01a` == 'yes' & featurestable$`Glot-03` == 'no', 'no', 'NA')))
  sourceCol <- collapseSourceColumns(list(featurestable$`Glot-01a.Source`, featurestable$`Glot-03.Source`))
  coderCol <- featurestable$`Glot-03.Coder`
  featurestable <- addDerivedState(featurestable, 'Glot-03a', stateCol, sourceCol, coderCol)
  
  #Glot-04c
  stateCol <- ifelse(featurestable$`Glot-04a` == '?' | featurestable$`Glot-04` == '?', '?', 
              ifelse(featurestable$`Glot-04a` == 'yes' & featurestable$`Glot-04` == 'yes', 'yes', 
              ifelse(featurestable$`Glot-04a` == 'yes' & featurestable$`Glot-04` == 'no', 'no', 'NA')))
  sourceCol <- collapseSourceColumns(list(featurestable$`Glot-04a.Source`, featurestable$`Glot-04.Source`))
  coderCol <- featurestable$`Glot-04.Coder`
  featurestable <- addDerivedState(featurestable, 'Glot-04c', stateCol, sourceCol, coderCol)
  
  #Glot-05a
  stateCol <- ifelse(featurestable$`Glot-04a` == '?' | featurestable$`Glot-05` == '?', '?', 
              ifelse(featurestable$`Glot-04a` == 'yes' & featurestable$`Glot-05` == 'yes', 'yes', 
              ifelse(featurestable$`Glot-04a` == 'yes' & featurestable$`Glot-05` == 'no', 'no', 'NA')))
  sourceCol <- collapseSourceColumns(list(featurestable$`Glot-04a.Source`, featurestable$`Glot-05.Source`))
  coderCol <- featurestable$`Glot-05.Coder`
  featurestable <- addDerivedState(featurestable, 'Glot-05a', stateCol, sourceCol, coderCol)
  
  #Glot-06a
  stateCol <- ifelse(featurestable$`Glot-04a` == '?' | featurestable$`Glot-06` == '?', '?', 
              ifelse(featurestable$`Glot-04a` == 'yes' & featurestable$`Glot-06` == 'yes', 'yes', 
              ifelse(featurestable$`Glot-04a` == 'yes' & featurestable$`Glot-06` == 'no', 'no', 'NA')))
  sourceCol <- collapseSourceColumns(list(featurestable$`Glot-04a.Source`, featurestable$`Glot-06.Source`))
  coderCol <- featurestable$`Glot-06.Coder`
  featurestable <- addDerivedState(featurestable, 'Glot-06a', stateCol, sourceCol, coderCol)
  
  #warnings
  warningsInt <- warnings
  warningsList <- dplyr::filter(featurestable, (`Glot-02` == 'yes' | `Glot-03` == 'yes' | `Glot-04` == 'yes' | `Glot-05` == 'yes' | `Glot-06` == 'yes') & (`Glot-01` != 'yes'))
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'Glot', "Glot-01 is not 'yes' but Glot-02 - Glot-06 contains at least one 'yes'", lgnames)
  
  #write warnings, errors, to global variable
  warningobj <- deparse(substitute(warnings))
  assign(warningobj, value = warningsInt, envir = parent.frame())
  
  return(featurestable)
}
