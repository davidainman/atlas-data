source('utils.R')

processCoronals <- function(featurestable, lgnames, warnings, errors) {
  
  #Cor-01a
  stateCol <- ifelse(featurestable$`Cor-01` == '?', '?',
              ifelse(featurestable$`Cor-01` == 1, 1,
              ifelse(featurestable$`Cor-01` >= 2, 'more than 1', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-01.Source`
  coderCol <- featurestable$`Cor-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-01a', stateCol, sourceCol, coderCol)
  
  #Cor-01b
  stateCol <- ifelse(featurestable$`Cor-01` == '?', '?',
              ifelse(featurestable$`Cor-01` <= 2, 'up to 2',
              ifelse(featurestable$`Cor-01` >= 3, 'more than 2', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-01.Source`
  coderCol <- featurestable$`Cor-01.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-01b', stateCol, sourceCol, coderCol)
  
  #Cor-02a
  stateCol <- ifelse(featurestable$`Cor-02` == '?', '?',
              ifelse(featurestable$`Cor-02` == 0, 'no',
              ifelse(featurestable$`Cor-02` >= 1, 'yes', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-02.Source`
  coderCol <- featurestable$`Cor-02.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-02a', stateCol, sourceCol, coderCol)
  
  #Cor-02b
  stateCol <- ifelse(featurestable$`Cor-02` == '?', '?',
              ifelse(featurestable$`Cor-02` <= 1, 'up to 1',
              ifelse(featurestable$`Cor-02` >= 2, 'more than 1', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-02.Source`
  coderCol <- featurestable$`Cor-02.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-02b', stateCol, sourceCol, coderCol)
  
  #Cor-02c
  stateCol <- ifelse(featurestable$`Cor-02` == '?', '?',
              ifelse(featurestable$`Cor-02` <= 2, 'up to 2',
              ifelse(featurestable$`Cor-02` >= 3, 'more than 2', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-02.Source`
  coderCol <- featurestable$`Cor-02.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-02c', stateCol, sourceCol, coderCol)
  
  #Cor-02d
  stateCol <- ifelse(featurestable$`Cor-02` == '?', '?',
              ifelse(featurestable$`Cor-02` <= 3, 'up to 3',
              ifelse(featurestable$`Cor-02` >= 4, 'more than 3', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-02.Source`
  coderCol <- featurestable$`Cor-02.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-02d', stateCol, sourceCol, coderCol)
  
  #Cor-03a
  stateCol <- ifelse(featurestable$`Cor-03` == '?', '?',
              ifelse(featurestable$`Cor-03` == 0, 'no',
              ifelse(featurestable$`Cor-03` >= 1, 'yes', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-03.Source`
  coderCol <- featurestable$`Cor-03.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-03a', stateCol, sourceCol, coderCol)
  
  #Cor-03b
  stateCol <- ifelse(featurestable$`Cor-03` == '?', '?',
              ifelse(featurestable$`Cor-03` <= 1, 'up to 1',
              ifelse(featurestable$`Cor-03` >= 2, 'more than 1', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-03.Source`
  coderCol <- featurestable$`Cor-03.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-03b', stateCol, sourceCol, coderCol)
  
  #Cor-03c
  stateCol <- ifelse(featurestable$`Cor-03` == '?', '?',
              ifelse(featurestable$`Cor-03` <= 2, 'up to 2',
              ifelse(featurestable$`Cor-03` >= 3, 'more than 2', 'ERROR: Impossible state')))
  sourceCol <- featurestable$`Cor-03.Source`
  coderCol <- featurestable$`Cor-03.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-03c', stateCol, sourceCol, coderCol)
  
  #Cor-04a
  stateCol <- ifelse(featurestable$`Cor-04` == '?', '?',
              ifelse(featurestable$`Cor-01` == 1, 'NA',
              ifelse(featurestable$`Cor-04` == 'yes', 'yes',
              ifelse(featurestable$`Cor-04` == 'no', 'no', 'ERROR: Impossible state'))))
  sourceCol <- featurestable$`Cor-04.Source`
  coderCol <- featurestable$`Cor-04.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-04a', stateCol, sourceCol, coderCol)
  
  #Cor-07a
  stateCol <- ifelse(featurestable$`Cor-07` == '?', '?',
              ifelse(featurestable$`Cor-03` == 1, 'NA',
              ifelse(featurestable$`Cor-07` == 'yes', 'yes',
              ifelse(featurestable$`Cor-07` == 'no', 'no', 'ERROR: Impossible state'))))
  sourceCol <- featurestable$`Cor-07.Source`
  coderCol <- featurestable$`Cor-07.Coder`
  featurestable <- addDerivedState(featurestable, 'Cor-07a', stateCol, sourceCol, coderCol)
  
  
  #errors=
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, (`Cor-07` == 'no' & (`Cor-08` != 'NA' | `Cor-09` != 'NA')) | (`Cor-07` == 'yes' & (`Cor-08` == 'NA' | `Cor-09` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Cor', 'Cor.07 = no <=> Cor.08 = NA & Cor.09 = NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Cor-07` == 'yes' & `Cor-08` != 'yes' & `Cor-09` != 'yes'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Cor', 'Cor.07 = yes => One of Cor.08 or Cor.09 should be <yes>', lgnames)
  
  #warnings
  warningsInt <- warnings
  
  warningsList <- dplyr::filter(featurestable, `Cor-01` == 2 & !(`Cor-04` == 'yes' | `Cor-05` == 'yes'))
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'Cor', "Cor-01 = 2 and neither Cor-04 nor Cor-05 = 'yes'", lgnames)
  
  warningsList <- dplyr::filter(featurestable, `Cor-01` >= 3 & !(`Cor-04` == 'yes' & `Cor-05` == 'yes'))
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'Cor', "Cor-01 >= 3, it is expected that Cor-04 = 'yes' AND Cor-05 = 'yes'", lgnames)
  
  warningsList <- dplyr::filter(featurestable, `Cor-07` == 'yes' & `Cor-03` == 1)
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'Cor', "Cor-07 = 'yes', it is expected that Cor-03 != 1", lgnames)
  
  #write warnings, errors, to global variable
  warningobj <- deparse(substitute(warnings))
  assign(warningobj, value = warningsInt, envir = parent.frame())
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}