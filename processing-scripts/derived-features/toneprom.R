source('utils.R')

processToneProm <- function(featurestable, lgnames, warnings, errors) {
  
  #ToneProm-02a
  stateCol <- ifelse(featurestable$`ToneProm-02` == '?', '?',
              ifelse(featurestable$`ToneProm-02` == 'NA', 'NA',
              sapply(featurestable$`ToneProm-02`, function(x) paste0(sort(unique(trimws(unlist(strsplit(x, ';;|&'))))), collapse=';;'))))
  sourceCol <- featurestable$`ToneProm-02.Source`
  coderCol <- featurestable$`ToneProm-02.Coder`
  featurestable <- addDerivedState(featurestable, 'ToneProm-02a', stateCol, sourceCol, coderCol)
  
  #ToneProm-11a
  stateCol <- ifelse(featurestable$`ToneProm-11` == '?', '?',
              ifelse(featurestable$`ToneProm-11` == 'NA', 'NA',
              ifelse(featurestable$`ToneProm-11` == 2, '2',
              ifelse(featurestable$`ToneProm-11` >= 3, "more than 2", "Error: Impossible state!"))))
  sourceCol <- featurestable$`ToneProm-11.Source`
  coderCol <- featurestable$`ToneProm-11.Coder`
  featurestable <- addDerivedState(featurestable, 'ToneProm-11a', stateCol, sourceCol, coderCol)
  
  #ToneProm-11b
  stateCol <- ifelse(featurestable$`ToneProm-11` == '?', '?',
              ifelse(featurestable$`ToneProm-11` == 'NA', 'NA',
              ifelse(featurestable$`ToneProm-11` == 2 | featurestable$`ToneProm-11` == 3, 'up to 3',
              ifelse(featurestable$`ToneProm-11` >= 4, "more than 3", "Error: Impossible state!"))))
  sourceCol <- featurestable$`ToneProm-11.Source`
  coderCol <- featurestable$`ToneProm-11.Coder`
  featurestable <- addDerivedState(featurestable, 'ToneProm-11b', stateCol, sourceCol, coderCol)
  
  #ToneProm-11c
  stateCol <- ifelse(featurestable$`ToneProm-11` == '?', '?',
              ifelse(featurestable$`ToneProm-11` == 'NA', 'NA',
              ifelse(featurestable$`ToneProm-11` == 2 | featurestable$`ToneProm-11` == 3 | featurestable$`ToneProm-11` == 4, 'up to 4',
              ifelse(featurestable$`ToneProm-11` >= 5, "more than 4", "Error: Impossible state!"))))
  sourceCol <- featurestable$`ToneProm-11.Source`
  coderCol <- featurestable$`ToneProm-11.Coder`
  featurestable <- addDerivedState(featurestable, 'ToneProm-11c', stateCol, sourceCol, coderCol)
  
  #ToneProm-09a
  stateCol <- ifelse(featurestable$`ToneProm-03` == 'yes' | featurestable$`ToneProm-09` == 'yes', 'yes',
              ifelse((featurestable$`ToneProm-03` == "no" | featurestable$`ToneProm-03` == 'NA') & featurestable$`ToneProm-09` == 'no', "no", 
              ifelse(featurestable$`ToneProm-03` == '?' | featurestable$`ToneProm-09` == '?', '?', "Error: Impossible state!")))
  sourceCol <- collapseSourceColumns(list(featurestable$`ToneProm-03.Source`, featurestable$`ToneProm-09.Source`))
  coderCol <- lapply(paste0(featurestable$`ToneProm-03.Coder`, ';', featurestable$`ToneProm-09.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'ToneProm-09a', stateCol, sourceCol, coderCol)
  
  #ToneProm-09b
  stateCol <- ifelse((featurestable$`ToneProm-03` == 'yes' & featurestable$`ToneProm-04` == 'contrastive') | (featurestable$`ToneProm-09` == 'yes'), 'yes',
              ifelse((featurestable$`ToneProm-03` == 'no' | featurestable$`ToneProm-03` == 'NA' | (featurestable$`ToneProm-03` == 'yes' & featurestable$`ToneProm-04` == 'non-contrastive')) & featurestable$`ToneProm-09` == 'no', 'no', 
              ifelse((featurestable$`ToneProm-03` == '?' & featurestable$`ToneProm-09` != 'yes') | (featurestable$`ToneProm-03` == 'yes' & featurestable$`ToneProm-04` == '?' & featurestable$`ToneProm-09` != 'yes') | (featurestable$`ToneProm-09` == '?' & featurestable$`ToneProm-04` != 'contrastive'), '?', "Error: Impossible state!")))
  sourceCol <- collapseSourceColumns(list(featurestable$`ToneProm-03.Source`, featurestable$`ToneProm-04.Source`, featurestable$`ToneProm-09.Source`))
  coderCol <- lapply(paste0(featurestable$`ToneProm-03.Coder`, ';', featurestable$`ToneProm-04.Coder`, ';', featurestable$`ToneProm-09.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'ToneProm-09b', stateCol, sourceCol, coderCol)
  
  #ToneProm-01a
  stateCol <- ifelse(featurestable$`ToneProm-01` == '?' | featurestable$`ToneProm-04` == '?' | featurestable$`ToneProm-06` == '?' | featurestable$`ToneProm-08` == '?', '?', 
              ifelse(featurestable$`ToneProm-01` == 'no' | (featurestable$`ToneProm-04` == 'NA' & featurestable$`ToneProm-06` == 'NA' & featurestable$`ToneProm-08` == 'NA'), 'NA', 
              ifelse(featurestable$`ToneProm-04` == "non-contrastive" | featurestable$`ToneProm-06` == 'non-contrastive' | featurestable$`ToneProm-08` == 'non-contrastive' & (featurestable$`ToneProm-04` != 'contrastive' & featurestable$`ToneProm-06` != 'contrastive' & featurestable$`ToneProm-08` != 'contrastive'), "non-contrastive", 
              ifelse(featurestable$`ToneProm-04` == 'contrastive' | featurestable$`ToneProm-06` == 'contrastive' | featurestable$`ToneProm-08` == 'contrastive', "contrastive", "ERROR: impossible state"))))
  sourceCol <- collapseSourceColumns(list(featurestable$`ToneProm-04.Source`, featurestable$`ToneProm-06.Source`, featurestable$`ToneProm-08.Source`))
  coderCol <- lapply(paste0(featurestable$`ToneProm-04.Coder`, ';', featurestable$`ToneProm-06.Coder`, ';', featurestable$`ToneProm-08.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'ToneProm-01a', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-01` == 'no' & (`ToneProm-02` != 'NA' | `ToneProm-03` != 'NA' | `ToneProm-04` != 'NA' | `ToneProm-05` != 'NA' | `ToneProm-06` != 'NA' | `ToneProm-07` != 'NA' | `ToneProm-08` != 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-01 == no => ToneProm-02 == NA & ToneProm-03 == NA & ToneProm-04 == NA & ToneProm-05 == NA & ToneProm006 == NA & ToneProm-07 == NA & ToneProm-08 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-01` == 'yes' & `ToneProm-03` == 'no' & `ToneProm-05` == 'no' & `ToneProm-07` == 'no'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-01 == yes & no primary tone system given in ToneProm-02 - ToneProm-07', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-03` == 'no' & `ToneProm-04` != 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-03 == no => ToneProm-04 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-03` == 'yes' & `ToneProm-04` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-03 == yes => ToneProm-04 != NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-03` == 'yes' & !grepl('pitch', `ToneProm-02`, perl=TRUE)))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', "ToneProm-03 == yes => ToneProm-02 contains 'pitch'", lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-05` == 'no' & `ToneProm-06` != 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-05 == no => ToneProm-06 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-05` == 'yes' & `ToneProm-06` == 'no'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-05 == yes => ToneProm-06 != NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-05` == 'yes' & !grepl('amplitude', `ToneProm-02`, perl=TRUE)))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', "ToneProm-05 == yes => ToneProm-02 contains 'amplitude'", lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-07` == 'no' & `ToneProm-08` != 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-07 == no => ToneProm-08 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-07` == 'yes' & `ToneProm-08` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-07 == yes => ToneProm-08 != NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`ToneProm-07` == 'yes' & !grepl('duration', `ToneProm-02`, perl=TRUE)))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', "ToneProm-07 == yes => ToneProm-02 contains 'duration'", lgnames)
  
  errorsList <- dplyr::filter(featurestable, `ToneProm-09` == 'no' & (`ToneProm-10` != 'NA' | `ToneProm-11` != 'NA' | `ToneProm-12` != 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-09 == no => ToneProm-10 == NA & ToneProm-11 == NA & ToneProm-12 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `ToneProm-09` == 'yes' & (`ToneProm-10` == 'NA' | `ToneProm-11` == 'NA' | `ToneProm-12` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'ToneProm', 'ToneProm-09 == yes => ToneProm-10 != NA & ToneProm-11 != NA & ToneProm-12 != NA', lgnames)
  
  #warnings
  warningsInt <- warnings
  
  warningsList <- dplyr::filter(featurestable, `ToneProm-01` == 'yes' & `ToneProm-09` == 'yes')
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'ToneProm', 'Both tone and accent present: ToneProm-01 == yes & ToneProm-09 == yes', lgnames)
  
  #write warnings, errors, to global variable
  warningobj <- deparse(substitute(warnings))
  assign(warningobj, value = warningsInt, envir = parent.frame())
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}
