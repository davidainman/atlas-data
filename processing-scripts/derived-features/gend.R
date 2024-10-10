source('utils.R')

processGenderlects <- function(featurestable, lgnames, warnings, errors) {
  
  #`Gend-03a`
  stateCol <- ifelse(featurestable$`Gend-01` == '?' | featurestable$`Gend-03` == '?' | featurestable$`Gend-05` == '?' | featurestable$`Gend-09` == '?', '?', 
              ifelse(featurestable$`Gend-01` == 'no', 'NA',
              ifelse((featurestable$`Gend-03` == 'speaker' | featurestable$`Gend-05` == 'speaker' | featurestable$`Gend-09` == 'speaker') & ((featurestable$`Gend-03` == 'speaker' | featurestable$`Gend-03` == 'NA') & (featurestable$`Gend-05` == 'speaker' | featurestable$`Gend-05` == 'NA') & (featurestable$`Gend-09` == 'speaker' | featurestable$`Gend-09` == 'NA')), 'speaker', 
              ifelse((featurestable$`Gend-03` == 'addressee' | featurestable$`Gend-05` == 'addressee' | featurestable$`Gend-09` == 'addressee') & ((featurestable$`Gend-03` == 'addressee' | featurestable$`Gend-03` == 'NA') & (featurestable$`Gend-05` == 'addressee' | featurestable$`Gend-05` == 'NA') & (featurestable$`Gend-09` == 'addressee' | featurestable$`Gend-09` == 'NA')), 'addressee',
              ifelse((featurestable$`Gend-03` == 'both' | featurestable$`Gend-05` == 'both' | featurestable$`Gend-09` == 'both') & ((featurestable$`Gend-03` == 'both' | featurestable$`Gend-03` == 'NA') & (featurestable$`Gend-05` == 'both' | featurestable$`Gend-05` == 'NA') & (featurestable$`Gend-09` == 'both' | featurestable$`Gend-09` == 'NA')), 'both',
              ifelse(featurestable$`Gend-01` == 'yes', 'mixed', 'ERROR: IMPOSSIBLE STATE'))))))
  sourceCol <- collapseSourceColumns(list(featurestable$`Gend-01.Source`, featurestable$`Gend-03.Source`, featurestable$`Gend-05.Source`, featurestable$`Gend-09.Source`))
  coderCol <- lapply(paste0(featurestable$`Gend-01.Coder`, ';', featurestable$`Gend-03.Coder`, ';', featurestable$`Gend-05.Coder`, ';', featurestable$`Gend-09.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Gend-03a', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, (`Gend-01` == 'no' & (`Gend-02` != 'NA' | `Gend-03` != 'NA' | `Gend-04` != 'NA' | `Gend-05` != 'NA' | `Gend-06` != 'NA' | `Gend-07` != 'NA' | `Gend-08` != 'NA' | `Gend-09` != 'NA' | `Gend-10` != 'NA' | `Gend-11` != 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Gend', 'Gend-01 = no => All other Gend features = NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Gend-02` == 'no' & `Gend-03` != 'NA') | (`Gend-02` == 'yes' & `Gend-03` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Gend', 'Gend-02 = no => Gend-03 = NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Gend-04` == 'no' & (`Gend-05` != 'NA' | `Gend-06` != 'NA')) | (`Gend-04` == 'yes' & (`Gend-05` == 'NA' | `Gend-06` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Gend', 'Gend-04 = no => Gend-05 = NA & Gend-06 = NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Gend-07` == 'no' & !(`Gend-08` == 'NA' & `Gend-09` == 'NA' & `Gend-10` == 'NA' & `Gend-11` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Gend', 'Gend-07 = no => Gend-08 = NA & Gend-09 = NA & Gend-10 = NA & Gend-11 = NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Gend-01` == 'yes' & !(`Gend-02` == 'yes' | `Gend-02` == '?' | `Gend-04` == 'yes' | `Gend-04` == '?' | `Gend-07` == 'yes' | `Gend-07` == '?')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Gend', 'Gend-01 = yes => Gend-02 = yes|? | Gend-04 = yes|? | Gend-07 = yes|?', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Gend-01` == 'yes' & (`Gend-02` == 'NA' | `Gend-04` == 'NA' | `Gend-07` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Gend', 'Gend-01 = yes => Gend-02 != NA & Gend-04 != NA | Gend-07 != NA', lgnames)
  
  #warnings
  warningsInt <- warnings
  
  #write warnings, errors, to global variable
  warningobj <- deparse(substitute(warnings))
  assign(warningobj, value = warningsInt, envir = parent.frame())
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}