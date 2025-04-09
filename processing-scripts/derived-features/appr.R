source('utils.R')

processApprehensional <- function(featurestable, lgnames, warnings, errors) {

  #Appr-12a
  stateCol <- ifelse(featurestable$`Appr-12` == 'NA', 'NA',
              ifelse(featurestable$`Appr-12` == '?', '?',
              str_count(gsub(" \\(apprehensive\\)| \\(precautioning\\)| \\(timitive\\)| \\(fear-complementation\\)","", featurestable$`Appr-12`), ';;') + 1))
  sourceCol <- featurestable$`Appr-12.Source`
  coderCol <- featurestable$`Appr-12.Coder`
  featurestable <- addDerivedState(featurestable, 'Appr-12a', stateCol, sourceCol, coderCol)
  
  #Appr-12b
  stateCol <- ifelse(featurestable$`Appr-01` == 'no', 'NA',
              ifelse(featurestable$`Appr-12` == '?', '?',
              sapply(gsub(" \\(apprehensive\\)| \\(precautioning\\)| \\(timitive\\)| \\(fear-complementation\\)","", featurestable$`Appr-12`), function(x) paste0(unique(trimws(unlist(strsplit(x, ';;|&')))), collapse=';;'))))
  sourceCol <- featurestable$`Appr-12.Source`
  coderCol <- featurestable$`Appr-12.Coder`
  featurestable <- addDerivedState(featurestable, 'Appr-12b', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors  
  
  errorsList <- dplyr::filter(featurestable, `Appr-01` == 'yes' & `Appr-02` != 'yes' & `Appr-06` != 'yes' & `Appr-09` != 'yes' & `Appr-10` != 'yes' & `Appr-02` != '?' & `Appr-06` != '?' & `Appr-10` != '?' & `Appr-09` != '?')
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-01 == yes => Appr-02 == yes | Appr-06 == yes | Appr-09 == yes | Appr-10 == yes', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-02` == 'yes' & `Appr-01` != 'yes')
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-02 == yes => Appr-01 == yes', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-06` == 'yes' & `Appr-01` != 'yes')
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-06 == yes => Appr-01 == yes', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-09` == 'yes' & `Appr-01` != 'yes')
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-09 == yes => Appr-01 == yes', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-10` == 'yes' & `Appr-01` != 'yes')
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-10 == yes => Appr-01 == yes', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-01` == 'no' & `Appr-02` != 'NA' & `Appr-03` != 'NA' & `Appr-04` != 'NA' & `Appr-05` != 'NA' & `Appr-06` != 'NA' & `Appr-07` != 'NA' & `Appr-08` != 'NA' & `Appr-09` != 'yes'  & `Appr-10` != 'NA' & `Appr-11` != 'NA'& `Appr-12` != 'NA')
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-01 == no => Appr-02 == NA & Appr-03 == NA & Appr-04 == NA & Appr-05 == NA & Appr-06 == NA  & Appr-07 == NA & Appr-08 == NA & Appr-09 == NA & Appr-10 == NA & Appr-11 == NA & Appr-12 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-02` == 'yes' & (`Appr-03` == 'NA' | `Appr-05` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-02 == yes => Appr-03 != NA & Appr-05 != NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, grepl('1', `Appr-03`) & `Appr-04` == 'NA')
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-03 includes <1> => Appr-04 != NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-06` == 'yes' & (`Appr-07` == 'NA' | `Appr-08` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-06 == yes => Appr-07 != NA & Appr-08 != NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-10` == 'yes' & `Appr-11` == 'NA')
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-10 == yes => Appr-11 != NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-02` == 'yes' & !grepl('A', `Appr-12`))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-02 == yes => Appr-12 includes <A>', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-06` == 'yes' & !grepl('P', `Appr-12`))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-06 == yes => Appr-12 includes <P>', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-09` == 'yes' & !grepl('F', `Appr-12`))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-09 == yes => Appr-12 includes <F>', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `Appr-10` == 'yes' & !grepl('T', `Appr-12`))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Appr', 'Appr-10 == yes => Appr-12 includes <T>', lgnames)
  

  #write warnings, errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}
