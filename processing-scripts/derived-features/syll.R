source('utils.R')

processSyllable <- function(featurestable, lgnames, warnings, errors) {
  
  #errors
  errorsInt <- errors
  
  errorsList <- suppressWarnings(dplyr::filter(featurestable, !is.na(as.numeric(`Syll-01`)) & !is.na(as.numeric(`Syll-02`)) & !is.na(as.numeric(`Syll-03`)) & (as.numeric(`Syll-03`) != as.numeric(`Syll-01`) + as.numeric(`Syll-02`))))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Syll', 'Syll-03 != Syll-01 + Syll-02', lgnames)
  
  errorsList <- suppressWarnings(dplyr::filter(featurestable, !is.na(as.numeric(`Syll-02`)) & (`Syll-02` >= 1 & `Syll-04` == 'simple')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Syll', "Syll-02 >= 1 and Syll-04 = 'simple'", lgnames)
  
  errorsList <- suppressWarnings(dplyr::filter(featurestable, !is.na(as.numeric(`Syll-02`)) & !is.na(as.numeric(`Syll-03`)) & (`Syll-01` >= 3 | `Syll-02` >= 2) & (`Syll-04` == 'simple' | `Syll-04` == 'moderately complex')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Syll', "Syll-01 >= 3 or Syll-02 >= 2 ==> Syll-04 != 'simple' or 'moderately complex'", lgnames)
  
  errorsList <- suppressWarnings(dplyr::filter(featurestable, !is.na(as.numeric(`Syll-01`)) & !is.na(as.numeric(`Syll-02`)) & (`Syll-01` >= 4 | `Syll-02` >= 4) & (`Syll-04` != 'highly complex')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Syll', "Syll-01 >= 4 or Syll-02 >= 4 ==> Syll-04 == 'highly complex'", lgnames)
  
  errorsList <- suppressWarnings(dplyr::filter(featurestable, (!is.na(as.numeric(`Syll-02`)) & `Syll-02` == 0 & `Syll-06` != 'nocoda') | (`Syll-06` == 'nocoda' & `Syll-02` != 0)))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Syll', "Syll-02 == 0 <=> Syll-06 == 'nocoda'", lgnames)
  
  errorsList <- suppressWarnings(dplyr::filter(featurestable, (!is.na(as.numeric(`Syll-02`)) & `Syll-02` == 1 & (`Syll-06` != 'sonorant' & `Syll-06` != 'simple')) | ((`Syll-06` == 'sonorant' | `Syll-06` == 'simple') & `Syll-02` != 1)))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Syll', "Syll-02 == 1 <=> Syll-06 == 'sonorant' | Syll-06 == 'simple'", lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Syll-05` == 'CG' & `Syll-03` == 'simple'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Syll', "Syll-05 == CG => Syll-03 != 'simple'", lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Syll-05` == 'CCplus' & (`Syll-03` == 'simple' | `Syll-03` == 'moderately complex')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Syll', "Syll-05 == CCplus => Syll-03 != 'simple' & Syll-03 != 'moderately complex'", lgnames)
  
  #warnings
  warningsInt <- warnings
  
  warningsList <- suppressWarnings(dplyr::filter(featurestable, (!is.na(as.numeric(`Syll-02`)) & `Syll-02` > 1 & (`Syll-06` != 'complex')) | (`Syll-06` == 'complex' & (is.na(as.numeric(`Syll-02`) | `Syll-02` <= 1)))))
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'Syll', "Syll-02 (coda number) >= 2 <=> Syll-06 (coda complexity) == 'complex'", lgnames)
  
  #write warnings, errors, to global variable
  warningobj <- deparse(substitute(warnings))
  assign(warningobj, value = warningsInt, envir = parent.frame())
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}