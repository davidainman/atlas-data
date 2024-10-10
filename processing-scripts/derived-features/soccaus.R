source('utils.R')

processSocCaus <- function(featurestable, lgnames, warnings, errors) {
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, (`SocCaus-01` != 'no' & (`SocCaus-02` == 'NA' | `SocCaus-03` == 'NA' | `SocCaus-04` == 'NA' | `SocCaus-05` == 'NA' | `SocCaus-06` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SocCaus', 'SocCaus-01 == no <=> SocCaus-02 == NA & SocCaus-03 == NA & SocCaus-04 == NA & SocCaus-05 == NA', lgnames)
  
  #write warnings, errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}