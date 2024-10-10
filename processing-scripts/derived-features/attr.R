source('utils.R')

processAttributives <- function(featurestable, lgnames, warnings, errors) {
  
  #Attr-01a
  stateCol <- ifelse(featurestable$`Attr-01` == 'NA' | featurestable$`Attr-01` == 'predicative only', 'NA',
              ifelse(featurestable$`Attr-01` == '?' | featurestable$`Attr-02` == '?' | (featurestable$`Attr-01` == 'adjective' & featurestable$`Attr-02` == 'closed'), '?', 
              ifelse(featurestable$`Attr-01` == 'modifier' | (featurestable$`Attr-01` == 'adjective' & (featurestable$`Attr-02` == 'open' | featurestable$`Attr-02` == 'open but')) | featurestable$`Attr-01` == 'adverb', 'modifier pos', 
              ifelse(featurestable$`Attr-01` == 'noun', 'noun',
              ifelse(featurestable$`Attr-01` == 'verb', 'verb', 'ERROR: IMPOSSIBLE STATE')))))
  sourceCol <- collapseSourceColumns(list(featurestable$`Attr-01.Source`, featurestable$`Attr-02.Source`))
  coderCol <- lapply(paste0(featurestable$`Attr-01.Coder`, ';', featurestable$`Attr-02.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'Attr-01a', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, ((`Attr-01` == 'adjective' | `Attr-01` == 'modifier') & `Attr-02` == 'NA') | (`Attr-01` != 'adjective' & `Attr-01` != 'modifier' & `Attr-02` != 'NA' & `Attr-02` != '?'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Attr', '`Attr-01` == "adjective" | "modifier" <=> `Attr-02` != NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`Attr-02` != '?' & `Attr-02` != 'open but' & `Attr-03` != '?' & `Attr-03` != 'NA') | (`Attr-02` == 'open but' & `Attr-03` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'Attr', '`Attr-02` == "open but" <=> `Attr-03` == NA', lgnames)
  
  #write errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}
