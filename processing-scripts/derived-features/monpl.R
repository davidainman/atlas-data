source('utils.R')
suppressPackageStartupMessages(library(stringr))

processMonPl <- function(featurestable, lgnames, warnings, errors) {
  
  #regularize MonPl-06
  for(glot in featurestable$LanguageID[featurestable$LanguageID != 'question']) {
    monpls <- c()
    for (monpl in trimws(unlist(strsplit(featurestable$`MonPl-06`[featurestable$LanguageID == glot], ';')))) {
      monpls <- c(monpls, compactMonPl(explodeMonPl(monpl)))
    }
    featurestable$`MonPl-06`[featurestable$LanguageID == glot] <- paste0(monpls,collapse='; ')
  }
  
  #MonPl-03a
  stateCol <- ifelse(featurestable$`MonPl-03` == 'NA', 'NA',
              ifelse(featurestable$`MonPl-03` == '?', '?',
              ifelse(str_detect(trimws(strsplit(featurestable$`MonPl-03`, ';')), '\\bS\\b'), 'yes', 'no')))
  sourceCol <- featurestable$`MonPl-03.Source`
  coderCol <- featurestable$`MonPl-03.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-03a', stateCol, sourceCol, coderCol)
  
  #MonPl-03b
  stateCol <- ifelse(featurestable$`MonPl-03` == 'NA', 'NA',
              ifelse(featurestable$`MonPl-03` == '?', '?',
              ifelse(str_detect(trimws(strsplit(featurestable$`MonPl-03`, ';')), '\\bA\\b'), 'yes', 'no')))
  sourceCol <- featurestable$`MonPl-03.Source`
  coderCol <- featurestable$`MonPl-03.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-03b', stateCol, sourceCol, coderCol)
  
  #MonPl-03c
  stateCol <- ifelse(featurestable$`MonPl-03` == 'NA', 'NA',
              ifelse(featurestable$`MonPl-03` == '?', '?',
              ifelse(str_detect(trimws(strsplit(featurestable$`MonPl-03`, ';')), '\\bP\\b'), 'yes', 'no')))
  sourceCol <- featurestable$`MonPl-03.Source`
  coderCol <- featurestable$`MonPl-03.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-03c', stateCol, sourceCol, coderCol)
  
  #MonPl-05a
  stateCol <- ifelse(featurestable$`MonPl-05` == 'NA', 'NA',
              ifelse(featurestable$`MonPl-05` == '?', '?',
              ifelse(str_detect(trimws(strsplit(featurestable$`MonPl-05`, ';')), '\\bSA\\b'), 'yes', 'no')))
  sourceCol <- featurestable$`MonPl-05.Source`
  coderCol <- featurestable$`MonPl-05.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-05a', stateCol, sourceCol, coderCol)
  
  #MonPl-05b
  stateCol <- ifelse(featurestable$`MonPl-05` == 'NA', 'NA',
              ifelse(featurestable$`MonPl-05` == '?', '?',
              ifelse(str_detect(trimws(strsplit(featurestable$`MonPl-05`, ';')), '\\bSP\\b'), 'yes', 'no')))
  sourceCol <- featurestable$`MonPl-05.Source`
  coderCol <- featurestable$`MonPl-05.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-05b', stateCol, sourceCol, coderCol)
  
  #MonPl-05c
  stateCol <- ifelse(featurestable$`MonPl-05` == 'NA', 'NA',
              ifelse(featurestable$`MonPl-05` == '?', '?',
              ifelse(str_detect(trimws(strsplit(featurestable$`MonPl-05`, ';')), '\\bSAP\\b'), 'yes', 'no')))
  sourceCol <- featurestable$`MonPl-05.Source`
  coderCol <- featurestable$`MonPl-05.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-05c', stateCol, sourceCol, coderCol)
  
  #MonPl-05d
  stateCol <- ifelse(is.na(featurestable$`MonPl-01`), NA,
              ifelse(featurestable$`MonPl-03a` == 'yes' |
                     featurestable$`MonPl-05a` == 'yes' |
                     featurestable$`MonPl-05b` == 'yes' |
                     featurestable$`MonPl-05c` == 'yes', 'yes', 'no'))
  sourceCol <- collapseSourceColumns(list(featurestable$`MonPl-03.Source`, featurestable$`MonPl-05.Source`))
  coderCol <- featurestable$`MonPl-05.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-05d', stateCol, sourceCol, coderCol)
  
  #MonPl.05e
  stateCol <- ifelse(is.na(featurestable$`MonPl-06`), NA,
              ifelse(featurestable$`MonPl-05` == 'NA' | featurestable$`MonPl-05` == 'no', 'NA',
              ifelse(featurestable$`MonPl-05` == '?', '?',
              ifelse((featurestable$`MonPl-03b` == 'yes' | featurestable$`MonPl-05a` == 'yes') & 
                      featurestable$`MonPl-03c` != 'yes' & featurestable$`MonPl-05b` != 'yes' & 
                      featurestable$`MonPl-05c` != 'yes', 'A',
              ifelse((featurestable$`MonPl-03c` == 'yes' | featurestable$`MonPl-05b` == 'yes') & 
                      featurestable$`MonPl-03b` != 'yes' & featurestable$`MonPl-05a` != 'yes' & 
                      featurestable$`MonPl-05c` != 'yes', 'P',       
              ifelse(featurestable$`MonPl-05c` == 'yes' & 
                     featurestable$`MonPl-03b` != 'yes' & featurestable$`MonPl-05a` != 'yes' & 
                     featurestable$`MonPl-03c` != 'yes' & featurestable$`MonPl-05a` != 'yes', 'A and P', '?'))))))
  sourceCol <- collapseSourceColumns(list(featurestable$`MonPl-03.Source`, featurestable$`MonPl-05.Source`))
  coderCol <- featurestable$`MonPl-05.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-05e', stateCol, sourceCol, coderCol)
  
  #MonPl-06a
  nonverbal <- c('noun', 'pronoun', 'adjective', 'demonstrative')
  nonverbalother <- c('particle', 'numeral', 'adposition', 'adverb')
  collapse_nonverbal <- function(x, add_other) {
    retval <- intersect(nonverbal, unique(trimws(unlist(strsplit(x, ';|&')))))
    if (length(intersect(nonverbalother, unique(trimws(unlist(strsplit(x, ';|&')))))) > 0) {
      retval <- c(retval, 'other')
    }
    return(paste0(sort(retval), collapse=';;'))
  }
  stateCol <- ifelse(featurestable$`MonPl-06` == 'NA', 'NA',
              ifelse(featurestable$`MonPl-06` == '?', '?',
              ifelse(!(grepl(paste0(nonverbal,collapse='|'), featurestable$`MonPl-06`)), 'None',
              sapply(featurestable$`MonPl-06`, collapse_nonverbal))))
  sourceCol <- featurestable$`MonPl-06.Source`
  coderCol <- featurestable$`MonPl-06.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-06a', stateCol, sourceCol, coderCol)
  
  #MonPl-06b
  stateCol <- ifelse(featurestable$`MonPl-06` == 'NA', 'NA',
              ifelse(featurestable$`MonPl-06` == '?', '?',
              ifelse(grepl('(?<!pro)noun', featurestable$`MonPl-06`, perl=TRUE), 'yes', 'no')))
  sourceCol <- featurestable$`MonPl-06.Source`
  coderCol <- featurestable$`MonPl-06.Coder`
  featurestable <- addDerivedState(featurestable, 'MonPl-06b', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, (`MonPl-01` == 'no' & (`MonPl-02` != 'NA' | `MonPl-03` != 'NA' | `MonPl-04` != 'NA' | `MonPl-05` != 'NA' | `MonPl-06` != 'NA')) | (`MonPl-01` != 'no' & (`MonPl-02` == 'NA' | `MonPl-03` == 'NA'  | `MonPl-04` == 'NA'  | `MonPl-05` == 'NA' | `MonPl-06` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'MonPl', 'MonPl-01 == no <=> MonPl-02 == NA & MonPl-03 == NA & MonPl-04 == NA & MonPl-05 == NA & MonPl-06 == NA', lgnames)

  errorsList <- dplyr::filter(featurestable, (`MonPl-03` == 'no' & `MonPl-05` == 'no'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'MonPl', 'Impossible combination: MonPl-03 == no & MonPl-05 == no', lgnames)
  
  #warnings
  warningsInt <- warnings
  
  warningsList <- dplyr::filter(featurestable, (`MonPl-03` == 'S' | `MonPl-03` == 'A' | `MonPl-03` == 'P')  & `MonPl-05` != 'no')
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'MonPl', 'MonPl-03 == S | A | P & MonPl-05 != no', lgnames)
  
  #write warnings, errors, to global variable
  warningobj <- deparse(substitute(warnings))
  assign(warningobj, value = warningsInt, envir = parent.frame())
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}