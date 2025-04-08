source('utils.R')

processSgPl <- function(featurestable, verblist, lgnames, warnings, errors) {
  
  #SgPl-01a
  stateCol <- ifelse(featurestable$`SgPl-01` == '?' | featurestable$`MonPl-02` == '?', '?',
              ifelse(featurestable$`SgPl-01` == 'yes' & featurestable$`MonPl-02` == 'yes', 'both',
              ifelse(featurestable$`SgPl-01` == 'no' & featurestable$`MonPl-02` == 'yes', 'irregular monoexponential plural',
              ifelse((featurestable$`SgPl-01` == 'yes' & featurestable$`MonPl-02` == 'no') | (featurestable$`SgPl-01` == 'yes' & featurestable$`MonPl-02` == 'NA'), 'singular-plural alternation',
              ifelse((featurestable$`SgPl-01` == 'no' & featurestable$`MonPl-02` == 'no') | (featurestable$`SgPl-01` == 'no' & featurestable$`MonPl-02` == 'NA'), 'none', 'ERROR: Impossible state!')))))
  sourceCol <- collapseSourceColumns(list(featurestable$`SgPl-01.Source`, featurestable$`MonPl-02.Source`))
  coderCol <- lapply(paste0(featurestable$`SgPl-01.Coder`, ';', featurestable$`MonPl-02.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'SgPl-01a', stateCol, sourceCol, coderCol)
  
  
  #SgPl.01b
  stateCol <- ifelse(featurestable$`SgPl-01` == '?' & featurestable$`SgPl-19` == '?', '?',
              ifelse(featurestable$`SgPl-01` == 'yes' | featurestable$`SgPl-19` == 'yes', 'yes', 'no'))
  sourceCol <- collapseSourceColumns(list(featurestable$`SgPl-01.Source`, featurestable$`SgPl-19.Source`))
  coderCol <- lapply(paste0(featurestable$`SgPl-01.Coder`, ';', featurestable$`SgPl-19.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'SgPl-01b', stateCol, sourceCol, coderCol)
  
  
  #SgPl-07a
  stateCol <- ifelse(featurestable$`SgPl-01` == '?' | featurestable$`SgPl-07` == '?', '?',
              ifelse(featurestable$`SgPl-01` == 'no', 'NA',
              ifelse(featurestable$`SgPl-01` == 'yes' & (featurestable$`SgPl-07` == 'intransitive' | featurestable$`SgPl-07` == 'both'), 'yes',
              ifelse(featurestable$`SgPl-01` == 'yes' & featurestable$`SgPl-07` == 'transitive', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- collapseSourceColumns(list(featurestable$`SgPl-01.Source`, featurestable$`SgPl-07.Source`))
  coderCol <- featurestable$`SgPl-07.Coder`
  featurestable <- addDerivedState(featurestable, 'SgPl-07a', stateCol, sourceCol, coderCol)
  
  #SgPl-07b
  stateCol <- ifelse(featurestable$`SgPl-01` == '?' | featurestable$`SgPl-07` == '?', '?',
              ifelse(featurestable$`SgPl-01` == 'no', 'NA',
              ifelse(featurestable$`SgPl-01` == 'yes' & (featurestable$`SgPl-07` == 'transitive' | featurestable$`SgPl-07` == 'both'), 'yes',
              ifelse(featurestable$`SgPl-01` == 'yes' & featurestable$`SgPl-07` == 'intransitive', 'no', 'ERROR: Impossible state!'))))
  sourceCol <- collapseSourceColumns(list(featurestable$`SgPl-01.Source`, featurestable$`SgPl-07.Source`))
  coderCol <- featurestable$`SgPl-07.Coder`
  featurestable <- addDerivedState(featurestable, 'SgPl-07b', stateCol, sourceCol, coderCol)
  
  #SgPl-16a
  stateCol <- ifelse(featurestable$`SgPl-16` == '?', '?',
              ifelse(featurestable$`SgPl-16` == 'NA', 'NA',
              ifelse(featurestable$`SgPl-16` == 'unergative' | featurestable$`SgPl-16` == 'both', 'yes', 
              ifelse(featurestable$`SgPl-16` == 'unaccusative', 'no', 'ERROR: Imposisble state!'))))
  sourceCol <- featurestable$`SgPl-09.Source`
  coderCol <- featurestable$`SgPl-09.Coder`
  featurestable <- addDerivedState(featurestable, 'SgPl-16a', stateCol, sourceCol, coderCol)
  
  #SgPl-16b
  stateCol <- ifelse(featurestable$`SgPl-16` == '?', '?',
              ifelse(featurestable$`SgPl-16` == 'NA', 'NA',
              ifelse(featurestable$`SgPl-16` == 'unaccusative' | featurestable$`SgPl-16` == 'both', 'yes', 
              ifelse(featurestable$`SgPl-16` == 'unergative', 'no', 'ERROR: Imposisble state!'))))
  sourceCol <- featurestable$`SgPl-09.Source`
  coderCol <- featurestable$`SgPl-09.Coder`
  featurestable <- addDerivedState(featurestable, 'SgPl-16b', stateCol, sourceCol, coderCol)
  
  #SgPl-18a
  stateCol <- ifelse(featurestable$`SgPl-18` == '?', '?',
              ifelse(featurestable$`SgPl-18` == 'NA', 'NA',
              ifelse(grepl('sg-du-pl|sg-paucal-pl', featurestable$`SgPl-18`), 'yes', 
              'no')))
  sourceCol <- featurestable$`SgPl-18.Source`
  coderCol <- featurestable$`SgPl-18.Coder`
  featurestable <- addDerivedState(featurestable, 'SgPl-18a', stateCol, sourceCol, coderCol)
  
  #errors
  errorsInt <- errors
  
  errorsList <- dplyr::filter(featurestable, (`SgPl-01` == 'no' & (`SgPl-02` != 'NA' | `SgPl-04` != 'NA' | `SgPl-06` != 'NA' | `SgPl-07` != 'NA' | `SgPl-08` != 'NA' | `SgPl-10` != 'NA' | `SgPl-12` != 'NA' | `SgPl-14` != 'NA' | `SgPl-15` != 'NA' | `SgPl-17` != 'NA' | `SgPl-18` != 'NA' | `SgPl-20` != 'NA' | `SgPl-21` != 'NA')) | ((`SgPl-02` == 'NA' | `SgPl-04` == 'NA' | `SgPl-07` == 'NA' | `SgPl-08` == 'NA' | `SgPl-10` == 'NA' | `SgPl-12` == 'NA' | `SgPl-14` == 'NA' | `SgPl-15` == 'NA' | `SgPl-18` == 'NA' | `SgPl-20` == 'NA' | `SgPl-21` == 'NA') & `SgPl-01` != 'no'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SgPl', 'SgPl-01 == no <=> SgPl-02 == NA & SgPl-04 == NA & SgPl-07 == NA & SgPl-08 == NA & SgPl-10 == NA & SgPl-12 == NA & SgPl-14 == NA & SgPl-15 == NA & SgPl-18 == NA & SgPl-20 == NA & SgPl-21 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`SgPl-02` == 'no common pattern' & `SgPl-03` != 'NA') | (`SgPl-02` == 'yes' & `SgPl-03` == 'NA'))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SgPl', 'SgPl-02 == no common pattern <=> SgPl-03 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, ((`SgPl-07` == 'transitive' | `SgPl-07` == 'NA') & (`SgPl-16` != 'NA')) | ((`SgPl-07` != 'transitive' & `SgPl-07` != 'NA') & (`SgPl-16` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SgPl', 'SgPl-07 == transitive | NA <=> SgPl-16 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, ((`SgPl-07` == 'intransitive' | `SgPl-07` == 'NA') & (`SgPl-17` != 'NA')) | (`SgPl-07` != 'intransitive' & `SgPl-07` != 'NA' & (`SgPl-17` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SgPl', 'SgPl-07 == intransitive | NA <=> SgPl-17 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, (`SgPl-04` == 'no' & (`SgPl-05` != 'NA' | `SgPl-06` != 'NA' | `SgPl-09` != 'NA' | `SgPl-11` != 'NA' | `SgPl-13` != 'NA')) | (`SgPl-04` == 'yes' & (`SgPl-05` == 0 | `SgPl-05` == 'NA' | `SgPl-06` == 'NA' | `SgPl-09` == 'NA' | `SgPl-11` == 'NA' | `SgPl-13` == 'NA')))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SgPl', 'SgPl-04 == "no" <=> SgPl-05 == NA & SgPl-06 == NA & SgPl-09 == NA & SgPl-11 == NA & SgPl-13 == NA', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `SgPl-12` == 'NA' & `SgPl-13` != 'NA' & `SgPl-14` != 'NA' )
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SgPl', 'SgPl-12 == NA => SgPl-13 == "NA" & SgPl-14 == "NA"', lgnames)
  
  errorsList <- dplyr::filter(featurestable, `SgPl-12` == 'none' & `SgPl-13` != 0 & `SgPl-14` != 0)
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SgPl', 'SgPl-12 == none => SgPl-13 == 0 & SgPl-14 == 0', lgnames)
  
  errorsList <- suppressWarnings(dplyr::filter(featurestable, (as.numeric(`SgPl-05`)) != (as.numeric(`SgPl-09`)) + (as.numeric(`SgPl-11`)) + (as.numeric(`SgPl-13`)) + (as.numeric(`SgPl-14`))))
  addErrorsOrWarnings('Error', errorsList, errorsInt, 'SgPl', 'SgPl-05 != SgPl-09 + SgPl-11 + SgPl-13 + SgPl-14', lgnames)
  
  #compare verb list & coding in the spreadsheet
  for (glot in featurestable$LanguageID[featurestable$LanguageID != 'question']) {
    if (featurestable$`SgPl-01`[featurestable$LanguageID == glot] == 'yes' & length(verblist$Meaning[verblist$Glottocode == glot]) == 0 & !(featurestable$`SgPl-03`[featurestable$LanguageID == glot] == '?' & featurestable$`SgPl-05`[featurestable$LanguageID == glot] == '?')) {
      addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', 'SgPl-01 == yes, but no verbs found in verb list', lgnames)
    }
    if (featurestable$`SgPl-02`[featurestable$LanguageID == glot] == 'yes') {
      if (length(verblist$Meaning[verblist$Glottocode == glot & verblist$Alternation_Type == 'regular']) == 0) {
        addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', 'SgPl-02 == yes, but no verbs found in verb list with regular pattern', lgnames)
      }
      if (featurestable$`SgPl-03`[featurestable$LanguageID == glot] != '?') {
        ques_reg_num <- as.numeric(featurestable$`SgPl-03`[featurestable$LanguageID == glot])
        verblist_reg_num <- length(verblist$Meaning[verblist$Glottocode == glot & verblist$Alternation_Type == 'regular'])
        if (ques_reg_num != verblist_reg_num) {
          addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot,': SgPl-03 gives', ques_reg_num, 'regular alternations, but', verblist_reg_num, 'found in verb list'), lgnames)
        }
      }
    }
    if (featurestable$`SgPl-04`[featurestable$LanguageID == glot] == 'yes') {
      if (length(verblist$Meaning[verblist$Glottocode == glot & verblist$Alternation_Type == 'suppletion']) == 0 & featurestable$`SgPl-05`[featurestable$LanguageID == glot] != '?') {
        addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot,': SgPl-04 == yes, but no verbs found in verb list with suppletion'), lgnames)
      }
      if (featurestable$`SgPl-05`[featurestable$LanguageID == glot] != '?') {
        ques_supp_num <- as.numeric(featurestable$`SgPl-05`[featurestable$LanguageID == glot])
        verblist_supp_num <- length(verblist$Meaning[verblist$Glottocode == glot & verblist$Alternation_Type == 'suppletion'])
        if (ques_supp_num != verblist_supp_num) {
          addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot, ': SgPl-05 gives', ques_supp_num, 'suppletive alternations, but', verblist_supp_num, 'found in verb list'), lgnames)
        }
      }
      verblist_exp_sem <- length(verblist$Meaning[verblist$Glottocode == glot & verblist$Alternation_Type == 'suppletion' & (verblist$Semantic_Category == 'motion' | verblist$Semantic_Category == 'caused motion' | verblist$Semantic_Category == 'posture' | verblist$Semantic_Category == 'caused posture' | verblist$Semantic_Category == 'sleep' | verblist$Semantic_Category == 'cry' | verblist$Semantic_Category == 'kill' | verblist$Semantic_Category == 'die' | verblist$Semantic_Category == 'die/kill' | verblist$Meaning == 'eat')])
      if (featurestable$`SgPl-06`[featurestable$LanguageID == glot] == 'yes' & verblist_exp_sem == 0) {
        addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot, ': SgPl-06 claims expected verbs found, but none present in verb list'), lgnames)
      }
      if (featurestable$`SgPl-08`[featurestable$LanguageID == glot] == 'yes' & featurestable$`SgPl-09`[featurestable$LanguageID == glot] != '?') {
        ques_posture <- as.numeric(featurestable$`SgPl-09`[featurestable$LanguageID == glot])
        verblist_posture <- length(verblist$Meaning[verblist$Glottocode == glot & verblist$Alternation_Type == 'suppletion' & (verblist$Semantic_Category == 'posture' | verblist$Semantic_Category == 'caused posture')])
        if (ques_posture != verblist_posture) {
          addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot, ': SgPl-09 gives', ques_posture, 'suppletive alternations for posture, but', verblist_posture, 'found in verb list'), lgnames)
        }
      }
      if (featurestable$`SgPl-10`[featurestable$LanguageID == glot] == 'yes' & featurestable$`SgPl-11`[featurestable$LanguageID == glot] != '?') {
        ques_motion <- as.numeric(featurestable$`SgPl-11`[featurestable$LanguageID == glot])
        verblist_motion <- length(verblist$Meaning[verblist$Glottocode == glot & verblist$Alternation_Type == 'suppletion' & (verblist$Semantic_Category == 'motion' | verblist$Semantic_Category == 'caused motion')])
        if (ques_motion != verblist_motion) {
          addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot, ': SgPl-10 gives', ques_motion, 'suppletive alternations for motion, but', verblist_motion, 'found in verb list'), lgnames)
        }
      }
      if (featurestable$`SgPl-12`[featurestable$LanguageID == glot] != 'NA' & featurestable$`SgPl-13`[featurestable$LanguageID == glot] != '?' & featurestable$`SgPl-14`[featurestable$LanguageID == glot] != '?') {
        ques_exp <- as.numeric(featurestable$`SgPl-13`[featurestable$LanguageID == glot])
        ques_other <- as.numeric(featurestable$`SgPl-14`[featurestable$LanguageID == glot])
        verblist_exp <- length(verblist$Forms[verblist$Glottocode == glot & verblist$Alternation_Type == 'suppletion' & (verblist$Semantic_Category == 'die' | verblist$Semantic_Category == 'kill' | verblist$Semantic_Category == 'die/kill' | verblist$Semantic_Category == 'sleep' | verblist$Semantic_Category == 'cry' | verblist$Meaning == 'eat')])
        verblist_other <- length(verblist$Meaning[verblist$Glottocode == glot & verblist$Alternation_Type == 'suppletion' & verblist$Semantic_Category != 'motion' & verblist$Semantic_Category != 'caused motion' & verblist$Semantic_Category != 'posture' & verblist$Semantic_Category != 'caused posture' & verblist$Semantic_Category != 'sleep' & verblist$Semantic_Category != 'cry' & verblist$Semantic_Category != 'kill' & verblist$Semantic_Category != 'die' & verblist$Semantic_Category != 'die/kill' & verblist$Meaning != 'eat'])
        if (ques_exp != verblist_exp) {
          addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot, ': SgPl-13 gives', ques_exp, 'suppletive alternations for expected semantics, but', verblist_exp, 'found in verb list'), lgnames)
        }
        if (ques_other != verblist_other) {
          addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot, ': SgPl-14 gives', ques_other, 'suppletive alternations for other semantics, but', verblist_other, 'found in verb list'), lgnames)
        }
      }
    }
    if (featurestable$`SgPl-21`[featurestable$LanguageID == glot] == 'in property concept verbs only') { #there must be property in sg-pl verb list
      if (length(verblist$Meaning[verblist$Glottocode == glot & verblist$Semantic_Category == 'property']) == 0) {
        addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot, ': SgPl-21 says <in property concept verbs only>, but no property concepts found in verb list'), lgnames)
      }
    }
    if (featurestable$`SgPl-21`[featurestable$LanguageID == glot] == 'no') { #there must be no property in sg-pl verb list
      if (length(verblist$Meaning[verblist$Glottocode == glot & verblist$Semantic_Category == 'property']) != 0) {
        addErrorsOrWarnings('Error', dplyr::filter(featurestable, LanguageID == glot), errorsInt, 'SgPl', paste(glot, ': SgPl-21 says <no>, but property concept verbs are found in verb list'), lgnames)
      }
    }
  }
  
  #warnings
  warningsInt <- warnings
  
  warningsList <- dplyr::filter(featurestable, `SgPl-01` == 'no' & `SgPl-19` == 'yes')
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'SgPl', 'SgPl-01 == no & SgPl-19 == yes', lgnames)
  
  warningsList <- dplyr::filter(featurestable, `SgPl-08` == 'yes' & `SgPl-09` == 0)
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'SgPl', 'SgPl-08 == "yes" & SgPl-09 == 0 ==> this means all posture alternation is a regular pattern; we expect this to be rare', lgnames)
  
  warningsList <- dplyr::filter(featurestable, `SgPl-10` == 'yes' & `SgPl-11` == 0)
  addErrorsOrWarnings('Warning', warningsList, warningsInt, 'SgPl', 'SgPl-10 == "yes" & SgPl-11 == 0 ==> this means all motion alternation is a regular pattern; we expect this to be rare', lgnames)
  
  #write warnings, errors, to global variable
  warningobj <- deparse(substitute(warnings))
  assign(warningobj, value = warningsInt, envir = parent.frame())
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
  
  return(featurestable)
}
