#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(rje))
options(warn=-1)

source('utils.R')
source('nounposs.R')
source('lat.R')
source('veluv.R')
source('glot.R')
source('cor.R')
source('toneprom.R')
source('syll.R')
source('ppron.R')
source('sgpl.R')
source('monpl.R')
source('wo.R')
source('attr.R')
source('gend.R')
source('soccaus.R')
source('dem.R')
source('appr.R')
source('nounposs.R')
source('align.R')


# output errors and warnings, preserving comments
writeErrorsandWarnings <- function(warnings, errors, outputloc) {
  if (nrow(errors) > 0) {
    oldErrors <- read.csv(paste0(outputloc, '/main-errors.csv'), na.strings = c(), colClasses = c("character"), stringsAsFactors = FALSE)
    newErrors <- right_join(oldErrors %>% select(-c("Coder")), errors, by = c('Feature', 'Language', 'Glottocode', 'ErrorText'))
    newErrors <- newErrors[, names(oldErrors)]
  } else {
    newErrors <- errors
  }
  
  oldWarnings <- read.csv(paste0(outputloc, '/main-warnings.csv'), na.strings = c(), colClasses = c("character"), stringsAsFactors = FALSE)
  newWarnings <- right_join(oldWarnings %>% select(-c("Coder")), warnings, by = c('Feature', 'Language', 'Glottocode', 'WarningText'))
  newWarnings <- newWarnings[, names(oldWarnings)]
  
  write.csv(newErrors, paste0(outputloc, '/main-errors.csv'), na="", row.names=FALSE)
  write.csv(newWarnings, paste0(outputloc, '/main-warnings.csv'), na="", row.names=FALSE)
}


# check that values in values.csv are present in codes.csv
validateValues <- function(features, codescsv, errors) {
  errorrows = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c('Feature', 'Language', 'Glottocode', 'Coder', 'ErrorText'))))
  for (sn in unique(codescsv$ParameterID)) {
    validValues <- unique(subset(codescsv, ParameterID == sn)$Description) %>% append('?') %>% append(NA)
    if ('_number_' %in% validValues) {
      invalids <- subset(features, !(features[[sn]] %in% validValues) & !grepl('[[:digit:]]', features[[sn]]) & features[[sn]] != '?' & !is.na(features[[sn]]) & features$glottocode != 'question')
    } else {
      invalids <- subset(features, !(features[[sn]] %in% validValues) & features[[sn]] != '?' & !is.na(features[[sn]]) & features$glottocode != 'question')
    }
    # split apart by ';' and regularize '&' sets, if they are present
    if (nrow(invalids) != 0) {
      fakeinvalids <- c()
      for (i in 1:nrow(invalids)) {
        invalid <- invalids[i,][[sn]]
        if (grepl(';', invalid)) {
          trueinvalids <- c()
          for (value in unlist(strsplit(invalid, ';'))) {
            value <- trimws(value)
            if (sn == 'MonPl-06') {
              value <- compactMonPl(explodeMonPl(value))
            } else if (grepl('&', value)) {
              value <- regularizeAmpersand(value)
            }
            if (!(value %in% validValues)) {
              trueinvalids <- c(trueinvalids, invalid)
            }
          }
          if (length(trueinvalids) != 0) {
            for (trueinvalid in trueinvalids) {
              trueinvalidrow <- invalids[i,]
              trueinvalidrow[[sn]] <- paste('List element:', trueinvalid)
              invalids <- rbind(invalids, trueinvalidrow)
            }
          }
          fakeinvalids <- c(fakeinvalids, i)
        } else if (grepl('&', invalid)) { # & and no ;
          if (sn == 'MonPl-06') {
            regularized <- compactMonPl(explodeMonPl(invalid))
          } else {
            regularized <- regularizeAmpersand(invalid)
          }
          if (regularized %in% validValues) {
            fakeinvalids <- c(fakeinvalids, i)
          }
        }
      }
      if (length(fakeinvalids) > 0) {
        invalids <- invalids[-c(fakeinvalids),]
      }
    }
    for (name in invalids$name) {
      errorrows[nrow(errorrows) +1, ] = c(sn, name, invalids$glottocode[invalids$name == name], invalids[[paste0(gsub('\\..*', '', sn), '.coder')]][invalids$name == name], paste0('Unexpected value: ', invalids[[sn]][invalids$name == name]))
    }
  }
  
  #write warnings, errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = rbindlist(list(errors, errorrows)), envir = parent.frame())
}


# check for sources across all features
globalPreprocess <- function(featurestable, warnings, errors) {
  questions <- colnames(featurestable)[grepl("[A-z]+\\-\\d+[A-z]*$",colnames(featurestable))]
  
  #errors
  errorsInt <- errors
  errorrows = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c('Feature', 'Language', 'Glottocode', 'Coder', 'ErrorText'))))
  
  #check for the existence of a source
  for (question in questions) {
    source = paste0(question, '-Source')
    errorsList <- dplyr::filter(featurestable, is.na(source))
    addErrorsOrWarnings('Error', errorsList, errorsInt, gsub('\\..*', '', question), paste0('Missing value for source ',  question))
  }
  
  #write warnings, errors, to global variable
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = errorsInt, envir = parent.frame())
}


main <- function(maincldfloc, alignmentcldfloc, possessioncldfloc, sgplloc, noncldfloc) {
  #initialize warnings & errors tables
  warnings <- data.table(matrix(ncol=5,nrow=0, dimnames=list(NULL, c('Feature', 'Language', 'Glottocode', 'Coder', 'WarningText'))))
  errors   <- data.table(matrix(ncol=5,nrow=0, dimnames=list(NULL, c('Feature', 'Language', 'Glottocode', 'Coder', 'ErrorText'))))
  
  # read csv files
  # main database
  valuescsv <- read.csv(paste0(maincldfloc, "/values.csv"), na.strings = c(), colClasses = c("character"), stringsAsFactors = FALSE)
  languagescsv <- read.csv(paste0(maincldfloc, "/languages.csv"), na.strings = c(), stringsAsFactors = FALSE)
  parameterscsv <- read.csv(paste0(maincldfloc, "/parameters.csv"), na.strings = c(), stringsAsFactors = FALSE)
  codescsv <- read.csv(paste0(maincldfloc, "/codes.csv"), na.strings = c(), stringsAsFactors = FALSE)
  
  # sg-pl verbs
  sgplverbs <- read.csv(paste0(sgplloc, "/verbs.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  
  # noun possession data
  nounposs_classescsv <- read.csv(paste0(possessioncldfloc, "/classes.csv"), na.strings = c(), stringsAsFactors = FALSE)
  nounposs_constructionscsv <- read.csv(paste0(possessioncldfloc, "/constructions.csv"), na.strings = c(), stringsAsFactors = FALSE)
  
  # alignment data
  align_contextscsv   <- read.csv(paste0(alignmentcldfloc, '/generic-cldf/contexts.csv'))
  align_selectorscsv  <- read.csv(paste0(alignmentcldfloc, '/generic-cldf/selectors.csv'))
  align_languagescsv  <- read.csv(paste0(alignmentcldfloc, '/generic-cldf/languages.csv'))
  align_referencescsv <- read.csv(paste0(alignmentcldfloc, '/generic-cldf/references.csv'))
  align_domscsv <- read.csv(paste0(alignmentcldfloc, '/generic-cldf/doms.csv'))
  align_valuescsv <- read.csv(paste0(alignmentcldfloc, '/structure-cldf/values.csv'))
  
  lgnames <- dplyr::select(languagescsv, c('Glottocode','Name'))
  
  # drop all derived questions
  wide_data <- convertToWide(valuescsv)
  # add lg names
  wide_data <- merge(languagescsv[,c("Glottocode", "Name")], wide_data, by.x="Glottocode", by.y="LanguageID")
  
  # remove derived questions (+ align, nounposs)
  derived_qs <- colnames(wide_data)[grepl('[[:digit:]]+[[:alpha:]]',colnames(wide_data))]
  align_qs <- colnames(wide_data)[grepl('Align',colnames(wide_data))]
  poss_qs <- colnames(wide_data)[grepl('NounPoss',colnames(wide_data))]
  wide_no_derived <- dplyr::select(wide_data, -one_of(derived_qs, align_qs, poss_qs))
  
  # generate NounPoss questions
  wide_no_derived <- processNounPoss(wide_no_derived, nounposs_classescsv, nounposs_constructionscsv, possessioncldfloc, lgnames, warnings, errors)
  
  # generate Align questions
  wide_no_derived <- processAlignment(wide_no_derived, align_languagescsv, align_contextscsv, align_selectorscsv, align_referencescsv, align_domscsv, align_valuescsv)
  
  # generate other derived questions
  wide_no_derived <- processLaterals(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processVeluv(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processGlottalized(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processCoronals(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processToneProm(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processSyllable(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processPronouns(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processSgPl(wide_no_derived, sgplverbs, lgnames, warnings, errors)
  wide_no_derived <- processMonPl(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processWordOrder(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processGenderlects(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processSocCaus(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processDemonstratives(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processAttributives(wide_no_derived, lgnames, warnings, errors)
  wide_no_derived <- processApprehensional(wide_no_derived, lgnames, warnings, errors)
  
  # make everything a character
  wide_no_derived <- wide_no_derived %>% mutate(across(everything(), as.character))
  
  # process for global errors
  globalPreprocess(wide_no_derived, warnings, errors)
  validateValues(wide_no_derived, codescsv, errors)
  
  # TODO left off here
  # changed 
  
  long_data <- convertToLong(wide_no_derived, parameterscsv, codescsv)
  long_data <- long_data[order(long_data$ID),]
  valuescsv <- valuescsv[order(valuescsv$ID),]
  #long_data_coders <- dplyr::filter(long_data, (long_data$Coder != valuescsv$Coder))$Coder
  #valuescsv_coders <- dplyr::filter(valuescsv, (long_data$Coder != valuescsv$Coder))$Coder
  
  if (all.equal(valuescsv, long_data, check.attributes = FALSE)) {
    print("Confirming that nothing changed in the data.")
  }
  print(paste0("Writing new values.csv to ", maincldfloc, "/values.csv"))
  write.csv(long_data, paste0(maincldfloc, "/values.csv"), na="", row.names=FALSE)
  
  print(paste0("Writing errors and warnings to ", noncldfloc))
  writeErrorsandWarnings(warnings, errors, noncldfloc)
}


if (!interactive()) {
  parser <- ArgumentParser()
  parser$add_argument("--main", type="character", help="Path to the main atlas cldf folder. Default: ../../cldf-atlas/", default="../../cldf-atlas/")
  parser$add_argument("--alignment", type="character", help="Path to the alignment cldfs parent folder. Default: ../../cldf-alignment/", default="../../cldf-alignment/")
  parser$add_argument("--possession", type="character", help="Path to the possession cldf folder. Default: ../../cldf-possession/", default="../../cldf-possession/")
  parser$add_argument("--sgpl", type="character", help="Path to the sg-pl verbs cldf folder. Default: ../../cldf-sgpl-verbs/", default="../../cldf-sgpl-verbs/")
  parser$add_argument("--output", type="character", help="Path where the non-CLDF files should be written (errors, etc). Default: ../../outputs/", default="../../outputs/")
  args <- parser$parse_args()
  main(args$main, args$alignment, args$possession, args$sgpl, args$output)
  
}
