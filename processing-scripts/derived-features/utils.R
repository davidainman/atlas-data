explodeMonPl <- function(string) {
  set <- unlist(strsplit(sub('.*\\[( *)?', '', sub('( *)?\\].*', '', string)), '( )*&( )*'))
  if ('sape' %in% set) {
    set <- set[set != 'sape'] %>% append(c('s', 'a', 'p', 'e'))
  } else if ('sap' %in% set) {
    set <- set[set != 'sap'] %>% append(c('s', 'a', 'p'))
  } else if ('spe' %in% set) {
    set <- set[set != 'spe'] %>% append(c('s', 'p', 'e'))
  } else if ('sae' %in% set) {
    set <- set[set != 'sae'] %>% append(c('s', 'a', 'e'))
  } else if ('ape' %in% set) {
    set <- set[set != 'ape'] %>% append(c('a', 'p', 'e'))
  } else if ('sa' %in% set) {
    set <- set[set != 'sa'] %>% append(c('s', 'a'))
  } else if ('sp' %in% set) {
    set <- set[set != 'sp'] %>% append(c('s', 'p'))
  } else if ('se' %in% set) {
    set <- set[set != 'se'] %>% append(c('s', 'e'))
  } else if ('ap' %in% set) {
    set <- set[set != 'ap'] %>% append(c('a', 'p'))
  } else if ('ae' %in% set) {
    set <- set[set != 'ae'] %>% append(c('a', 'e'))
  } else if ('pe' %in% set) {
    set <- set[set != 'pe'] %>% append(c('p', 'e'))
  }
  #sort nicely: 's','a','p','e' always comes first
  sape <- set[set == 's' | set == 'a' | set == 'p' | set == 'e']
  nonsape <- set[set != 's' & set != 'a' & set != 'p' & set != 'e']
  regform <- paste(c(sape, sort(nonsape)), collapse=' & ')
  return(regform)
}

compactMonPl <- function(string) {
  #we have to do this twice, because 'sape' is potentially 4 characters long
  regform <- gsub('(s|a|p) & (a|ap|ae|p|pe|e)(?![a-z])', '\\1\\2', string, perl = TRUE)
  regform <- gsub('(s|a|p) & (a|ap|ae|p|pe|e)(?![a-z])', '\\1\\2', regform, perl = TRUE)
  return(regform)
}

regularizeAmpersand <- function(string) {
  values <- c()
  for (v in sort(unlist(strsplit(string, '( )*&( )*')))) {
    values <- c(values, trimws(v))
  }
  return(paste(values, collapse = ' & '))
}

addErrorsOrWarnings <- function(isErrorOrWarning, ewList, ewDataFrame, feature, text, lgnames) {
  if (isErrorOrWarning == 'Error') {
    ewRows = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c('Feature', 'Language', 'Glottocode', 'Coder', 'ErrorText'))))
  } else if (isErrorOrWarning == 'Warning') {
    ewRows = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c('Feature', 'Language', 'Glottocode', 'Coder', 'WarningText'))))
  } else {
    stop('Invalid value passed to addErrorsOrWarnings function: Only "Warning" or "Error" accepted.')
  }
  for (glotto in ewList$Glottocode) {
    # all features until now have the same coder, we can assume feat-01-Coder is sufficient
    ewRows[nrow(ewRows) + 1, ] = c(feature, lgnames$Name[lgnames$Glottocode == glotto], glotto, ewList[[paste0(feature,'-01.Coder')]][ewList$Glottocode==glotto], text)
  }
  
  #write back to global variable
  ewDataFrameObj <- deparse(substitute(ewDataFrame))
  assign(ewDataFrameObj, value = rbindlist(list(ewDataFrame, ewRows)), envir = parent.frame())
}

compressPageNumbers <- function(pageNumbersAsString) {
  nonNumList <- c()
  numList <- c()
  for (page in unlist(strsplit(pageNumbersAsString, ','))) {
    page = trimws(gsub("–", "-", page)) #get rid of weird unicode
    if (grepl('^[0-9]+-[0-9]+$', page)) {
      first <- as.numeric(unlist(strsplit(page,'-'))[1])
      last <- as.numeric(unlist(strsplit(page,'-'))[2])
      for (x in first:last) {
        numList <- c(x, numList)
      }
    }
    else if (grepl('^[0-9]+$', page)) {
      numList <- c(as.numeric(page), numList)
    }
    else {
      nonNumList <- c(page,nonNumList)
    }
  }
  
  nonNumList <- sort(unique(nonNumList))
  numList <- unique(numList)
  numeric <- c()
  firstNum <- -1
  lastNum <- -1
  for (x in sort(numList)) {
    if (x == lastNum + 1) {
      lastNum = x
    }
    else {
      if (lastNum != -1) {
        if (lastNum == firstNum) {
          numeric <- c(numeric, as.character(lastNum))
        }
        else {
          numeric <- c(numeric, paste0(as.character(firstNum),'-',as.character(lastNum)))
        }
      }
      firstNum <- x
      lastNum <- x
    }
  }
  if (lastNum == firstNum && firstNum != -1) {
    numeric <- c(numeric, as.character(lastNum))
    }
    else {
      if (lastNum != -1) {
        numeric <- c(numeric, paste0(as.character(firstNum),'-',as.character(lastNum)))
      }
    }
  
  retval <- c()
  if (length(numeric) != 0) {
    retval <- c(retval, numeric)
  }
  if (length(nonNumList) != 0) {
    retval <- c(retval,nonNumList)
  }
  
  return(paste0(retval,collapse=','))
}

collapseSourceColumns <- function(sourceColumnsAsList) {
  sourceNumber <- length(sourceColumnsAsList)
  sourceLength <- length(sourceColumnsAsList[[1]])
  for (i in 1:sourceNumber) {
    if (length(sourceColumnsAsList[[i]]) != sourceLength) {
      print("Error in collapsing sources: Different lengths")
      return
    }
  }
  retVal <- c()
  for (i in 1:sourceLength) {
    sources <- c()
    for (j in 1:sourceNumber) {
      for (colSource in unique(unlist(strsplit(sourceColumnsAsList[[j]][i], ';')))) {
        if (!is.na(colSource)) {
          # sources <- c(sources, trimws(gsub(':.*', '', colSource, perl=TRUE)))
          sources <- c(sources, trimws(gsub('(.*)\\[[^;]+\\]','\\1', colSource, perl=TRUE)))
        }
      }
    }
    sources <- unique(sources)
    if (length(sources) == 0) {
      retVal <- c(retVal, NA)
      next
    }
    outputsources <- c()
    for (source in sources) {
      pageNumbers <- list()
      for (j in 1:sourceNumber) {
        for (colSource in unlist(strsplit(sourceColumnsAsList[[j]][i], ';'))) {
          if (grepl(source, colSource, fixed = TRUE)) {
            if (grepl('\\[', colSource)) {
              # pageNumbers <- c(pageNumbers, gsub('.*:', '', colSource, perl=TRUE))
              pageNumbers <- c(pageNumbers, trimws(gsub('.*\\[([^;]+)\\]','\\1', colSource, perl=TRUE)))
            } else {
              pageNumbers <- c(pageNumbers, '')
            }
          }
        }
      }
      pageNumbers <- unlist(unique(trimws(pageNumbers)))
      if (length(pageNumbers) == 1 & unlist(pageNumbers)[1] == '') {
        outputsources <- c(outputsources, source)
      } else {
        outputsources <- c(outputsources, paste0(source, '[', compressPageNumbers(pageNumbers), ']'))
      }
    }
    retVal <- c(retVal, paste0(sort(outputsources), collapse=';'))
  }
  return(retVal)
}

collapseCoders <- function(codersString) {
  if (is.na(codersString) | codersString == 'NA') {
    return('')
  }
  return(paste0(sort(unique(trimws(unlist(strsplit(codersString, ';'))))), collapse=';'))
}

collapseSources <- function(sourcesAsList) {
  sources <- c()
  retval <- c()
  for (source in unlist(strsplit(sourcesAsList, ';'))) {
    sources <- c(sources, trimws(gsub('(.*)\\[[^;]+\\]','\\1', source, perl=TRUE)))
  }
  sources <- unique(sources)
  sources <- sources[!is.na(sources)]
  for (source in sources) {
    if (is.na(source)) {
      next
    }
    pageNumbers <- list()
    for (s in unlist(strsplit(sourcesAsList, ';'))) {
      s <- trimws(s)
      if (grepl(source, s, fixed = TRUE)) {
        if (grepl('\\[', s)) {
          pageNumbers <- c(pageNumbers, trimws(gsub('.*\\[([^;]+)\\]','\\1', s, perl=TRUE)))
        } else {
          pageNumbers <- c(pageNumbers, '')
        }
      }
    }
    pageNumbers <- unlist(unique(pageNumbers))
    if (length(pageNumbers) == 1 & unlist(pageNumbers)[1] == '') {
      retval <- c(retval, source)
    } else {
      retval <- c(retval, paste0(source, '[', compressPageNumbers(pageNumbers), ']'))
    }
  }
  if (is.null(retval)) {
    return (c(NA))
  } else { return(sort(retval)) }
}

addDerivedState <- function(featurestable, feature, stateColumn, sourceColumn, coderColumn) {
  featurestable[[paste0(feature)]] <- stateColumn
  featurestable[[paste0(feature, '.Frequency')]] <- rep('1', nrow(featurestable))
  featurestable[[paste0(feature, '.Remark')]] <- 'derived'
  featurestable[[paste0(feature, '.Source')]] <- sourceColumn
  featurestable[[paste0(feature, '.Coder')]] <- coderColumn
  
  return(featurestable)
}

convertToWide <- function(valuescsv) {
  wider_data <- valuescsv %>% 
    dplyr::select(c('LanguageID', 'ParameterID', 'Value', 'Frequency', 'Remark')) %>% 
    group_by(LanguageID, ParameterID, Frequency, Remark) %>% 
    pivot_wider(names_from = ParameterID,
                values_from = c(Value, Frequency, Remark), 
                names_glue = "{ParameterID}.{.value}", names_vary = "slowest",
                values_fn = function(x) paste(x, collapse=";;")) %>%
    rename_at(vars(contains(".Value")), list(~ sub('.Value','',.)))
  wider_metadata <- valuescsv %>% 
    dplyr::select(c('LanguageID', 'ParameterID', 'Value', 'Source','Coder')) %>% 
    group_by(LanguageID, ParameterID, Source, Coder) %>% 
    distinct(Value) %>%
    pivot_wider(names_from = ParameterID,
                values_from = c(Value, Source, Coder), 
                names_glue = "{ParameterID}.{.value}", names_vary = "slowest",
                values_fn = function(x) paste(unique(x), collapse=";;")) %>%
    select(-contains("Value"))
  wider <- merge(wider_data, wider_metadata, by="LanguageID")
  wider <- wider %>% select(order(colnames(wider))) %>% select(LanguageID, everything())
  return(wider)
}

convertToLong <- function(wider, parameterscsv, codescsv) {
  wider_int <- wider %>% select(-c("Name")) %>% rename(LanguageID = Glottocode)
  colnames(wider_int) <- lapply(colnames(wider_int), function(x) {ifelse(grepl("\\.|LanguageID",x), x, paste0(x,".Value"))})
  longer <- pivot_longer(wider_int, cols=-c('LanguageID'), names_pattern='(.*-.*)\\.(.*)', names_to=c('ParameterID', '.value')) %>%
    select(c("LanguageID", "ParameterID", "Value", "Frequency", "Remark", "Source", "Coder"))
  # expand lists
  list_indices <- which(grepl(';;',longer$Value))
  for (l in list_indices) {
    for (sep in seq(1, str_count(longer[[l, "Value"]], ";;") + 1)) {
      longer <- rbind(longer, c(longer[[l,"LanguageID"]],
                                longer[[l,"ParameterID"]],
                                unlist(strsplit(longer[[l,"Value"]], ";;"))[sep],
                                unlist(strsplit(longer[[l,"Frequency"]], ";;"))[sep],
                                ifelse(grepl("^.*;;.*$", longer[[l,"Remark"]]),
                                       unlist(strsplit(longer[[l,"Remark"]], ";;"))[sep],
                                       longer[[l,"Remark"]]),
                                longer[[l,"Source"]],
                                longer[[l,"Coder"]]))
    }
  }
  longer <- longer[-c(list_indices),]
  
  # convert frequencies for
  list_indices <- which(grepl(':',longer$Value))
  for (l in list_indices) {
    val <- unlist(strsplit(longer[[l, 'Value']], ':'))[1]
    freq <- unlist(strsplit(longer[[l, 'Value']], ':'))[2]
    longer[[l, 'Value']] <- val
    longer[[l, 'Frequency']] <- freq
  }
  
  # add frequencies to non-frequency questions
  nonfrequencies <- parameterscsv$ParameterID[parameterscsv$datatype != "frequency"]
  list_indices <- which(is.na(longer$Frequency) & longer$ParameterID %in% nonfrequencies)
  for (l in list_indices) {
    longer[[l, 'Frequency']] <- "1"
  }
  
  # add CodeID
  longer <- merge(longer, codescsv %>% select(CodeID, Description, ParameterID), by.x=c("ParameterID", "Value"), by.y=c("ParameterID", "Description"), all.x = TRUE)
  
  # add ID
  longer <- longer[order(longer$LanguageID, longer$ParameterID, longer$Value),]
  longer$ID <- paste0(longer$LanguageID,"_",longer$ParameterID)
  duplicateIDs <- which(duplicated(longer$ID)|duplicated(longer$ID, fromLast = TRUE))
  lastID <- ''
  cnt <- 1
  for (row in duplicateIDs) {
    thisID <- longer[[row, 'ID']]
    if (thisID != lastID) {
      lastID <- thisID
      cnt <- 1
    }
    longer[[row,'ID']] <- paste0(longer[[row,'ID']],'_',cnt)
    cnt <- cnt + 1
  }
  
  # convert NAs to ""
  longer[is.na(longer)] <- ""
  
  return(longer %>% select(c("ID", "LanguageID", "ParameterID", "CodeID", "Value", "Frequency", "Remark", "Source", "Coder")))
}