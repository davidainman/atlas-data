source('utils.R')

CONCEPTUALLY_INALIENABLE = c('tools', 'body', 'body_internal', 'owner', 'clothing', 'nuclear_kin', 'plant_part', 'blood_kin', 'kin', 'part', 'relation', 'place_relation', 'intimate_property', 'blood_kin_below_ego', 'blood_kin_above_ego', 'non_blood_kin', 'food')
CONCEPTUALLY_NONPOSSESSIBLE = c('humans', 'plants', 'wild_animals', 'animals', 'nature_inanimate', 'mass_noun', 'celestial', 'names')

processNounPoss <- function(featurestable, classes, constructions, possessioncldfloc, lgnames, warnings, errors) {
  errorRows = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c('Feature', 'Language', 'Glottocode', 'Coder', 'ErrorText'))))
  warningRows = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c('Feature', 'Language', 'Glottocode', 'Coder', 'WarningText'))))
  
  #find errors and warnings
  glottocodes <- unique(classes$Glottocode)
  for (glotto in glottocodes) {
    classInClasses <- dplyr::filter(classes, as.character(Glottocode) == glotto, as.character(Coder) != '') %>% select(ID)
    classInCxns <- dplyr::filter(constructions, as.character(Glottocode) == glotto) %>% select(Class_ID)
    name <- lgnames$Name[lgnames$Glottocode == glotto]
    coder <- paste(unique(trimws(unlist(strsplit(dplyr::filter(constructions, as.character(Glottocode) == glotto)$Coder, ';')))), collapse=';')
    cxnClasses = c()
    for (cxnClass in classInCxns$Class_ID) {
      cxnClasses = c(cxnClasses, strsplit(cxnClass, "( )*;( )*"))
    }
    cxnClasses = unique(unlist(cxnClasses))
    for (class in classInClasses$ID) {
      if (!class %in% cxnClasses) {
        errorRows[nrow(errorRows)+1,] = c('NounPoss', name, glotto, coder, paste0("No construction found for class ", class, "."))
      } else if (!class %in% classInCxns$Class_ID && class != paste0(glotto,'_alienable')) {
        warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, paste0("Unique construction for ", class, " not found in constructions."))
      }
      semantics <- unlist(strsplit(classes$Semantic_Categories[classes$Glottocode == glotto & classes$ID == class], "( )*;( )*"))
      for (s in semantics) {
        if (!s %in% CONCEPTUALLY_INALIENABLE && !s %in% CONCEPTUALLY_NONPOSSESSIBLE && s != "default" && s != "mixed") {
          errorRows[nrow(errorRows)+1,] = c('NounPoss', name, glotto, coder, paste0("Unknown semantic type detected for class ", class, ": ", s, "."))
        }
      }
    }
    Indirects <- dplyr::filter(constructions, as.character(Glottocode) == glotto, as.character(Coder) != '', as.character(Construction_Type) == 'CLASS' | as.character(Construction_Type) == 'CLAUSE' | as.character(Construction_Type) == 'PRO_CLASS' | as.character(Construction_Type) == 'PRO_CLAUSE')
    NonNullNonPossessions <- dplyr::filter(constructions, as.character(Glottocode) == glotto, as.character(Coder) != '', as.character(Construction_Type) == 'UNPOSSESSION', as.character(Construction_Form) != 'NULL', as.character(Construction_Form) != 'N')
    for (class in cxnClasses) {
      hasIndirect <- FALSE
      hasNonNullNonPossession <- FALSE
      for (IndCxnClass in Indirects$Class_ID) {
        if (class %in% unlist(strsplit(IndCxnClass, "( )*;( )*"))) {
          hasIndirect <- TRUE
        }
      }
      for (NonPossClass in NonNullNonPossessions$Class_ID) {
        if (class %in% unlist(strsplit(NonPossClass, "( )*;( )*"))) {
          hasNonNullNonPossession <- TRUE
        }
      }
      if (hasIndirect & hasNonNullNonPossession) {
        warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, paste("The class",class,"has both an INDIRECT possession construction and a UNPOSSESSION construction."))
      }
    }
    cxns <- dplyr::filter(constructions, as.character(Glottocode) == glotto, as.character(Coder) != '', as.character(Construction_Type) != 'UNPOSSESSION', as.character(Construction_Type) != 'POSSESSION', !grepl('SUPPLETIVE', as.character(Class_ID)))
    if (nrow(cxns) > 0) {
      for (i in 1:nrow(cxns)) {
        hasPSSR = grepl('PSSR', cxns[i,"Construction_Form"])
        hasPSSD = grepl('PSSD', cxns[i,"Construction_Form"])
        isPronominal = grepl('PRO', cxns[i,"Construction_Type"])
        if (!hasPSSR && !hasPSSD) {
          errorRows[nrow(errorRows)+1,] = c('NounPoss', name, glotto, coder, paste("Strings 'PSSR' and 'PSSD' not found in possession construction for",name,cxns[i,"Class_ID"]))
        } else if (!hasPSSR && !isPronominal) {
          errorRows[nrow(errorRows)+1,] = c('NounPoss', name, glotto, coder, paste("String 'PSSR' not found in possession construction for",name,cxns[i,"Class_ID"]))
        } else if (!hasPSSD) {
          errorRows[nrow(errorRows)+1,] = c('NounPoss', name, glotto, coder, paste("String 'PSSD' not found in possession construction for",name,cxns[i,"Class_ID"]))
        }
      }
    }
    uncxns <- dplyr::filter(constructions, as.character(Glottocode) == glotto, as.character(Coder) != '', as.character(Construction_Type) == 'UNPOSSESSION')
    if (nrow(uncxns > 0)) {
      for (i in 1:nrow(uncxns)) {
        hasPSSR = grepl('PSSR', uncxns[i,"Construction_Form"])
        hasPSSD = grepl('PSSD', uncxns[i,"Construction_Form"])
        if (hasPSSR || hasPSSD) {
          errorRows[nrow(errorRows)+1,] = c('NounPoss', name, glotto, coder, paste("Strings 'PSSR' and 'PSSD' cannot be present in unpossession construction:",name,cxns[i,"Class_ID"]))
        }
      }
    }
    # check for matching of default class order and order in WO
    defaultclass <- classes$ID[classes$Glottocode == glotto & grepl('default', classes$Semantic_Categories)]
    glottoconstructions <- dplyr::filter(constructions, Glottocode == glotto)
    defaultshapes <- unique(glottoconstructions$Construction_Form[glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',defaultclass,'($|\\s|;)'), glottoconstructions$Class_ID)])
    if (!(is.na(featurestable$`WO-03`[featurestable$Glottocode == glotto]))) {
      wo_order <- featurestable$`WO-03`[featurestable$Glottocode == glotto]
      np_orders <- c()
      for (shape in defaultshapes) {
        if (grepl("PSSR", shape) & grepl("PSSD", shape)) {
          PSSR_loc <- unlist(gregexpr("PSSR", shape))[1]
          PSSD_loc <- unlist(gregexpr("PSSD", shape))[1]
          if (PSSR_loc < PSSD_loc) {
            np_orders <- c(np_orders, "PSSR-PSSD")
          } else {
            np_orders <- c(np_orders, "PSSD-PSSR")
          }
        }
      }
      np_orders <- unique(np_orders)
      if (wo_order == "PSSR-PSSD") {
        if (!("PSSR-PSSD" %in% np_orders)) {
          if (length(np_orders) == 0) {
            warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, "WO-03 claims the order is PSSR-PSSD, but no order found for default class in NounPoss.")
          } else {
            warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, paste("WO-03 claims the order is PSSR-PSSD, but the orders found in NounPoss are",paste(np_orders, collapse=",")))
          }
        }
      }
      if (wo_order == "PSSD-PSSR") {
        if (!("PSSD-PSSR" %in% np_orders)) {
          if (length(np_orders) == 0) {
            warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, "WO-03 claims the order is PSSD-PSSR, but no order found for default class in NounPoss.")
          } else {
            warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, paste("WO-03 claims the order is PSSD-PSSR, but the only orders found in NounPoss are",paste(np_orders, collapse=",")))
          }
        }
      }
      if (wo_order == "no dominant order") {
        if (!("PSSD-PSSR" %in% np_orders) & !("PSSR-PSSD" %in% np_orders)) {
          if (length(np_orders) == 0) {
            warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, "WO-03 claims no dominant order for possession, but no order found for default class in NounPoss.")
          } else {
            warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, paste("WO-03 claims no dominant order for possession, but the only orders found in NounPoss are",paste(np_orders, collapse=",")))
          }
        }
      }
      if (wo_order == "?" | wo_order == "NA") {
        if (length(np_orders) > 0) {
          warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, coder, paste("WO-03 claims no information for possession order, but NounPoss contains the orders",paste(np_orders, collapse=",")))
        }
      }
    }
  }
  
  errorsList <- dplyr::filter(constructions, Construction_Type == 'NON_POSSESSION' & grepl('PSSR', Construction_Form, perl=TRUE))
  addErrorsOrWarnings('Error', errorsList, errorRows, 'NounPoss', "NON_POSSESSION construction should not have PSSR in its Construction_Form", lgnames)
  
  #check for partially coded languages
  codedClasses = unique(dplyr::filter(classes, Coder != '')$Glottocode)
  codedConstructions = unique(dplyr::filter(constructions, Coder != '')$Glottocode)
  fully_coded     = intersect(codedClasses, codedConstructions)
  partially_coded = setdiff(codedClasses, codedConstructions)
  for (glotto in partially_coded) {
    name <- lgnames$Name[lgnames$Glottocode == glotto]
    warningRows[nrow(warningRows)+1,] = c('NounPoss', name, glotto, '?', 'Language is only coded in Noun Classes or Constructions, but not both.')
  }
  
  #write warnings, errors, to global variable
  warningobj <- deparse(substitute(warnings))
  assign(warningobj, value = rbindlist(list(warnings, warningRows)), envir = parent.frame())
  errorobj <- deparse(substitute(errors))
  assign(errorobj, value = rbindlist(list(errors, errorRows)), envir = parent.frame())
  
  #Generate different levels of construction generalizations
  constructions$dd_Construction_Type_Simplified <- ifelse(constructions$Construction_Type == 'POSSESSION' & constructions$Construction_Form == 'NULL', 'NULL',
                                  ifelse(constructions$Construction_Type == 'PRO_MARKER', 'MARKER',
                                  ifelse(constructions$Construction_Type == 'PRO_CLASS', 'CLASS',
                                  ifelse(constructions$Construction_Type == 'UNPOSSESSION', NA,
                                  ifelse(grepl('(?<!:)PSSD', constructions$Construction_Type, perl=TRUE) & grepl('(?<!:)PSSR', constructions$Construction_Type, perl=TRUE), 'PSSR-PSSD',
                                  ifelse(grepl('(?<!:)PSSD', constructions$Construction_Type, perl=TRUE), 'PSSD',
                                  ifelse(grepl('(?<!:)PSSR', constructions$Construction_Type, perl=TRUE), 'PSSR', constructions$Construction_Type)))))))
  
  constructions$dd_Construction_Type_Generic <- ifelse(grepl('PSSD', constructions$Construction_Type) | grepl('PSSR', constructions$Construction_Type) | grepl('PRO_MARKER', constructions$Construction_Type) | grepl('LINKER', constructions$Construction_Type), 'MARKER', 
        ifelse(grepl('JUXT', constructions$Construction_Type), 'JUXT', 
        ifelse(grepl('CLASS', constructions$Construction_Type), 'CLASS', 
        ifelse(grepl('CLAUSE', constructions$Construction_Type), 'CLAUSE',
        ifelse(grepl('^POSSESSION$', constructions$Construction_Type) & grepl('NULL', constructions$Construction_Form), 'NULL', NA)))))
  
  constructions$dd_Construction_Directness <- ifelse(grepl('MARKER', constructions$dd_Construction_Type_Generic) | grepl('JUXT',   constructions$dd_Construction_Type_Generic), 'DIRECT', 
        ifelse(grepl('CLASS',  constructions$dd_Construction_Type_Generic) | grepl('CLAUSE', constructions$dd_Construction_Type_Generic), 'INDIRECT', 
        ifelse(grepl('NULL', constructions$dd_Construction_Type_Generic), 'NULL', NA)))
  
  #Fill in deduced columns
  for (glotto in glottocodes) {
    possessionClasses <- dplyr::filter(classes, as.character(Glottocode) == glotto, as.character(Coder) != '')$ID
    defaultclass <- classes$ID[classes$Glottocode == glotto & grepl('default', classes$Semantic_Categories)]
    for (class in possessionClasses) {
      classConstructions <- dplyr::filter(constructions, as.character(Glottocode) == glotto, 
                                          grepl(gsub(" ", "", paste("(^|[[:space:]]|;)", paste(class,"([[:space:]]|;|$)"))), Class_ID))
      bareNounPossible <- TRUE
      if (nrow(dplyr::filter(classConstructions, Construction_Type == 'UNPOSSESSION' & Construction_Form == 'N')) == 0 & nrow(dplyr::filter(classConstructions, Construction_Type == 'UNPOSSESSION' & Construction_Form != 'N')) > 0) {
        bareNounPossible <- FALSE
      }
      classes$dd_Valency[classes$Glottocode == glotto & classes$ID == class] <-
        ifelse(class == 'SUPPLETIVE', NA,
        ifelse(!('DIRECT' %in% classConstructions$dd_Construction_Directness) & 
                 ('INDIRECT' %in% classConstructions$dd_Construction_Directness | 
                    nrow(dplyr::filter(classConstructions, Construction_Type == 'POSSESSION' & 
                                  Construction_Form == 'NULL')) > 0), 'Non-possessible',
        ifelse(!bareNounPossible & ('DIRECT' %in% classConstructions$dd_Construction_Directness), 'Obligatorily possessed', 
        ifelse(('DIRECT' %in% classConstructions$dd_Construction_Directness & bareNounPossible) | (!bareNounPossible & 'INDIRECT' %in% classConstructions$dd_Construction_Directness & 'DIRECT' %in% classConstructions$dd_Construction_Directness), 'Optionally possessed', 'FAULTY LOGIC'))))
      
      semantics <- trimws(unlist(strsplit(classes$Semantic_Categories[ which(classes$Glottocode == glotto & classes$ID == class)], ';')))
      semantictype <- 'UNKNOWN'
      for (semval in semantics) {
        if (semval %in% CONCEPTUALLY_INALIENABLE & semantictype == 'UNKNOWN') {
          semantictype <- 'Inalienable'
        } else if (semval %in% CONCEPTUALLY_NONPOSSESSIBLE & semantictype == 'UNKNOWN') {
          semantictype <- 'Non-possessible'
        } else if(semval == 'mixed') {
          semantictype <- 'Mixed'
        } else if ((semval %in% CONCEPTUALLY_INALIENABLE & semantictype == 'Non-possessible') | (semval %in% CONCEPTUALLY_NONPOSSESSIBLE & semantictype == 'Inalienable')) {
          semantictype <- 'Both'
        }
      }
      
      classes$dd_Semantic_Type[classes$Glottocode == glotto & classes$ID == class] <- ifelse(class == defaultclass, 'Default', semantictype)
      
      coarseconstructiontypes <- c()
      if(nrow(classConstructions) > 0) {
        for(i in 1:nrow(classConstructions)) {
          if (!is.na(classConstructions[i,]$dd_Construction_Type_Generic)) {
            coarseconstructiontypes <- c(coarseconstructiontypes, classConstructions[i,]$dd_Construction_Type_Generic)
          }
        }
      }
      coarseconstructiontypes <- paste(sort(unique(coarseconstructiontypes)), collapse=';')
      classes$dd_Construction_Type_Generic[classes$Glottocode == glotto & classes$ID == class] <- coarseconstructiontypes
      
      mediumconstructiontypes <- c()
      if(nrow(classConstructions) > 0) {
        for (i in 1:nrow(classConstructions)) {
          if (!is.na(classConstructions[i,]$dd_Construction_Type_Simplified)) {
            mediumconstructiontypes <- c(mediumconstructiontypes, classConstructions[i,]$dd_Construction_Type_Simplified)
          }
        }
      }
      mediumconstructiontypes <- paste(sort(unique(mediumconstructiontypes)), collapse=';')
      classes$dd_Construction_Type_Simplified[classes$Glottocode == glotto & classes$ID == class] <- mediumconstructiontypes
      
      classes$dd_Unpossession_Construction[classes$Glottocode == glotto & classes$ID == class] <- 
        ifelse(nrow(dplyr::filter(classConstructions, Construction_Type == 'UNPOSSESSION', Construction_Form == 'NULL')) > 0, 'NULL',
        ifelse(nrow(dplyr::filter(classConstructions, Construction_Type == 'UNPOSSESSION', Construction_Form == 'N')), 'Noun',
        ifelse(nrow(dplyr::filter(classConstructions, Construction_Type == 'UNPOSSESSION', Construction_Form == '?')), '?',
        ifelse(nrow(dplyr::filter(classConstructions, Construction_Type == 'UNPOSSESSION', Construction_Form != 'NULL')) > 0, 'MARKER', 'Noun'))))
    }
  }
  
  classes$Unpossession_Marker_Type <-
    ifelse(classes$dd_Unpossession_Construction == 'Noun' | classes$dd_Unpossession_Construction == 'NULL', 'NA', classes$Unpossession_Marker_Type)
  
  # write out changes to classes, constructions
  write.csv(classes, paste0(possessioncldfloc, '/classes.csv'), na="", row.names=FALSE)
  write.csv(constructions, paste0(possessioncldfloc, '/constructions.csv'), na="", row.names=FALSE)
  
  #prepopulate features -- in a meaningless if statement so I can collapse it while working
  if(TRUE) {
    na_array <- rep(NA, nrow(featurestable))
    featurestable <- addDerivedState(featurestable, 'NounPoss-01',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-01a', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-02',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-03',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-03a', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-03b', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-03c', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-03d', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-04',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-04a', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-04b', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-05',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-06',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-07',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-08',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-08a', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-08b', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-08c', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-09',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-10',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-11',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-12',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-12a', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-13',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-14',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-15',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-16',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-16a', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-17',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-17a', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-21',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-22',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-23',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-24',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-18',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-19',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-20',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-20a', na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-25',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-26',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-27',  na_array, na_array, na_array)
    featurestable <- addDerivedState(featurestable, 'NounPoss-28',  na_array, na_array, na_array)
  }
  
  # first deal with suppletive pairs by answering its own question, then delete them from the (internal) table
  # suppletion is a "fake category", we don't want to count it as a construction in construction types.
  for (glotto in glottocodes) {
    constructionscoder <- unique(trimws(unlist(strsplit(constructions$Coder[constructions$Glottocode == glotto], ';'))))
    classescoder <- unique(trimws(unlist(strsplit(classes$Coder[classes$Glottocode == glotto], ';'))))
    featurestable$`NounPoss-20.Coder`[featurestable$Glottocode == glotto] <- paste(unique(c(constructionscoder, classescoder)), collapse=';')
    
    hasSuppletion <- nrow(filter(classes, Glottocode == glotto & Noun_Class == 'SUPPLETIVE')) > 0
    
    featurestable$`NounPoss-20`[ which(featurestable$Glottocode == glotto)] <- ifelse(hasSuppletion, 'yes', 'no')
    featurestable$`NounPoss-20.Source`[ which(featurestable$Glottocode == glotto)] <- ifelse(hasSuppletion, 
        filter(classes, Glottocode == glotto & Noun_Class == 'SUPPLETIVE')$Source, 
        paste0(collapseSources(unique(c(filter(classes, Glottocode == glotto)$Source))), collapse=';'))
  }
  
  #delete suppletive rows (from the internal table)
  classes <- filter(classes, is.na(Noun_Class) | Noun_Class != 'SUPPLETIVE')
  constructions <- filter(constructions, is.na(Construction_Type) | Construction_Type != 'SUPPLETION')
  
  #non-suppletive questions, with the suppletion rows removed
  for (glotto in glottocodes) {
    
    glottoconstructions <- dplyr::filter(constructions, Glottocode == glotto)
    allsources <- paste0(collapseSources(unique(c(classes$Source[classes$Glottocode==glotto], constructions$Source[constructions$Glottocode==glotto]))), collapse=';')
    
    # populate coder information, in a semantically useless if(TRUE) statement so I can collapse while coding
    constructionscoder <- unique(trimws(unlist(strsplit(constructions$Coder[constructions$Glottocode == glotto], ';'))))
    classescoder <- unique(trimws(unlist(strsplit(classes$Coder[classes$Glottocode == glotto], ';'))))
    coder <- paste(sort(unique(c(constructionscoder, classescoder))), collapse=';')
    if (TRUE) {
      featurestable$`NounPoss-01.Coder`[featurestable$Glottocode == glotto] <- coder 
      featurestable$`NounPoss-01a.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-02.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-03.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-03a.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-03b.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-03c.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-03d.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-04.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-04a.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-04b.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-05.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-06.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-07.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-08.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-08a.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-08b.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-08c.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-09.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-10.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-11.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-12.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-12a.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-13.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-14.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-15.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-16.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-16a.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-17.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-17a.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-21.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-22.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-23.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-24.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-18.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-19.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-20.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-20a.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-25.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-26.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-27.Coder`[featurestable$Glottocode == glotto] <- coder
      featurestable$`NounPoss-28.Coder`[featurestable$Glottocode == glotto] <- coder
    }
    
    
    defaultclass <- classes$ID[classes$Glottocode == glotto & grepl('default', classes$Semantic_Categories)]
    
    inalienableclasses <- classes$ID[classes$Glottocode == glotto & classes$dd_Semantic_Type == 'Inalienable']
    inalienableclassessources <- ifelse(length(inalienableclasses > 0), paste0(collapseSources(c(classes$Source[classes$Glottocode == glotto & classes$dd_Semantic_Type == 'Inalienable'])), collapse=';'), allsources)

    nonpossessibleclasses <- classes$ID[classes$Glottocode == glotto & classes$dd_Semantic_Type == 'Non-possessible']
    nonpossessibleclassessources <- ifelse(length(nonpossessibleclasses > 0), paste0(collapseSources(c(classes$Source[classes$Glottocode == glotto & classes$dd_Semantic_Type == 'Non-possessible'])), collapse=';'), allsources)
    
    mixedclasses <- classes$ID[classes$Glottocode == glotto & classes$dd_Semantic_Type == 'Mixed']
    mixedclassessources <- ifelse(length(mixedclasses > 0), paste0(collapseSources(c(classes$Source[classes$Glottocode == glotto & classes$dd_Semantic_Type == 'Mixed'])), collapse=';'), allsources)
    
    optionallypossessedclasses <- classes$ID[classes$Glottocode == glotto & classes$dd_Valency == 'Optionally possessed']
    optionallypossessedclassessources <- ifelse(length(optionallypossessedclasses > 0), paste0(collapseSources(c(classes$Source[classes$Glottocode == glotto & classes$dd_Valency == 'Optionally possessed'])), collapse=';'), allsources)
    
    obligatorilypossessedclasses <- classes$ID[classes$Glottocode == glotto & classes$dd_Valency == 'Obligatorily possessed']
    obligatorilypossessedclassessources <- ifelse(length(obligatorilypossessedclasses > 0), paste0(collapseSources(c(classes$Source[classes$Glottocode == glotto & classes$dd_Valency == 'Obligatorily possessed'])), collapse=';'), allsources)
    
    nonpossessiblevalenceclasses <- classes$ID[classes$Glottocode == glotto & classes$dd_Valency == 'Non-possessible']
    nonpossessiblevalenceclassessources <- ifelse(length(nonpossessiblevalenceclasses > 0), paste0(collapseSources(c(classes$Source[classes$Glottocode == glotto & classes$dd_Valency == 'Non-possessible'])), collapse=';'), allsources)
    
    # Questions about number of classes: 01-08
    # Total number of classes
    featurestable$`NounPoss-01`[featurestable$Glottocode == glotto] <- length(classes$Noun_Class[classes$Glottocode == glotto])
    featurestable$`NounPoss-01.Source`[featurestable$Glottocode == glotto] <- allsources
    
    featurestable$`NounPoss-01a`[featurestable$Glottocode == glotto] <- ifelse(length(classes$Noun_Class[classes$Glottocode == glotto]) == 1, '1', ifelse(length(classes$Noun_Class[classes$Glottocode == glotto]) == 2, '2', '3-or-more'))
    featurestable$`NounPoss-01a.Source`[featurestable$Glottocode == glotto] <- allsources
    
    # Distribution of classes by semantic category
    classeslist <- 'default:1'
    if (length(inalienableclasses) > 0) {
      classeslist <- paste0(classeslist,';;conceptually inalienable:',length(inalienableclasses))
    }
    if (length(nonpossessibleclasses) > 0) {
      classeslist <- paste0(classeslist,';;conceptually non-possessible:',length(nonpossessibleclasses))
    }
    if (length(mixedclasses) > 0) {
      classeslist <- paste0(classeslist,';;mixed:',length(mixedclasses))
    }
    featurestable$`NounPoss-02`[featurestable$Glottocode == glotto] <- classeslist
    featurestable$`NounPoss-02.Source`[featurestable$Glottocode == glotto] <- allsources
    
    # inalienable (03), non-possessible (04), and mixed (05) numbers
    featurestable$`NounPoss-03`[featurestable$Glottocode == glotto] <- length(inalienableclasses)
    featurestable$`NounPoss-03.Source`[featurestable$Glottocode == glotto] <- inalienableclassessources
    featurestable$`NounPoss-04`[featurestable$Glottocode == glotto] <- length(nonpossessibleclasses)
    featurestable$`NounPoss-04.Source`[featurestable$Glottocode == glotto] <- nonpossessibleclassessources
    featurestable$`NounPoss-05`[featurestable$Glottocode == glotto] <- length(mixedclasses)
    featurestable$`NounPoss-05.Source`[featurestable$Glottocode == glotto] <- mixedclassessources
    
    # deeper dive on inalienables (3a-3d)
    featurestable$`NounPoss-03a`[featurestable$Glottocode == glotto] <- ifelse(length(inalienableclasses) > 0, 'yes', 'no')
    featurestable$`NounPoss-03a.Source`[featurestable$Glottocode == glotto] <- inalienableclassessources
    
    inalienable_obligatory <- ifelse('Obligatorily possessed' %in% classes$dd_Valency[classes$Glottocode == glotto & classes$ID %in% inalienableclasses], 'yes', 'no')
    featurestable$`NounPoss-03b`[featurestable$Glottocode == glotto] <- ifelse(featurestable$`NounPoss-03a`[featurestable$Glottocode == glotto] == 'yes', inalienable_obligatory, 'NA')
    featurestable$`NounPoss-03b.Source`[featurestable$Glottocode == glotto] <- inalienableclassessources
    
    kinship_only <- 'no'
    body_only <- 'no'
    for (class in inalienableclasses) {
      semantics <- trimws(unlist(strsplit(classes$Semantic_Categories[classes$Glottocode == glotto & classes$ID == class],';')))
      if ('body' %in% semantics | 'body_internal' %in% semantics) {
        if (!('kin' %in% semantics) & !('nuclear_kin' %in% semantics) & !('blood_kin' %in% semantics) & !('blood_kin_below_ego' %in% semantics) & !('blood_kin_above_ego' %in% semantics)) {
          body_only <- 'yes'
        }
      }
      if ('kin' %in% semantics | 'nuclear_kin' %in% semantics | 'blood_kin' %in% semantics | 'blood_kin_below_ego' %in% semantics | 'blood_kin_above_ego' %in% semantics) {
        if (!('body' %in% semantics) & !('body_internal' %in% semantics)) {
          kinship_only <- 'yes'
        }
      }
    }
    
    featurestable$`NounPoss-03c`[featurestable$Glottocode == glotto] <- ifelse(featurestable$`NounPoss-03a`[featurestable$Glottocode == glotto] == 'yes', kinship_only, 'NA')
    featurestable$`NounPoss-03c.Source`[featurestable$Glottocode == glotto] <- inalienableclassessources
    
    featurestable$`NounPoss-03d`[featurestable$Glottocode == glotto] <- ifelse(featurestable$`NounPoss-03a`[featurestable$Glottocode == glotto] == 'yes', body_only, 'NA')
    featurestable$`NounPoss-03d.Source`[featurestable$Glottocode == glotto] <- inalienableclassessources
    
    # deeper dive on non-possessibles (4a-4b)
    featurestable$`NounPoss-04a`[featurestable$Glottocode == glotto] <- ifelse(featurestable$`NounPoss-01`[featurestable$Glottocode == glotto] > 1, ifelse(length(nonpossessibleclasses) > 0, 'yes', 'no'), 'NA')
    featurestable$`NounPoss-04a.Source`[featurestable$Glottocode == glotto] <- nonpossessibleclassessources
    
    nonpossessible_nonpossessible <- ifelse(featurestable$`NounPoss-01`[featurestable$Glottocode == glotto] > 1, ifelse('Non-possessible' %in% classes$dd_Valency[classes$Glottocode == glotto & classes$ID %in% nonpossessibleclasses], 'yes', 'no'), 'NA')
    featurestable$`NounPoss-04b`[featurestable$Glottocode == glotto] <- ifelse(featurestable$`NounPoss-04a`[featurestable$Glottocode == glotto] == 'yes', nonpossessible_nonpossessible, 'NA')
    featurestable$`NounPoss-04b.Source`[featurestable$Glottocode == glotto] <- nonpossessibleclassessources
    
    # valency questions: 21-24
    featurestable$`NounPoss-21`[featurestable$Glottocode == glotto] <- length(optionallypossessedclasses)
    featurestable$`NounPoss-21.Source`[featurestable$Glottocode == glotto] <- optionallypossessedclassessources
    featurestable$`NounPoss-22`[featurestable$Glottocode == glotto] <- length(obligatorilypossessedclasses)
    featurestable$`NounPoss-22.Source`[featurestable$Glottocode == glotto] <- obligatorilypossessedclassessources
    featurestable$`NounPoss-24`[featurestable$Glottocode == glotto] <- length(nonpossessiblevalenceclasses)
    featurestable$`NounPoss-24.Source`[featurestable$Glottocode == glotto] <- nonpossessiblevalenceclassessources
    
    # calculate the unpossession construction of inalienable classes, 11
    unpossessioncxns <- unique(classes$dd_Unpossession_Construction[classes$Glottocode == glotto & classes$ID %in% inalienableclasses])
    is_unknown <- FALSE
    if (length(unpossessioncxns) == 1) {
      if (unpossessioncxns == "?") {
        is_unknown <- TRUE
      }
    }
    if (!is_unknown) {
      unpossessioncxns <- c(unpossessioncxns, unlist(strsplit(unique(classes$Unpossession_Marker_Type[classes$Glottocode == glotto & classes$ID %in% inalienableclasses]), ';')))
      unpossessioncxns <- unpossessioncxns[unpossessioncxns != "MARKER" & unpossessioncxns != "NA"]
    }
    
    featurestable$`NounPoss-11`[featurestable$Glottocode == glotto] <- ifelse(length(inalienableclasses) > 0, paste0(unpossessioncxns, collapse=';;'), 'NA')
    featurestable$`NounPoss-11.Source`[featurestable$Glottocode == glotto] <- inalienableclassessources
    
    # obligatorily possessed class with special unpossession: 23
    featurestable$`NounPoss-23`[featurestable$Glottocode == glotto] <- ifelse(length(obligatorilypossessedclasses) > 0, ifelse(length(classes$Noun_Class[classes$Unpossession_Marker_Type == 'SPECIAL_MARKER' & classes$Glottocode == glotto & classes$ID %in% obligatorilypossessedclasses]) > 0, 'yes', 'no'), 'NA')
    featurestable$`NounPoss-23.Source`[featurestable$Glottocode == glotto] <- obligatorilypossessedclassessources
    
    # inalienable semantics and nonpossessible semantics: 06 & 07
    inalienableclasssemantics <- c()
    for (small_class in trimws(unlist(strsplit(classes$Semantic_Categories[classes$Glottocode == glotto & classes$ID %in% inalienableclasses],';')))) {
      if (small_class == 'kin' | small_class == 'blood_kin' | small_class == 'nuclear_kin' | small_class == 'blood_kin_below_ego' | small_class == 'blood_kin_above_ego') {
        inalienableclasssemantics <- c(inalienableclasssemantics, 'kin')
      }
      else if (small_class == 'part' | small_class == 'plant_part') {
        inalienableclasssemantics <- c(inalienableclasssemantics, 'part')
      }
      else if (small_class == 'body' | small_class == 'body_internal') {
          inalienableclasssemantics <- c(inalienableclasssemantics, 'body')
      }
      else if (small_class == 'relation' | small_class == 'owner') {
          inalienableclasssemantics <- c(inalienableclasssemantics, 'relation')
      }
      else if (small_class == 'intimate_property' | small_class == 'clothing' | small_class == 'tools') {
        inalienableclasssemantics <- c(inalienableclasssemantics, 'intimate_property')
      }
    }
    inalienableclasssemantics <- unique(inalienableclasssemantics)
    
    nonpossessibleclasssemantics <- c()
    for (small_class in trimws(unlist(strsplit(classes$Semantic_Categories[classes$Glottocode == glotto & classes$ID %in% nonpossessibleclasses],';')))) {
      if (small_class == 'humans') {
        nonpossessibleclasssemantics <- c(nonpossessibleclasssemantics, 'humans')
      }
      else if (small_class == 'plants') {
        nonpossessibleclasssemantics <- c(nonpossessibleclasssemantics, 'plants')
      }
      else if (small_class == 'wild_animals' | small_class == 'animals') {
        nonpossessibleclasssemantics <- c(nonpossessibleclasssemantics, 'animals')
      }
      else if (small_class == 'nature_inanimate' | small_class == 'nature' | small_class == 'celestial') {
        nonpossessibleclasssemantics <- c(nonpossessibleclasssemantics, 'nature_inanimate')
      }
      else if (small_class == 'names') {
        nonpossessibleclasssemantics <- c(nonpossessibleclasssemantics, 'names')
      }
    }
    nonpossessibleclasssemantics <- unique(nonpossessibleclasssemantics)
    
    featurestable$`NounPoss-06`[featurestable$Glottocode == glotto] <- ifelse(length(inalienableclasses) > 0, paste0(inalienableclasssemantics, collapse=';;'), 'NA')
    featurestable$`NounPoss-06.Source`[featurestable$Glottocode == glotto] <- inalienableclassessources
    
    featurestable$`NounPoss-07`[featurestable$Glottocode == glotto] <- ifelse(length(nonpossessibleclasses) > 0, paste0(nonpossessibleclasssemantics, collapse=';;'), 'NA')
    featurestable$`NounPoss-07.Source`[featurestable$Glottocode == glotto] <- nonpossessibleclassessources
    
    # constructions for default, inalienable, and non-possessible classes: 08 - 10
    defaultcxs <- unique(glottoconstructions$dd_Construction_Type_Simplified[glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',defaultclass,'($|\\s|;)'), glottoconstructions$Class_ID)])
    defaultcxs <- unique(replace(defaultcxs, defaultcxs == 'PRO_CLAUSE', 'CLAUSE'))
    defaultcxs <- unique(replace(defaultcxs, defaultcxs == 'PRO_JUXT', 'JUXT'))
    defaultcxs <- unique(replace(defaultcxs, defaultcxs == 'PSSR-PSSD', 'PSSR and PSSD'))
    if (length(defaultcxs) == 0) {
      defaultcxs <- '?'
    } else if (length(defaultcxs) == 1 && defaultcxs == 'MARKER') {
      defaultcxs <- '?'
    } else { defaultcxs <- paste0(defaultcxs[defaultcxs != 'MARKER'], collapse=';;') }
    defaultcxssources <- paste0(collapseSources(c(glottoconstructions$Source[glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',defaultclass,'($|\\s|;)'), glottoconstructions$Class_ID)])), collapse=';')
    
    if (length(inalienableclasses) == 0) {
      inalienablecxs <- 'NA'
      inalienablecxssources <- allsources
    } else {
      inalienablecxs <- unique(glottoconstructions$dd_Construction_Type_Simplified[glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(inalienableclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)])
      inalienablecxs <- unique(replace(inalienablecxs, inalienablecxs == 'PRO_CLAUSE', 'CLAUSE'))
      inalienablecxs <- unique(replace(inalienablecxs, inalienablecxs == 'PRO_JUXT', 'JUXT'))
      inalienablecxs <- unique(replace(inalienablecxs, inalienablecxs == 'PSSR-PSSD', 'PSSR and PSSD'))
      if (length(inalienablecxs) == 0) { 
        inalienablecxs <- '?' 
      } else if (length(inalienablecxs) == 1 && inalienablecxs == 'MARKER') {
        inalienablecxs <- '?'
      } else { inalienablecxs <- paste0(inalienablecxs[inalienablecxs != 'MARKER'], collapse = ';;') }
      inalienablecxssources <- paste0(collapseSources(c(glottoconstructions$Source[glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(inalienableclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)])), collapse=';')
    }
    
    if (length(nonpossessibleclasses) == 0) {
      nonpossessiblecxs <- 'NA'
      nonpossessiblecxssources <- allsources
    } else {
      nonpossessiblecxs <- unique(glottoconstructions$dd_Construction_Type_Simplified[glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(nonpossessibleclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)])
      nonpossessiblecxs <- unique(replace(nonpossessiblecxs, nonpossessiblecxs == 'PRO_CLAUSE', 'CLAUSE'))
      nonpossessiblecxs <- unique(replace(nonpossessiblecxs, nonpossessiblecxs == 'PRO_JUXT', 'JUXT'))
      nonpossessiblecxs <- unique(replace(nonpossessiblecxs, nonpossessiblecxs == 'PSSR-PSSD', 'PSSR and PSSD'))
      if (length(nonpossessiblecxs) == 0) { 
        nonpossessiblecxs <- '?' 
      } else if (length(nonpossessiblecxs) == 1 && nonpossessiblecxs == 'MARKER') {
        nonpossessiblecxs <- '?'
      } else { nonpossessiblecxs <- paste0(nonpossessiblecxs[nonpossessiblecxs != 'MARKER'], collapse = ';;') }
      nonpossessiblecxssources <- paste0(collapseSources(c(glottoconstructions$Source[glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(nonpossessibleclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)])), collapse=';')
    }
    
    featurestable$`NounPoss-08`[featurestable$Glottocode == glotto] <- defaultcxs
    featurestable$`NounPoss-08.Source`[featurestable$Glottocode == glotto] <- defaultcxssources
    
    featurestable$`NounPoss-09`[featurestable$Glottocode == glotto] <- inalienablecxs
    featurestable$`NounPoss-09.Source`[featurestable$Glottocode == glotto] <- inalienablecxssources
    
    featurestable$`NounPoss-10`[featurestable$Glottocode == glotto] <- nonpossessiblecxs
    featurestable$`NounPoss-10.Source`[featurestable$Glottocode == glotto] <- nonpossessiblecxssources
    
    # deeper dive on default construction possession: 08a (PSSR marking), 08b (PSSD marking), 08c (combined)
    featurestable$`NounPoss-08a`[featurestable$Glottocode == glotto] <- 
      ifelse('PRO_MARKER' %in% defaultcxs & !('PSSD' %in% defaultcxs | 'PSSR' %in% defaultcxs | 'PSSR and PSSD' %in% defaultcxs | 'LINKER' %in% defaultcxs), '?',
      ifelse('PSSD' %in% defaultcxs | 'PSSR' %in% defaultcxs | 'PSSR and PSSD' %in% defaultcxs | 'LINKER' %in% defaultcxs | 'MARKER' %in% defaultcxs, 
      ifelse('PSSR' %in% defaultcxs | 'PSSR and PSSD' %in% defaultcxs, 'yes', 
      ifelse('MARKER' %in% defaultcxs, '?', 'no')), 
      'NA'))
    featurestable$`NounPoss-08a.Source`[featurestable$Glottocode == glotto] <- defaultcxssources
    
    featurestable$`NounPoss-08b`[featurestable$Glottocode == glotto] <- 
      ifelse('PRO_MARKER' %in% defaultcxs & !('PSSD' %in% defaultcxs | 'PSSR' %in% defaultcxs | 'PSSR and PSSD' %in% defaultcxs | 'LINKER' %in% defaultcxs), '?',
      ifelse('PSSD' %in% defaultcxs | 'PSSR' %in% defaultcxs | 'PSSR and PSSD' %in% defaultcxs | 'LINKER' %in% defaultcxs | 'MARKER' %in% defaultcxs, 
      ifelse('PSSD' %in% defaultcxs | 'PSSR and PSSD' %in% defaultcxs, 'yes', 
      ifelse('MARKER' %in% defaultcxs, '?', 'no')), 
      'NA'))
    featurestable$`NounPoss-08b.Source`[featurestable$Glottocode == glotto] <- defaultcxssources
    
    featurestable$`NounPoss-08c`[featurestable$Glottocode == glotto] <- 
      ifelse('PRO_MARKER' %in% defaultcxs & !('PSSD' %in% defaultcxs | 'PSSR' %in% defaultcxs | 'PSSR and PSSD' %in% defaultcxs | 'LINKER' %in% defaultcxs), '?',
      ifelse('PSSD' %in% defaultcxs | 'PSSR' %in% defaultcxs | 'PSSR and PSSD' %in% defaultcxs | 'MARKER' %in% defaultcxs, 
      ifelse('PSSR and PSSD' %in% defaultcxs & !('PSSR' %in% defaultcxs) & !('PSSD' %in% defaultcxs), 'PSSR and PSSD', 
      ifelse('PSSR' %in% defaultcxs & !('PSSR and PSSD' %in% defaultcxs) & !('PSSD' %in% defaultcxs), 'PSSR', 
      ifelse('PSSD' %in% defaultcxs & !('PSSR' %in% defaultcxs) & !('PSSR and PSSD' %in% defaultcxs), 'PSSD',
      '?'))), 
      'NA'))
    featurestable$`NounPoss-08c.Source`[featurestable$Glottocode == glotto] <- defaultcxssources
    
    
    # individual constructions: 12-17
    juxtclasses <- unique(trimws(unlist(strsplit(unique(glottoconstructions$Class_ID[glottoconstructions$dd_Construction_Type_Simplified == 'JUXT' & glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION']), ';'))))
    if (length(juxtclasses) > 0) {
      juxtclassessources <- paste0(collapseSources(c(unique(glottoconstructions$Source[glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(juxtclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)]))), collapse=';')
      juxtclassessemantics <- tolower(paste0(unique(classes$dd_Semantic_Type[classes$Glottocode == glotto & classes$ID %in% juxtclasses]), collapse=';;'))
      if (juxtclassessemantics == 'both') {
        juxtclassessemantics <- '?'
      }
      if (grepl('both',juxtclassessemantics)) {
        juxtclassessemantics <- gsub('both','?',juxtclassessemantics)
      }
    } else { 
      juxtclassessemantics <- 'no' 
      juxtclassessources <- allsources
    }
    
    pssrclasses <- unique(trimws(unlist(strsplit(unique(glottoconstructions$Class_ID[glottoconstructions$dd_Construction_Type_Simplified == 'PSSR' & glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION']), ';'))))
    if (length(pssrclasses) > 0) {
      pssrclassessources <- paste0(collapseSources(c(unique(glottoconstructions$Source[glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(pssrclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)]))), collapse=';')
      pssrclassessemantics <- tolower(paste0(unique(classes$dd_Semantic_Type[classes$Glottocode == glotto & classes$ID %in% pssrclasses]), collapse=';;'))
      if (pssrclassessemantics == 'both') {
        pssrclassessemantics <- '?'
      }
      if (grepl('both',pssrclassessemantics)) {
        pssrclassessemantics <- gsub('both','?',pssrclassessemantics)
      }
    } else { 
      pssrclassessemantics <- 'no' 
      pssrclassessources <- allsources
    }
    
    pssdclasses <- unique(trimws(unlist(strsplit(unique(glottoconstructions$Class_ID[glottoconstructions$dd_Construction_Type_Simplified == 'PSSD' & glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION']), ';'))))
    if (length(pssdclasses) > 0) {
      pssdclassessources <- paste0(collapseSources(c(unique(glottoconstructions$Source[glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(pssdclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)]))), collapse=';')
      pssdclassessemantics <- tolower(paste0(unique(classes$dd_Semantic_Type[classes$Glottocode == glotto & classes$ID %in% pssdclasses]), collapse=';;'))
      if (pssdclassessemantics == 'both') {
        pssdclassessemantics <- '?'
      }
      if (grepl('both',pssdclassessemantics)) {
        pssdclassessemantics <- gsub('both','?',pssdclassessemantics)
      }
    } else { 
      pssdclassessemantics <- 'no' 
      pssdclassessources <- allsources
    }
    
    pssrpssdclasses <- unique(trimws(unlist(strsplit(unique(glottoconstructions$Class_ID[glottoconstructions$dd_Construction_Type_Simplified == 'PSSR-PSSD' & glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION']), ';'))))
    if (length(pssrpssdclasses) > 0) {
      pssrpssdclassessources <- paste0(collapseSources(c(unique(glottoconstructions$Source[glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(pssrpssdclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)]))), collapse=';')
      pssrpssdclassessemantics <- tolower(paste0(unique(classes$dd_Semantic_Type[classes$Glottocode == glotto & classes$ID %in% pssrpssdclasses]), collapse=';;'))
      if (pssrpssdclassessemantics == 'both') {
        pssrpssdclassessemantics <- '?'
      }
      if (grepl('both',pssrpssdclassessemantics)) {
        pssrpssdclassessemantics <- gsub('both','?',pssrpssdclassessemantics)
      }
    } else { 
      pssrpssdclassessemantics <- 'no' 
      pssrpssdclassessources <- allsources
    }
    
    clauseclasses <- unique(trimws(unlist(strsplit(unique(glottoconstructions$Class_ID[(glottoconstructions$dd_Construction_Type_Simplified == 'CLAUSE' | glottoconstructions$dd_Construction_Type_Simplified == 'PRO_CLAUSE') & glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION']), ';'))))
    if (length(clauseclasses) > 0) {
      clauseclassessources <- paste0(collapseSources(c(unique(glottoconstructions$Source[glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(clauseclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)]))), collapse=';')
      clauseclassessemantics <- tolower(paste0(unique(classes$dd_Semantic_Type[classes$Glottocode == glotto & classes$ID %in% clauseclasses]), collapse=';;'))
      if (clauseclassessemantics == 'both') {
        clauseclassessemantics <- '?'
      }
      if (grepl('both',clauseclassessemantics)) {
        clauseclassessemantics <- gsub('both','?',clauseclassessemantics)
      }
    } else { 
      clauseclassessemantics <- 'no' 
      clauseclassessources <- allsources
    }
    
    classclasses <- unique(trimws(unlist(strsplit(unique(glottoconstructions$Class_ID[(glottoconstructions$dd_Construction_Type_Simplified == 'CLASS' | glottoconstructions$dd_Construction_Type_Simplified == 'PRO_CLASS') & glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION']), ';'))))
    if (length(classclasses) > 0) {
      classclassessources <- paste0(collapseSources(c(unique(glottoconstructions$Source[glottoconstructions$Construction_Type != 'POSSESSION' & glottoconstructions$Construction_Type != 'UNPOSSESSION' & grepl(paste0('(^|\\s|;)',paste0(classclasses,collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)]))), collapse=';')
      classclassessemantics <- tolower(paste0(unique(classes$dd_Semantic_Type[classes$Glottocode == glotto & classes$ID %in% classclasses]), collapse=';;'))
      if (classclassessemantics == 'both') {
        classclassessemantics <- '?'
      }
      if (grepl('both',classclassessemantics)) {
        classclassessemantics <- gsub('both','?',classclassessemantics)
      }
    } else { 
      classclassessemantics <- 'no' 
      classclassessources <- allsources
    }
    
    featurestable$`NounPoss-12`[featurestable$Glottocode == glotto] <- juxtclassessemantics
    featurestable$`NounPoss-12.Source`[featurestable$Glottocode == glotto] <- juxtclassessources
    
    featurestable$`NounPoss-13`[featurestable$Glottocode == glotto] <- pssrclassessemantics
    featurestable$`NounPoss-13.Source`[featurestable$Glottocode == glotto] <- pssrclassessources
    
    featurestable$`NounPoss-14`[featurestable$Glottocode == glotto] <- pssdclassessemantics
    featurestable$`NounPoss-14.Source`[featurestable$Glottocode == glotto] <- pssdclassessources
    
    featurestable$`NounPoss-15`[featurestable$Glottocode == glotto] <- pssrpssdclassessemantics
    featurestable$`NounPoss-15.Source`[featurestable$Glottocode == glotto] <- pssrpssdclassessources
    
    featurestable$`NounPoss-16`[featurestable$Glottocode == glotto] <- clauseclassessemantics
    featurestable$`NounPoss-16.Source`[featurestable$Glottocode == glotto] <- clauseclassessources
    
    featurestable$`NounPoss-17`[featurestable$Glottocode == glotto] <- classclassessemantics
    featurestable$`NounPoss-17.Source`[featurestable$Glottocode == glotto] <- classclassessources
    
    # sBayes derivations: 12a, 16a, 17a
    featurestable$`NounPoss-12a`[featurestable$Glottocode == glotto] <- 
      ifelse(juxtclassessemantics == 'no', 'no JUXT', 
      ifelse(juxtclassessemantics == 'inalienable', 'only inalienables', 
      ifelse(grepl('default', juxtclassessemantics), 'default plus', '?')))
    featurestable$`NounPoss-12a.Source`[featurestable$Glottocode == glotto] <- juxtclassessources
    
    featurestable$`NounPoss-16a`[featurestable$Glottocode == glotto] <- ifelse(clauseclassessemantics == 'no', 'no', 'yes')
    featurestable$`NounPoss-16a.Source`[featurestable$Glottocode == glotto] <- clauseclassessources
    
    featurestable$`NounPoss-17a`[featurestable$Glottocode == glotto] <- 
      ifelse(classclassessemantics == 'no', 'none', 
      ifelse(classclassessemantics == 'non-possessible', 'only non-possessibles',
      ifelse(grepl('default', classclassessemantics), 'default plus', '?')))
    featurestable$`NounPoss-17a.Source`[featurestable$Glottocode == glotto] <- classclassessources
    
    # agreement presence on (PSSR-)PSSD constructions, 18 + 19
    pssdagrpssr <- 'NA'
    if (length(pssdclasses) > 0 | length(pssrpssdclasses) > 0) {
      pssdagrpssr <- ifelse(any(grepl('AGR:PSSR', 
                     glottoconstructions$Construction_Type[
                     glottoconstructions$Construction_Type != 'POSSESSION' & 
                     glottoconstructions$Construction_Type != 'UNPOSSESSION' & 
                     grepl(paste0('(^|\\s|;)',paste0(unique(c(pssdclasses, pssrpssdclasses)), collapse='|'),'($|\\s|;)'), glottoconstructions$Class_ID)])),
                    'yes', 'no')
    }
    
    featurestable$`NounPoss-18`[featurestable$Glottocode == glotto] <- pssdagrpssr
    featurestable$`NounPoss-18.Source`[featurestable$Glottocode == glotto] <- paste0(collapseSources(c(pssdclassessources, pssrpssdclassessources)), collapse=';')
    
    pssdalone <- 'no'
    if (length(pssdclasses) > 0 | length(pssrpssdclasses) > 0) {
      for (class in unique(c(pssdclasses, pssrpssdclasses))) {
        for (cxntype in glottoconstructions$Construction_Type[grepl(paste0('(^|\\s|;)',class,'($|\\s|;)'), glottoconstructions$Class_ID)]) {
          if (grepl("PSSD(?!\\[)", cxntype, perl=TRUE)) {
            pssdalone <- 'yes'
          }
        }
      }
    }
    featurestable$`NounPoss-19`[featurestable$Glottocode == glotto] <- 
      ifelse(length(pssdclasses) > 0 | length(pssrpssdclasses) > 0, pssdalone, 'NA')
    featurestable$`NounPoss-19.Source`[featurestable$Glottocode == glotto] <- paste0(collapseSources(c(pssdclassessources, pssrpssdclassessources)), collapse=';')
    
    # we took care of suppletion (20) early on, now for conditioned suppletion (20a)
    featurestable$`NounPoss-20a`[featurestable$Glottocode == glotto] <- 
      ifelse(length(classes$Noun_Class[classes$Glottocode == glotto]) > 1,
          ifelse(featurestable$`NounPoss-20`[featurestable$Glottocode == glotto] == 'yes', 'yes', 'no'),
          'NA')
    featurestable$`NounPoss-20a.Source`[featurestable$Glottocode == glotto] <- featurestable$`NounPoss-20.Source`[featurestable$Glottocode == glotto]
    
    # valency questions: 25, 26, 27, 28
    featurestable$`NounPoss-25`[featurestable$Glottocode == glotto] <- tolower(classes$dd_Valency[classes$Glottocode == glotto & grepl(paste0('(^|\\s|;)',defaultclass,'($|\\s|;)'), classes$ID)])
    featurestable$`NounPoss-25.Source`[featurestable$Glottocode == glotto] <- classes$Source[classes$Glottocode == glotto & grepl(paste0('(^|\\s|;)',defaultclass,'($|\\s|;)'), classes$ID)]
    
    inalienablevalencies <- as.data.frame(table(classes$dd_Valency[classes$Glottocode == glotto & grepl(paste0('(^|\\s|;)',paste0(inalienableclasses, collapse='|'),'($|\\s|;)'), classes$ID)]))
    inalienablevalencyfreq <- c()
    for (inal in inalienablevalencies$Var1) {
      inalienablevalencyfreq <- c(inalienablevalencyfreq,paste0(tolower(inal),':', inalienablevalencies$Freq[inalienablevalencies$Var1 == inal]))
    }
    inalienablevalencyfreq <- paste0(inalienablevalencyfreq, collapse=';;')
    
    nonpossessiblevalencies <- as.data.frame(table(classes$dd_Valency[classes$Glottocode == glotto & grepl(paste0('(^|\\s|;)',paste0(nonpossessibleclasses, collapse='|'),'($|\\s|;)'), classes$ID)]))
    nonpossessiblevalencyfreq <- c()
    for (nonposs in nonpossessiblevalencies$Var1) {
      nonpossessiblevalencyfreq <- c(nonpossessiblevalencyfreq, paste0(tolower(nonposs),':', nonpossessiblevalencies$Freq[nonpossessiblevalencies$Var1 == nonposs]))
    }
    nonpossessiblevalencyfreq <- paste0(nonpossessiblevalencyfreq, collapse=';;')
    
    mixedvalencies <- as.data.frame(table(classes$dd_Valency[classes$Glottocode == glotto & grepl(paste0('(^|\\s|;)',paste0(mixedclasses, collapse='|'),'($|\\s|;)'), classes$ID)]))
    mixedvalencyfreq <- c()
    for (m in mixedvalencies$Var1) {
      mixedvalencyfreq <- c(mixedvalencyfreq, paste0(tolower(m),':', mixedvalencies$Freq[mixedvalencies$Var1 == m]))
    }
    mixedvalencyfreq <- paste0(mixedvalencyfreq, collapse=';;')
    
    featurestable$`NounPoss-26`[featurestable$Glottocode == glotto] <- ifelse(length(inalienableclasses) == 0, 'NA', inalienablevalencyfreq)
    featurestable$`NounPoss-26.Source`[featurestable$Glottocode == glotto] <- inalienableclassessources
    
    featurestable$`NounPoss-27`[featurestable$Glottocode == glotto] <- ifelse(length(nonpossessibleclasses) == 0, 'NA', nonpossessiblevalencyfreq)
    featurestable$`NounPoss-27.Source`[featurestable$Glottocode == glotto] <- nonpossessibleclassessources
    
    featurestable$`NounPoss-28`[featurestable$Glottocode == glotto] <- ifelse(length(mixedclasses) == 0, 'NA', mixedvalencyfreq)
    featurestable$`NounPoss-28.Source`[featurestable$Glottocode == glotto] <- mixedclassessources
  }
  
  return(featurestable)
}
