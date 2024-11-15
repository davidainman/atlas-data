source('utils.R')

POSSIBLE_ALIGNMENTS <- c('no marking', 'accusative', 'ergative', 'overt neutral', 'tripartite', 'horizontal', 'sensitive')
POSSIBLE_ALIGNMENTS_DOTS <- str_replace(POSSIBLE_ALIGNMENTS, ' ', '.')

quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

check_for_split_s <- function(referencescsv, glot, domain) {
  split_s <- FALSE
  split_pred_classes <- c()
  
  default_bivalent_classes <- "default"
  if(!("default" %in% referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == domain])) {
    default_bivalent_classes <- unique(referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == domain])
  }
  
  pred_classes <- unique(referencescsv$Monovalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == domain])
  if (length(pred_classes) > 1) {
    for (ref in unique(referencescsv$Referential_type[referencescsv$Glottocode == glot & referencescsv$Domain == domain])) {
      for (tam in unique(referencescsv$TAM[referencescsv$Glottocode == glot & referencescsv$Domain == domain & referencescsv$Referential_type == ref])) {
        match_a = FALSE
        match_p = FALSE
        for (cond in unique(referencescsv$Condition[referencescsv$Glottocode == glot & referencescsv$Domain == domain & referencescsv$TAM == tam & referencescsv$Referential_type == ref & referencescsv$TAM == tam])) {
          a_selectors <- c()
          for (selector in gsub('_coarg:.*|_slot".*','',unique(trimws(unlist(strsplit(referencescsv$A[referencescsv$Glottocode == glot & referencescsv$Domain == domain & referencescsv$Referential_type == ref & referencescsv$TAM == tam & referencescsv$Condition == cond & referencescsv$Bivalent_predicate_class %in% default_bivalent_classes], ";")))))) {
            if (grepl('_zero', selector)) {
              a_selectors <- c(a_selectors, "ZERO")
            }
            else {
              a_selectors <- c(a_selectors, selector)
            }
          }
          p_selectors <- c()
          for (selector in gsub('_coarg:.*|_slot".*','',unique(trimws(unlist(strsplit(referencescsv$P[referencescsv$Glottocode == glot & referencescsv$Domain == domain & referencescsv$Referential_type == ref & referencescsv$TAM == tam & referencescsv$Condition == cond & referencescsv$Bivalent_predicate_class %in% default_bivalent_classes], ";")))))) {
            if (grepl('_zero', selector)) {
              p_selectors <- c(p_selectors, "ZERO")
            }
            else {
              p_selectors <- c(p_selectors, selector)
            }
          }
          for (pred_class in pred_classes) {
            s_selectors <- c()
            for (selector in unique(referencescsv$S[referencescsv$Glottocode == glot & referencescsv$Domain == domain & referencescsv$Referential_type == ref & referencescsv$TAM == tam & referencescsv$Condition == cond & referencescsv$Monovalent_predicate_class == pred_class])) {
              if (grepl('_zero', selector)) {
                s_selectors <- c(s_selectors, "ZERO")
              }
              else {
                s_selectors <- c(s_selectors, selector)
              }
            }
            if (any(s_selectors %in% a_selectors) & !(any(s_selectors %in% p_selectors))) {
              match_a <- TRUE
            }
            if (any(s_selectors %in% p_selectors) & !(any(s_selectors %in% a_selectors))) {
              match_p <- TRUE
            }
          }
          if (match_a & match_p) {
            split_s <- TRUE
            split_pred_classes <- c(split_pred_classes, paste0(sort(pred_classes), collapse='/'))
          }
        }
      }
    }
  }
  
  if (split_s == FALSE) {
    return(FALSE)
  }
  else {
    return(unique(split_pred_classes))
  }
  
}

processAlignment <- function(featurestable, languagescsv, contextscsv, selectorscsv, referencescsv, domscsv, valuescsv) {

  # smash the coders together in a lookup table
  # !! note that the lookup table is ordered identically with featurestable
  coderslookup <- data.frame(Glottocode=character(), Coder=character())
  for (glot in featurestable$Glottocode) {
    coders <- unique(c(unique(contextscsv$Coder[contextscsv$Glottocode == glot]), unique(selectorscsv$Coder[selectorscsv$Glottocode==glot])))
    coders <- coders[!is.na(coders)]
    coders <- paste0(sort(unique(trimws(unlist(strsplit(coders, ";"))))), collapse=";")
    coderslookup[nrow(coderslookup) + 1,] <- c(glot, coders)
  }
  
  #Align-06: Indexing
  stateCol <- ifelse(
    featurestable$Glottocode %in% valuescsv$Glottocode[valuescsv$Parameter_ID == "indexing" & valuescsv$Value == "True"], 'yes',
    ifelse(
      featurestable$Glottocode %in% valuescsv$Glottocode[valuescsv$Parameter_ID == "indexing" & valuescsv$Value == "False"], 'no', NA
    ))
  featurestable <- addDerivedState(featurestable, 'Align-06', stateCol, rep(NA, nrow(featurestable)), coderslookup$Coder)
  for (glot in featurestable$Glottocode) {
    featurestable$`Align-06.Source`[featurestable$Glottocode == glot] <- 
      ifelse(glot %in% valuescsv$Glottocode[valuescsv$Parameter_ID == 'indexing'],
             valuescsv$Source[valuescsv$Glottocode == glot & valuescsv$Parameter_ID == 'indexing'], NA)
  }
  
  #Align-03: DOM in nouns
  featurestable <- addDerivedState(featurestable, 'Align-03', rep(NA,nrow(featurestable)), rep(NA, nrow(featurestable)), coderslookup$Coder)
  for (glot in valuescsv$Glottocode) {
    if (glot %in% languagescsv$Glottocode) {
      featurestable$`Align-03.Source`[featurestable$Glottocode == glot] <- valuescsv$Source[valuescsv$Glottocode == glot & valuescsv$Parameter_ID == "dom"]
      if(glot %in% unique(domscsv$Glottocode)) {
        if (all(grepl("Reference",domscsv$Conditioning[domscsv$Glottocode == glot]))) {
          noun_markings <- c()
          for (condition in domscsv$Conditioning[domscsv$Glottocode == glot]) {
            for (colname in colnames(domscsv)[grepl("Condition_",colnames(domscsv))]) {
              for (var in unlist(strsplit(domscsv[[colname]][domscsv$Glottocode == glot & domscsv$Conditioning == condition], ";"))) {
                if (grepl("Noun",unlist(strsplit(var, "\\+"))[1])) {
                  number <- gsub(".*_","",colname)
                  noun_markings <- c(noun_markings, domscsv[[paste0("Marking_",number)]][domscsv$Glottocode == glot & domscsv$Conditioning == condition])
                }
              }
            }
          }
          noun_markings <- unique(noun_markings)
          if (length(noun_markings) > 1) { # different markings for nouns
            featurestable$`Align-03`[featurestable$Glottocode == glot] <- "yes"
          }
          else {
            featurestable$`Align-03`[featurestable$Glottocode == glot] <- "no"
          }
        }
        else { # DOM is not based on reference -- it must include nouns
          featurestable$`Align-03`[featurestable$Glottocode == glot] <- "yes"
        }
      }
      else {
        featurestable$`Align-03`[featurestable$Glottocode == glot] <- "no"
      }
    }
  }
  
  #Align-09 S != A, flagging, 'ergativity senu lato'
  stateCol <- ifelse(!(featurestable$Glottocode %in% unique(referencescsv$Glottocode[referencescsv$Domain == 'Pro' | referencescsv$Domain == 'Noun'])), NA,
              ifelse(featurestable$Glottocode %in% languagescsv$Glottocode[languagescsv$sufficient_data_flagging == 'False'], '?',
              ifelse(featurestable$Glottocode %in% referencescsv$Glottocode[(referencescsv$Domain == 'Pro' | referencescsv$Domain == 'Noun') & (referencescsv$Alignment_not_local == 'ergative' | referencescsv$Alignment_not_local == 'tripartite' | referencescsv$Alignment_not_local == 'horizontal')], 'yes', 'no')))
  featurestable <- addDerivedState(featurestable, 'Align-09', stateCol, rep(NA, nrow(featurestable)), coderslookup$Coder)
  for (glot in featurestable$Glottocode) {
    featurestable$`Align-09.Source`[featurestable$Glottocode == glot] <- 
      ifelse(glot %in% referencescsv$Glottocode[referencescsv$Domain == 'Pro' | referencescsv$Domain == 'Noun'],
             paste0(collapseSources(unique(referencescsv$Source[referencescsv$Glottocode == glot & (referencescsv$Domain == 'Pro' | referencescsv$Domain == 'Noun')])), collapse=';'), NA)
  }
  
  #Align-10 S != A, indexing, 'ergativity senu lato'
  stateCol <- ifelse(is.na(featurestable$`Align-06`), NA,
              ifelse(featurestable$Glottocode %in% languagescsv$Glottocode[languagescsv$sufficient_data_indexing == 'False'], '?',
              ifelse(featurestable$Glottocode %in% referencescsv$Glottocode[(referencescsv$Domain == 'Verb') & (referencescsv$Alignment_not_local == 'ergative' | referencescsv$Alignment_not_local == 'tripartite' | referencescsv$Alignment_not_local == 'horizontal')], 'yes', 'no')))
  featurestable <- addDerivedState(featurestable, 'Align-10', stateCol, rep(NA, nrow(featurestable)), coderslookup$Coder)
  for (glot in featurestable$Glottocode) {
    featurestable$`Align-10.Source`[featurestable$Glottocode == glot] <- 
      ifelse(glot %in% referencescsv$Glottocode[referencescsv$Domain == 'Verb'],
             paste0(collapseSources(unique(referencescsv$Source[referencescsv$Glottocode == glot & (referencescsv$Domain == 'Verb')])), collapse=';'), 
             valuescsv$Source[valuescsv$Glottocode == glot & valuescsv$Parameter_ID == 'indexing'])
  }
  
  #Align-11: Split-S according to predicate class
  stateCol <- ifelse(is.na(featurestable$`Align-09`) & is.na(featurestable$`Align-10`), NA,
              ifelse(featurestable$`Align-09` == 'yes' | featurestable$`Align-10` == 'yes',
              ifelse(featurestable$Glottocode %in% languagescsv$Glottocode[languagescsv$sufficient_data_flagging == 'False' | languagescsv$sufficient_data_indexing == 'False'], '?',
              ifelse(featurestable$Glottocode %in% valuescsv$Glottocode[valuescsv$Parameter_ID == 'split-s' & valuescsv$Value == 'True'], 'yes', 'no')),
              'NA'))
  featurestable <- addDerivedState(featurestable, 'Align-11', stateCol, rep(NA, nrow(featurestable)), coderslookup$Coder)
  for (glot in featurestable$Glottocode) {
    featurestable$`Align-11.Source`[featurestable$Glottocode == glot] <-
      ifelse(glot %in% valuescsv$Glottocode[valuescsv$Parameter_ID == 'split-s'],
             valuescsv$Source[valuescsv$Parameter_ID == 'split-s' &
                                      valuescsv$Glottocode == glot], NA)
    # fill in remark
    split_s = FALSE
    split_pred_classes = c()
    default_bivalent_pro_classes <- ifelse("default" %in% referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Pro"], c("default"), referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Pro"])
    default_bivalent_noun_classes <- ifelse("default" %in% referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Noun"], c("default"), referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Noun"])
    default_bivalent_indexing_classes <- ifelse("default" %in% referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Verb"], c("default"), referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Verb"])
    
    pro_split_s <- check_for_split_s(referencescsv, glot, "Pro")
    noun_split_s <- check_for_split_s(referencescsv, glot, "Noun")
    verb_split_s <- check_for_split_s(referencescsv, glot, "Verb")
    
    if (all(pro_split_s  != FALSE)) {
      split_s <- TRUE
      split_pred_classes <- c(split_pred_classes, pro_split_s)
    } 
    if (all(noun_split_s != FALSE)) {
      split_s <- TRUE
      split_pred_classes <- c(split_pred_classes, noun_split_s)
    } 
    if (all(verb_split_s != FALSE)) {
      split_s <- TRUE
      split_pred_classes <- c(split_pred_classes, verb_split_s)
    }
    featurestable$`Align-11.Remark`[featurestable$Glottocode == glot] <-
      ifelse(glot %in% valuescsv$Glottocode[valuescsv$Parameter_ID == 'split-s'],
             paste0(sort(unique(split_pred_classes)), collapse = "; "),
             'derived')
  }
  
  #Align-12: S split that matches A/P
  featurestable <- addDerivedState(featurestable, 'Align-12', rep(NA,nrow(featurestable)), rep(NA, nrow(featurestable)), coderslookup$Coder)
  for (glot in featurestable$Glottocode) {
    if (glot %in% languagescsv$Glottocode) {    # shortcut to get the source
      featurestable$`Align-12.Source`[featurestable$Glottocode == glot] <- valuescsv$Source[valuescsv$Parameter_ID == 'split-s' & valuescsv$Glottocode == glot]
      if (is.na(featurestable$`Align-09`[featurestable$Glottocode == glot]) | is.na(featurestable$`Align-10`[featurestable$Glottocode == glot]) | (featurestable$`Align-09`[featurestable$Glottocode == glot] == "no" & featurestable$`Align-10`[featurestable$Glottocode == glot] == "no")) {
        featurestable$`Align-12`[featurestable$Glottocode == glot] <- "NA"
      }
      else {
        split_s = FALSE
        split_pred_classes = c()
        default_bivalent_pro_classes <- ifelse("default" %in% referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Pro"], c("default"), referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Pro"])
        default_bivalent_noun_classes <- ifelse("default" %in% referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Noun"], c("default"), referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Noun"])
        default_bivalent_indexing_classes <- ifelse("default" %in% referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Verb"], c("default"), referencescsv$Bivalent_predicate_class[referencescsv$Glottocode == glot & referencescsv$Domain == "Verb"])
        
        pro_split_s <- check_for_split_s(referencescsv, glot, "Pro")
        noun_split_s <- check_for_split_s(referencescsv, glot, "Noun")
        verb_split_s <- check_for_split_s(referencescsv, glot, "Verb")
        
        if (all(pro_split_s  != FALSE)) {
          split_s <- TRUE
          split_pred_classes <- c(split_pred_classes, pro_split_s)
        } 
        if (all(noun_split_s != FALSE)) {
          split_s <- TRUE
          split_pred_classes <- c(split_pred_classes, noun_split_s)
        } 
        if (all(verb_split_s != FALSE)) {
          split_s <- TRUE
          split_pred_classes <- c(split_pred_classes, verb_split_s)
        }
        featurestable$`Align-12`[featurestable$Glottocode == glot] <- ifelse(split_s, "yes", ifelse(glot %in% languagescsv$Glottocode[languagescsv$sufficient_data_indexing == 'False'], "?", "no"))
        featurestable$`Align-12.Remark`[featurestable$Glottocode == glot] <- ifelse(split_s, paste0(sort(unique(split_pred_classes)), collapse = "; "), "derived")
      }
    }
  }
  
  #Align-13: A and P indexed
  featurestable <- addDerivedState(featurestable, 'Align-13', rep(NA, nrow(featurestable)), rep(NA, nrow(featurestable)), coderslookup$Coder)
  for (glot in featurestable$Glottocode) {
    if (is.na(featurestable$`Align-06`[featurestable$Glottocode == glot])) {
      next
    }
    if (glot %in% languagescsv$Glottocode[languagescsv$sufficient_data_indexing == 'False']) {
      featurestable$`Align-13`[featurestable$Glottocode == glot] <- '?'
      featurestable$`Align-13.Source`[featurestable$Glottocode == glot] <- featurestable$`Align-06.Source`[featurestable$Glottocode == glot]
    }
    else if (featurestable$`Align-06`[featurestable$Glottocode == glot] == 'no') {
        featurestable$`Align-13`[featurestable$Glottocode == glot] <- 'NA'
        featurestable$`Align-13.Source`[featurestable$Glottocode == glot] <- featurestable$`Align-06.Source`[featurestable$Glottocode == glot]
    }
    else if (featurestable$`Align-06`[featurestable$Glottocode == glot] == 'yes') {
      a_and_p_together <- FALSE
      a_selectors <- selectorscsv$ID[selectorscsv$Glottocode == glot & selectorscsv$Marker_type == 'overt' & selectorscsv$Selector_type == 'indexing marker' & selectorscsv$A_references != 'NONE' & selectorscsv$Features != 'number' & selectorscsv$Features != 'gender' & selectorscsv$Features != 'other']
      p_selectors <- selectorscsv$ID[selectorscsv$Glottocode == glot & selectorscsv$Marker_type == 'overt' & selectorscsv$Selector_type == 'indexing marker' & selectorscsv$P_references != 'NONE' & selectorscsv$Features != 'number' & selectorscsv$Features != 'gender' & selectorscsv$Features != 'other']
      if (length(a_selectors) > 0 & length(p_selectors) > 0) {
        a_contexts <- dplyr::filter(contextscsv, Selector_ID %in% a_selectors & Role == 'A')
        p_contexts <- dplyr::filter(contextscsv, Selector_ID %in% p_selectors & Role == 'P')
        for (i in 1:nrow(a_contexts)) {
          a_row <- a_contexts[i,]
          a_reference <-ifelse(a_row$Reference == "any", "*", quotemeta(a_row$Reference))
          a_coarg <- ifelse(a_row$Co.argument_reference == "any", "*", quotemeta(a_row$Co.argument_reference))
          a_tam <- ifelse(a_row$TAM == "any", "*", quotemeta(a_row$TAM))
          a_pred <- ifelse(a_row$Predicate_class == "any", "*", quotemeta(a_row$Predicate_class))
          a_misc <- ifelse(a_row$Miscellaneous_condition == "any", "*", quotemeta(a_row$Miscellaneous_condition))
          if (length(p_contexts$ID[(grepl(a_reference, p_contexts$Co.argument_reference) | p_contexts$Co.argument_reference == 'any') & 
                        (grepl(a_tam, p_contexts$TAM) | p_contexts$TAM == 'any') & 
                        (grepl(a_pred, p_contexts$Predicate_class) | p_contexts$Predicate_class == 'any') & 
                        (grepl(a_misc, p_contexts$Miscellaneous_condition) | p_contexts$Miscellaneous_condition == 'any')]) > 0) {
            a_and_p_together <- TRUE
          }
        }
      }
      if (a_and_p_together) {
        featurestable$`Align-13`[featurestable$Glottocode == glot] <- 'yes'
      }
      else {
        featurestable$`Align-13`[featurestable$Glottocode == glot] <- 'no'
      }
    featurestable$`Align-13.Source`[featurestable$Glottocode == glot] <- featurestable$`Align-06.Source`[featurestable$Glottocode == glot]
    }
  }
  
  #Align-14: Portmanteaus
  stateCol <- ifelse(is.na(featurestable$`Align-06`), NA, 
              ifelse(featurestable$Glottocode %in% languagescsv$Glottocode[languagescsv$sufficient_data_indexing == 'False'], '?',
              ifelse(featurestable$`Align-13` != 'yes', 'NA',
              ifelse(featurestable$Glottocode %in% contextscsv$Glottocode[contextscsv$Portmanteau == 'portmanteau'],
                      'yes',
                      'no'))))
  featurestable <- addDerivedState(featurestable, 'Align-14', stateCol, featurestable$`Align-06.Source`, coderslookup$Coder)
  
  #Align-15: Non-portmanteaus co-argument sensitivity
  coarg_sensitive_flagging_lgs <- valuescsv$Glottocode[valuescsv$Parameter_ID == "coargument-sensitivity flagging" & valuescsv$Value == "yes"]
  coarg_sensitive_indexing_lgs <- valuescsv$Glottocode[valuescsv$Parameter_ID == "coargument-sensitivity indexing" & valuescsv$Value == "yes"]
  
  stateCol <- ifelse(is.na(featurestable$`Align-06`), NA, 
              ifelse(featurestable$Glottocode %in% languagescsv$Glottocode[languagescsv$sufficient_data_indexing == 'False'], '?',
              ifelse(featurestable$Glottocode %in% coarg_sensitive_flagging_lgs & featurestable$Glottocode %in% coarg_sensitive_indexing_lgs, 'both',
              ifelse(featurestable$Glottocode %in% coarg_sensitive_flagging_lgs & !(featurestable$Glottocode %in% coarg_sensitive_indexing_lgs), 'flagging',
              ifelse(!(featurestable$Glottocode %in% coarg_sensitive_flagging_lgs) & featurestable$Glottocode %in% coarg_sensitive_indexing_lgs, 'indexing', 'no')))))
  featurestable <- addDerivedState(featurestable, 'Align-15', stateCol, rep(NA, nrow(featurestable)), coderslookup$Coder)
  for (glot in featurestable$Glottocode) {
    featurestable$`Align-15.Source`[featurestable$Glottocode == glot] <- ifelse(is.na(featurestable$`Align-06`[featurestable$Glottocode == glot]), NA, valuescsv$Source[valuescsv$Parameter_ID == "coargument-sensitivity indexing" & valuescsv$Glottocode == glot])
  }
  
  #Align-15a: Coargument sensitivity anywhere
  stateCol <- ifelse(is.na(featurestable$`Align-15`), NA, 
              ifelse(featurestable$`Align-15` == '?', '?',
              ifelse(featurestable$`Align-15` == 'indexing' | featurestable$`Align-15` == 'flagging' | featurestable$`Align-15` == 'both', 'yes', 'no')))
  featurestable <- addDerivedState(featurestable, 'Align-15a', stateCol, rep(NA, nrow(featurestable)), coderslookup$Coder)
  featurestable$`Align-15a.Source` <- featurestable$`Align-15.Source`
  
  #Align-16: Direct-inverse
  coarg_sensitive_indexing_IDs <- contextscsv$Selector_ID[contextscsv$Co.argument_reference != 'any' & !is.na(contextscsv$Co.argument_reference) & contextscsv$Co.argument_reference != 'NA']
  coarg_sensitive_indexing_IDs <- coarg_sensitive_indexing_IDs[!(coarg_sensitive_indexing_IDs %in% selectorscsv$ID[!is.na(selectorscsv$Portmanteau) & selectorscsv$Portmanteau == 'portmanteau']) & coarg_sensitive_indexing_IDs %in% selectorscsv$ID[selectorscsv$Selector_type == 'indexing marker']]
  coarg_sensitive_indexing_lgs <- unique(selectorscsv$Glottocode[selectorscsv$ID %in% coarg_sensitive_indexing_IDs])
  dir_inv_lgs <- unique(contextscsv$Glottocode[contextscsv$DIR.INV == 'DIR' | contextscsv$DIR.INV == 'INV'])
  
  stateCol <- ifelse(featurestable$Glottocode %in% languagescsv$Glottocode[languagescsv$sufficient_data_indexing == 'False'], '?',
              ifelse(featurestable$Glottocode %in% featurestable$Glottocode[(featurestable$`Align-15` == 'indexing') | (derivedtable$Align.15 == 'flagging') | (featurestable$`Align-15` == 'both')],
              ifelse(featurestable$Glottocode %in% dir_inv_lgs, 'yes', 'no'), 'NA'))
  featurestable <- addDerivedState(featurestable, 'Align-16', stateCol, featurestable$`Align-06.Source`, coderslookup$Coder)
  
  #populate questions for each domain
  for (domain in list(list('Noun', 'Align.Noun', 'nouns','01a','01','02'), list('Pro', 'Align.Pro', 'pronouns','04a','04','05'), list('Verb', 'Align.Index', 'indexing','07a','07','08'))) {
    dom <- processAlignmentDomain(
      featurestable, 
      dplyr::filter(referencescsv, Domain == unlist(domain)[1]), 
      unlist(domain)[2], 
      unlist(domain)[3])
    domnames <- names(dom)
    newdomnames <- gsub(paste0(unlist(domain)[2],'.Majority'),paste0('Align-',unlist(domain)[4]),domnames)
    newdomnames <- gsub(paste0(unlist(domain)[2],'.Exemplar'),paste0('Align-',unlist(domain)[5]),newdomnames)
    newdomnames <- gsub(paste0(unlist(domain)[2],'.All'),paste0('Align-',unlist(domain)[6]),newdomnames)
    newdomnames <- gsub('.*Coder','Align.Coder', newdomnames)
    newdomnames <- gsub('.Value','',newdomnames)
    setnames(dom, old=domnames, new=newdomnames)
    # copy coder information
    for (num in domain[4:6]) {
      dom[[paste0("Align-",num,".Coder")]] <- dom[[paste0("Align.Coder")]]
    }
    featurestable <- merge(featurestable, select(dom, -c(paste0("Align.Coder"))), by=c('Glottocode'), all=TRUE)
  }
  
  #fixing not local
  for (domain in list(list('Noun', 'Align.Noun', 'nouns', '01b'), list('Pro', 'Align.Pro', 'pronouns', '04b'), list('Verb', 'Align.Index', 'indexing', '07b'))) {
    dom <- processAlignmentDomain(
      featurestable, 
      dplyr::filter(referencescsv %>% select(-c("Alignment")) %>% rename(Alignment = Alignment_not_local), Domain == unlist(domain)[1]), 
      unlist(domain)[2], 
      unlist(domain)[3])
    dom <- dplyr::select(dom, c(names(dom)[c(1:9)]))
    domnames <- names(dom)
    newdomnames <- gsub(paste0(unlist(domain)[2],'.Majority'),paste0('Align-',unlist(domain)[4]),domnames)
    newdomnames <- gsub('.*Coder','Align.Coder', newdomnames)
    newdomnames <- gsub('.Value','',newdomnames)
    setnames(dom, old=domnames, new=newdomnames)
    # copy coder information
    dom[[paste0("Align-",domain[4],".Coder")]] <- dom[[paste0("Align.Coder")]]
    # only merge the new feature
    selectors <- c('Glottocode', paste0('Align-',domain[4]), paste0('Align-',domain[4],'.Source'), paste0('Align-',domain[4],'.Remark'), paste0('Align-',domain[4],'.Coder'))
    featurestable <- merge(featurestable, select(dom, selectors), by=c('Glottocode'), all=TRUE)
  }
  
  #come back and add <?>s for align and indexing if not enough data
  for (glot in languagescsv$Glottocode[languagescsv$sufficient_data_flagging == 'False']) {
    missingsources <- paste0(sort(unique(gsub('\\[.*\\]','',c(contextscsv$Source[contextscsv$Glottocode == glot], selectorscsv$Source[selectorscsv$Glottocode == glot])))), collapse=';')
    featurestable$`Align-01a`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-01`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-04a`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-04`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-02`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-01b`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-05`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-04b`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-01a.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-01.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-04a.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-04.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-02.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-01b.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-05.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-04b.Source`[featurestable$Glottocode == glot] <- missingsources
  }
  for (glot in languagescsv$Glottocode[languagescsv$sufficient_data_indexing == 'False']) {
    missingsources <- paste0(sort(unique(gsub('\\[.*\\]','',c(contextscsv$Source[contextscsv$Glottocode == glot], selectorscsv$Source[selectorscsv$Glottocode == glot])))), collapse=';')
    featurestable$`Align-07a`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-07`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-08`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-07b`[featurestable$Glottocode == glot] <- '?'
    featurestable$`Align-07a.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-07.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-08.Source`[featurestable$Glottocode == glot] <- missingsources
    featurestable$`Align-07b.Source`[featurestable$Glottocode == glot] <- missingsources
  }
  
  #come back and add NAs for indexing if the language lacks it
  noindexing <- valuescsv$Glottocode[valuescsv$Parameter_ID == "indexing" & valuescsv$Value == "False"]
  for (glot in setdiff(valuescsv$Glottocode[valuescsv$Parameter_ID == "indexing" & valuescsv$Value == "True"], languagescsv$Glottocode[languagescsv$sufficient_data_indexing == 'False'])) {
    if (length(selectorscsv$ID[selectorscsv$Glottocode == glot & selectorscsv$Selector_type == "indexing marker" & selectorscsv$Features != "other" & selectorscsv$Features != "number" & selectorscsv$Features != "gender"]) == 0) {
      noindexing <- c(noindexing, glot)
    }
  }
  for (glot in noindexing) {
    missingsource <- valuescsv$Source[valuescsv$Parameter_ID == "indexing" & valuescsv$Glottocode == glot]
    featurestable$`Align-07a`[featurestable$Glottocode == glot] <- 'NA'
    featurestable$`Align-07`[featurestable$Glottocode == glot] <- 'NA'
    featurestable$`Align-08`[featurestable$Glottocode == glot] <- 'NA'
    featurestable$`Align-07b`[featurestable$Glottocode == glot] <- 'NA'
    featurestable$`Align-07a.Source`[featurestable$Glottocode == glot] <- missingsource
    featurestable$`Align-07.Source`[featurestable$Glottocode == glot] <- missingsource
    featurestable$`Align-08.Source`[featurestable$Glottocode == glot] <- missingsource
    featurestable$`Align-07b.Source`[featurestable$Glottocode == glot] <- missingsource
  }
  
  #come back and add NAs for pronouns if the language lacks pronouns
  nopronouns = c()
  for (glot in languagescsv$Glottocode[languagescsv$sufficient_data_flagging == "True"]) {
    selectors = selectorscsv$ID[selectorscsv$Glottocode == glot & selectorscsv$Selector_type == "flagging"]
    if (!("any" %in% contextscsv$Reference[contextscsv$Selector_ID %in% selectors]) & !any(grepl("Pro", contextscsv$Reference[contextscsv$Selector_ID %in% selectors]))) {
      nopronouns = c(nopronouns, glot)
    }
  }
  for (glot in nopronouns) {
    missingsource <- valuescsv$Source[valuescsv$Parameter_ID == "flagging" & valuescsv$Glottocode == glot]
    featurestable$`Align-04a`[featurestable$Glottocode == glot] <- 'NA'
    featurestable$`Align-04`[featurestable$Glottocode == glot] <- 'NA'
    featurestable$`Align-05`[featurestable$Glottocode == glot] <- 'NA'
    featurestable$`Align-04b`[featurestable$Glottocode == glot] <- 'NA'
    featurestable$`Align-04a.Source`[featurestable$Glottocode == glot] <- missingsource
    featurestable$`Align-04.Source`[featurestable$Glottocode == glot] <- missingsource
    featurestable$`Align-05.Source`[featurestable$Glottocode == glot] <- missingsource
    featurestable$`Align-04b.Source`[featurestable$Glottocode == glot] <- missingsource
  }
  
  return(featurestable)
}

processAlignmentDomain <- function(featurestable, intermediate, shortname, longname) {
  
  Glottocodes <- unique(intermediate$Glottocode)
  
  #generate intermediate squared table
  intermediate_squared <- intermediate %>%
    select(-ID) %>%
    group_by(Glottocode,Exemplar) %>%
    mutate(value=n()) %>%
    group_by(Glottocode,Exemplar,Alignment) %>%
    mutate(Percent=n()/value) %>%
    group_by(Glottocode,Exemplar,Alignment) %>%
    mutate(Remark=paste0(sort(unique(Referential_type)), collapse='; ')) %>%
    mutate(Percent_Remark = paste(Percent,Remark, sep='_')) %>%
    select(-c(Referential_type,S,A,P,Condition)) %>%
    distinct() %>%
    select(c(Glottocode,Language,Coder,Source,Exemplar,Alignment,Percent_Remark)) %>% 
    distinct() %>%
    pivot_wider(names_from = Alignment, values_from = Percent_Remark) %>% 
    select(c(Glottocode,Language,Coder,Source,Exemplar,POSSIBLE_ALIGNMENTS[POSSIBLE_ALIGNMENTS %in% intermediate$Alignment])) %>%
    group_by(Glottocode,Exemplar) %>%
    summarise_all(funs(gsub("NA;|;NA","",trimws(paste(unique(.),collapse=';')))))
  
  intermediate_squared$Source <- collapseSourceColumns(list(intermediate_squared$Source))
  
  for(alignment in POSSIBLE_ALIGNMENTS[POSSIBLE_ALIGNMENTS %in% intermediate$Alignment]) {
    intermediate_squared <- separate(intermediate_squared, col=alignment, into=c(alignment, paste0(alignment,'_Remark')),sep='_',remove=TRUE)
    intermediate_squared[[alignment]] <- ifelse(intermediate_squared[[alignment]] == 'NA', 0, intermediate_squared[[alignment]])
    intermediate_squared[[paste0(alignment, '_Remark')]] <- ifelse(intermediate_squared[[paste0(alignment, '_Remark')]] == 'NA', 0, intermediate_squared[[paste0(alignment, '_Remark')]])
  }
  
  names(intermediate_squared) <- make.names(names(intermediate_squared))
  
  # generate questionnaire --> we will use this to turn everything into a readable cldf & long format
  # ALIGN.QUESTIONS is a lookup table for the question name, the internal name I use to generate the questions below, and the external name (XAlign.01a, etc)
  ALIGN.QUESTIONS <- data.frame(matrix(ncol=2,nrow=0))
  names(ALIGN.QUESTIONS) <- c('question','internal.key')
  if (longname == 'indexing' | longname == 'pronouns') {
    ALIGN.QUESTIONS[nrow(ALIGN.QUESTIONS)+1,] <- c(paste0('What is the simple majority alignment in exemplar scenarios of ', longname, '? { NA | ', paste(POSSIBLE_ALIGNMENTS, collapse=' | '), ' | no majority }'),paste0(shortname,'.Majority'))
    ALIGN.QUESTIONS[nrow(ALIGN.QUESTIONS)+1,] <- c(paste0('What are the exemplar alignments of ', longname, '? { ; separated list of [ ', paste(paste0(POSSIBLE_ALIGNMENTS,':_frequency_'), collapse=' , '), ' ] | NA }'),paste0(shortname,'.Exemplar'))
    ALIGN.QUESTIONS[nrow(ALIGN.QUESTIONS)+1,] <- c(paste0('What are all the alignments of ', longname, '? { ; separated list of [ ', paste(paste0(POSSIBLE_ALIGNMENTS,':_frequency_'), collapse=' , '), ' ] | NA }'),paste0(shortname,'.All'))
  } else {
    ALIGN.QUESTIONS[nrow(ALIGN.QUESTIONS)+1,] <- c(paste0('What is the simple majority alignment in exemplar scenarios of ', longname, '? { ', paste(POSSIBLE_ALIGNMENTS, collapse=' | '), ' | no majority }'),paste0(shortname,'.Majority'))
    ALIGN.QUESTIONS[nrow(ALIGN.QUESTIONS)+1,] <- c(paste0('What are the exemplar alignments of ', longname, '? { ; separated list of [ ', paste(paste0(POSSIBLE_ALIGNMENTS,':_frequency_'), collapse=' , '), ' ] }'),paste0(shortname,'.Exemplar'))
    ALIGN.QUESTIONS[nrow(ALIGN.QUESTIONS)+1,] <- c(paste0('What are all the alignments of ', longname, '? { ; separated list of [ ', paste(paste0(POSSIBLE_ALIGNMENTS,':_frequency_'), collapse=' , '), ' ] }'),paste0(shortname,'.All'))
  }  
  coalesce_by_column <- function(df) {
    return(dplyr::coalesce(!!! as.list(df)))
  }
  
  # collapse and take the mean
  all_percentages <- intermediate_squared
  all_percentages$Source <- collapseSourceColumns(list(all_percentages$Source))
  
  # generate every combination of glottocode and exemplar (primary, secondary only) and affiliate these with coder, source
  codersource <- intermediate %>% 
    dplyr::select(Glottocode, Language,Exemplar, Coder, Source) %>% 
    rename(Exemplar = Exemplar) %>%
    distinct()
  glotexemplar <- data.frame(all_percentages) %>% 
    expand(Glottocode, Exemplar)
  for (glot in Glottocodes) {
    glotexemplar$Language[glotexemplar$Glottocode == glot] <- unique(codersource$Language[codersource$Glottocode == glot])
    glotexemplar$Coder[glotexemplar$Glottocode == glot] <- unique(codersource$Coder[codersource$Glottocode == glot])
    primary_source <- paste0(collapseSources(unique(codersource$Source[codersource$Glottocode == glot & grepl('exemplar', codersource$Exemplar)])), collapse=';')
    secondary_source <- paste0(collapseSources(unique(codersource$Source[codersource$Glottocode == glot & grepl('all', codersource$Exemplar)])), collapse=';')
    glotexemplar$Source[glotexemplar$Glottocode == glot & glotexemplar$Exemplar == 'exemplar'] <- primary_source
    glotexemplar$Source[glotexemplar$Glottocode == glot & glotexemplar$Exemplar == 'all'] <- ifelse(secondary_source == "NA", primary_source, secondary_source)
  }
  
  # Add in all glotexemplar combinations: Uncoded things get <NA>, which I turn into "NA"
  for(align in POSSIBLE_ALIGNMENTS_DOTS[POSSIBLE_ALIGNMENTS_DOTS %in% names(all_percentages)]) {
    all_percentages[[align]] <- ifelse(is.na(all_percentages[[align]]), "NA", all_percentages[[align]])
  }
  
  #make the CLDF format
  #first make the table even wider by expanding sources
  for (align in POSSIBLE_ALIGNMENTS_DOTS[POSSIBLE_ALIGNMENTS_DOTS %in% names(all_percentages)]) {
    all_percentages[[paste0(align, '_Source')]] <- all_percentages$Source
  }
  
  #now get rid of the original source, add _value, then pivot_longer
  is_melt <- all_percentages %>% 
    select(-c('Source')) %>%
    mutate(across(everything(),as.character)) %>%
    rename_with(.cols = matches(paste0('(',paste0(POSSIBLE_ALIGNMENTS_DOTS, collapse='|'),')$')), ~paste0(.x, "_Value")) %>% 
    pivot_wider(names_from = Exemplar, values_from = 5:(length(POSSIBLE_ALIGNMENTS_DOTS[POSSIBLE_ALIGNMENTS_DOTS %in% names(all_percentages)])*3+5-1), names_glue = "{Exemplar}.{.value}") %>%
    summarise_all(coalesce_by_column) %>% 
    pivot_longer(-c('Glottocode', 'Language','Coder'), names_sep='_', names_to=c('ParameterID', '.value'))
  
  # get the most frequent primary alignment per language
  top_alignments <- is_melt[!is.na(as.numeric(is_melt$Value)),] %>%
    subset(grepl('exemplar',ParameterID)) %>% 
    group_by(Glottocode) %>% 
    top_n(1, Value)
 
  #collapse alignments per exemplar case
  is_melt_new <- data.frame(matrix(ncol=length(colnames(is_melt)),nrow=0))
  names(is_melt_new) <- names(is_melt)
  for (glot in Glottocodes) {
    for (exemplar in c('exemplar','all')) {
      aligns <- gsub('\\.', ' ', gsub(paste0(exemplar,'.'),'',is_melt$ParameterID[is_melt$Glottocode == glot & grepl(exemplar, is_melt$ParameterID)]))
      freqs <- is_melt$Value[is_melt$Glottocode == glot & grepl(exemplar, is_melt$ParameterID)]
      remarks <- gsub(';',',',is_melt$Remark[is_melt$Glottocode == glot & grepl(exemplar, is_melt$ParameterID)])
      if (length(unique(freqs)) == 1 && unique(freqs) == 'NA') {
        new_value <- 'NA'
        new_remark <- NA
      }
      else {
        new_value <- paste0(aligns,':',round(as.numeric(freqs), 4))
        new_value <- paste0(new_value[!grepl(':0$',new_value)], collapse=';;')
        new_remark <- paste0(aligns,':',remarks)
        new_remark <- paste0(new_remark[!grepl(':NA',new_remark)], collapse=';;')
      }
      language <- unique(is_melt$Language[is_melt$Glottocode == glot & grepl(exemplar, is_melt$ParameterID)])
      source <- unique(is_melt$Source[is_melt$Glottocode == glot & grepl(exemplar, is_melt$ParameterID)])
      coder <- unique(is_melt$Coder[is_melt$Glottocode == glot & grepl(exemplar, is_melt$ParameterID)])
      is_melt_new[nrow(is_melt_new)+1,] <- 
        c(glot,
          language, 
          coder,
          ifelse(exemplar=='exemplar','Align.Exemplar','Align.All'),
          new_value,
          new_remark,
          source
        )
    }
  }
  is_melt <- is_melt_new
  
  # add in exemplar majority alignment
  additional_questions <- data.frame(matrix(ncol=7,nrow=0))
  colnames(additional_questions) <- colnames(is_melt)
  for (glot in Glottocodes) {
    glottbl <- top_alignments[which(top_alignments$Glottocode == glot),]
    additional_questions[nrow(additional_questions)+1, ] <- 
      c(glot, 
        unique(is_melt$Language[is_melt$Glottocode == glot]),
        paste0(unique(is_melt[which(is_melt$Glottocode == glot),]$Coder),collapse=';'),
        'Align.Majority', 
        ifelse(!(glot %in% unique(top_alignments$Glottocode)), 'NA',
          ifelse(nrow(glottbl) == 1, 
             gsub('\\.',' ', sub('exemplar.','',as.character(top_alignments$ParameterID[top_alignments$Glottocode == glot]))), 
             'no majority')), 
        ifelse(!(glot %in% unique(top_alignments$Glottocode)), 'derived',
          paste0(sub('exemplar ','',sort(paste0(gsub('\\.',' ',glottbl$ParameterID), ":", round(as.numeric(glottbl$Value), 4)))), collapse=', ')),
        paste0(sort(collapseSources(unique(is_melt[which(is_melt$Glottocode == glot),]$Source))),collapse=';'))
  }
  is_melt <- rbind(is_melt, additional_questions)
  
  #unmelt to get the human readable
  wider <- is_melt %>% 
    pivot_wider(names_from = ParameterID, values_from = c(Value, Remark, Source), names_glue = '{ParameterID}.{.value}')
  
  #rename wider output
  oldnames <- names(wider)
  newnames <- gsub('Align',shortname,oldnames)
  newnames <- gsub('Coder',paste0(shortname,'.Coder'),newnames)
  setnames(wider, old=oldnames, new=newnames)

  #reorder nicely and add questions
  niceorder <- c('Glottocode',paste0(shortname,'.Coder'))
  for (x in ALIGN.QUESTIONS$internal.key) {
    niceorder <- c(niceorder, paste0(x,'.Value'), paste0(x,'.Remark'), paste0(x,'.Source'))
  }
  wider <- select(wider, niceorder)
  
  return(wider)
}