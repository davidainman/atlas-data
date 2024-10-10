source('utils.R')

processVeluv <- function(featurestable, lgnames, warnings, errors) {
  
  #VelUv-01a
  stateCol <- ifelse((featurestable$`VelUv-01` == '?' & featurestable$`VelUv-02` != 'yes') | (featurestable$`VelUv-01` != 'yes' & featurestable$`VelUv-02` == '?'), '?', 
              ifelse(featurestable$`VelUv-01` == 'yes' | featurestable$`VelUv-02` == 'yes', 'yes', 
              ifelse(featurestable$`VelUv-01` == 'no' & featurestable$`VelUv-02` == 'no', 'no', 'ERROR: Impossible state!')))
  sourceCol <- collapseSourceColumns(list(featurestable$`VelUv-01.Source`, featurestable$`VelUv-02.Source`))
  coderCol <- sapply(paste0(featurestable$`VelUv-01.Coder`, ';', featurestable$`VelUv-02.Coder`), collapseCoders)
  featurestable <- addDerivedState(featurestable, 'VelUv-01a', stateCol, sourceCol, coderCol)
  
  #VelUv-02a
  stateCol <- ifelse(featurestable$`VelUv-01a` == '?' | featurestable$`VelUv-02` == '?', '?', 
              ifelse(featurestable$`VelUv-01` == 'yes' & featurestable$`VelUv-02` == 'yes', 'yes', 
              ifelse (featurestable$`VelUv-01` == 'yes' & featurestable$`VelUv-02` == 'no', 'no', 'NA')))
  sourceCol <- featurestable$`VelUv-02.Source`
  coderCol  <- featurestable$`VelUv-02.Coder`
  featurestable <- addDerivedState(featurestable, 'VelUv-02a', stateCol, sourceCol, coderCol)
  
  return(featurestable)
}
