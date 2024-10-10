valuescsv <- read.csv("~/Documents/GitHub/ooa-processing/cldf/values.csv", na.strings = c(), colClasses = c("character"), stringsAsFactors = FALSE)
languagescsv <- read.csv("~/Documents/GitHub/ooa-processing/cldf/languages.csv", na.strings = c(), stringsAsFactors = FALSE)

# make isolates their own families
languagescsv$Family_Name <- ifelse(languagescsv$Family_Name == "", languagescsv$Name, languagescsv$Family_Name)
  
  freqs <- list(
    Australia = c(),
    Africa = c(),
    Eurasia = c(),
    Papunesia = c(),
    North.America = c(),
    South.America = c(),
    Global = c()
  )
  
  for (i in seq(1,250)) {
    lg_sample <- c()
    for (macroarea in unique(languagescsv$Macroarea)) {
      m_filter <- dplyr::filter(languagescsv, Macroarea == macroarea)
      # randomly pick 15 languages from different families
      family_sample <- sample(m_filter$Family_Name, 15)
      m_sample <- c()
      for (fs in family_sample) {
        m_sample <- c(m_sample, sample(dplyr::filter(m_filter, Family_Name == fs)$Glottocode, 1))
      }
      m_freq = nrow(dplyr::filter(valuescsv, LanguageID %in% m_sample, ParameterID == "SgPl-01", Value == "yes")) / nrow(dplyr::filter(valuescsv, LanguageID %in% m_sample, ParameterID == "SgPl-01", Value != "?"))
      freqs[[gsub(" ", ".", macroarea)]] <- c(freqs[[gsub(" ", ".", macroarea)]], m_freq)
      lg_sample <- c(lg_sample, m_sample)
    }
    g_freq = nrow(dplyr::filter(valuescsv, LanguageID %in% lg_sample, ParameterID == "SgPl-01", Value == "yes")) / nrow(dplyr::filter(valuescsv, LanguageID %in% lg_sample, ParameterID == "SgPl-01", Value != "?"))
    freqs[["Global"]] <- c(freqs[["Global"]], g_freq)
  }
  
  plot(density(freqs[["Australia"]]))
  plot(density(freqs[["Africa"]]))
  plot(density(freqs[["North.America"]]))
  plot(density(freqs[["South.America"]]))
  plot(density(freqs[["Papunesia"]]))
  plot(density(freqs[["Eurasia"]]))
  
  mean(freqs[["Australia"]])
  sd(freqs[["Australia"]])
  mean(freqs[["Africa"]])
  sd(freqs[["Africa"]])
  mean(freqs[["Papunesia"]])
  sd(freqs[["Papunesia"]])
  mean(freqs[["Eurasia"]])
  sd(freqs[["Eurasia"]])
  mean(freqs[["North.America"]])
  sd(freqs[["North.America"]])
  mean(freqs[["South.America"]])
  sd(freqs[["South.America"]])
  mean(freqs[["Global"]])
  sd(freqs[["Global"]])
