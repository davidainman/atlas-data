newSpreadsheet <- read.csv('constructions.csv', na.strings = c(), colClasses = c("character"), stringsAsFactors = FALSE)
newSpreadsheet$Coder <- gsub(";$","",paste(newSpreadsheet$Coder, newSpreadsheet$Construction_Form_Coder, sep=";"))
newSpreadsheet <- subset(newSpreadsheet, select = -c(Construction_Form_Coder))
write.csv(newSpreadsheet, 'constructions.csv', na="", row.names=FALSE)