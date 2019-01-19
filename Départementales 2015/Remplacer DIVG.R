library(XML)
results <- read.table("Results.txt") 
data <- results
results$Nuances <- as.character.factor (results$Nuances)

'trancher les "BC-DVG'

'Si il y a un BC-SOC ou un BC-UG sur le canton : "AltG"
Sinon "UG"'

'Créer un fichier des cantons concernés'

zouzou <- function(popo)
{
	DIVG <- subset(results, results$Nuances == popo)

	GOSH <- subset(results, results$Nuances %in% c("BC-SOC", "BC-UG"))
	GOSH <- subset(GOSH, select=c(Canton))
	GOSH <- unique(GOSH)

Alter <- merge(DIVG, GOSH, by = "Canton")

'remplacer DIVG par ALtG dans Alter'


Alter$Nuances[Alter$Nuances == popo] <- "AltG"

results$Nuances <- replace(results$Nuances, results$Binômes.de.candidats %in% Alter$Binômes.de.candidats, Alter$Nuances)
}

zouzou("BC-DVG")


results$Nuances[results$Nuances == "BC-UD"] <- "Droite"
results$Nuances[results$Nuances == "BC-UG"] <- "Gauche"
results$Nuances[results$Nuances == "BC-FN"] <- "FN"
results$Nuances[results$Nuances == "BC-COM"] <- "AltG"
results$Nuances[results$Nuances == "BC-VEC"] <- "AltG"
results$Nuances[results$Nuances == "BC-DVD"] <- "Droite"
results$Nuances[results$Nuances == "BC-UMP"] <- "Droite"
results$Nuances[results$Nuances == "BC-DIV"] <- "WTF"
results$Nuances[results$Nuances == "BC-EXG"] <- "AltG"
results$Nuances[results$Nuances == "BC-UDI"] <- "Droite"
results$Nuances[results$Nuances == "BC-SOC"] <- "Gauche"
results$Nuances[results$Nuances == "BC-FG"] <- "AltG"
results$Nuances[results$Nuances == "BC-DVG"] <- "Gauche"
results$Nuances[results$Nuances == "BC-RDG"] <- "Gauche"
results$Nuances[results$Nuances == "BC-EXD"] <- "FN"
results$Nuances[results$Nuances == "BC-DLF"] <- "FN"
results$Nuances[results$Nuances == "BC-MDM"] <- "Droite"
results$Nuances[results$Nuances == "BC-PG"] <- "AltG"
results$Nuances[results$Nuances == "BC-UC"] <- "Droite"

results$Voix <- as.numeric(results$Voix)
bilan <- aggregate(results$Voix, list(results$Nuances), sum)
bilan <- bilan[order(bilan$x,decreasing=TRUE),]