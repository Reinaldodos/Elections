setwd("~/Dropbox/Carto & Stats/R/Elections/Régionales 2015")

# Parser le site du Ministère

# récupérer les pages des candidats
library(XML)
url <- "http://elections.interieur.gouv.fr/regionales-2015/"
toto <- htmlParse(url)
REGIONS <- xpathSApply(toto,'//*[@id="listeRG"]/option', xmlValue)
titi <- xpathSApply(toto,'//*[@id="listeRG"]/option', xmlGetAttr, name = "value")
REGIONS <- cbind(REGIONS,titi)
# REGIONS <- as.data.frame(REGIONS)

result <- NULL
for(n in 2:nrow(REGIONS))
{
  region <- REGIONS[n,1]
  show(region)
  newurl <- paste(url, REGIONS[n,2], sep="")
  toto <- htmlParse(newurl)
  tata <- readHTMLTable(toto, stringsAsFactors = FALSE)
  tata <- tata[2]
  tata <- as.data.frame(tata)
  toto <- merge(as.data.frame(region), as.data.frame(tata))
  result <- rbind(result, toto)
}

resultats <- as.data.frame(result)
colnames(resultats) <- c("Région", "Candidat", "Nuances", "Voix", "Inscrits", "Exprimés", "Sièges")
resultats$Exprimés <- as.double(gsub(",", ".", result$NULL...Exprimés))

library(sqldf)
resultats <- sqldf('
select Région, Nuances, Exprimés, Sièges
from resultats
GROUP BY Région, Nuances
                   ')

resultats <- resultats[order(resultats$Région, -resultats$Exprimés),]

library(tidyr)
toto <- subset(resultats, select = c("Région", "Nuances", "Sièges"), Sièges!="")
tap <- spread(toto, key=Nuances, value=Sièges, fill="", drop=TRUE)

write.csv(resultats, "Deuxième tour.csv", row.names = FALSE)
