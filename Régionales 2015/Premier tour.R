setwd("~/Dropbox/Carto & Stats/R/Elections/Régionales 2015")

# Parser le site du Ministère

# récupérer les pages des candidats
library(XML)
url <- "http://elections.interieur.gouv.fr/regionales-2015/"
toto <- htmlParse(url)
REGIONS <- xpathSApply(toto,'//*[@id="listeRG"]/option', xmlValue)
titi <- xpathSApply(toto,'//*[@id="listeRG"]/option', xmlGetAttr, name = "value")
REGIONS <- cbind(REGIONS,titi)

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

library(tidyr)

toto <- subset(resultats, select = c("Région", "Candidat", "Nuances", "Exprimés"))
tap <- spread(toto, key=Nuances, value=Exprimés, fill="0", drop=TRUE)


