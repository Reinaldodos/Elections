setwd("~/Dropbox/Carto & Stats/R/Elections/Régionales 2015")

# Parser le site du Ministère

# récupérer les pages des candidats
library(XML)
url <- "http://elections.interieur.gouv.fr/regionales-2015/"
toto <- htmlParse(paste(url,"11/11.html", sep=""))
REGIONS <- xpathSApply(toto,'//*[@class="offset2 span8"]/a', xmlValue)
titi <- xpathSApply(toto,'//*[@class="offset2 span8"]/a', xmlGetAttr, name = "href")
REGIONS <- cbind(REGIONS,titi)
# REGIONS <- as.data.frame(REGIONS)

result <- NULL

for(n in 1:nrow(REGIONS))
{
  region <- REGIONS[n,1]
  show(region)
  newurl <- paste(url, substr(REGIONS[n,2], 4, nchar(REGIONS[n,2])), sep="")
  toto <- htmlParse(newurl)
  tata <- readHTMLTable(toto, stringsAsFactors = FALSE)
  tata <- tata[2]
  tata <- as.data.frame(tata)
  toto <- merge(as.data.frame(region), as.data.frame(tata))
  result <- rbind(result, toto)
}

resultats <- as.data.frame(result)
colnames(resultats) <- c("Région", "Candidat", "Nuances", "Voix", "Inscrits", "Exprimés")
resultats$Exprimés <- as.double(gsub(",", ".", result$NULL...Exprimés))

library(sqldf)
resultats <- sqldf('
select Région, Nuances, Exprimés
from resultats
GROUP BY Région, Nuances
                   ')

resultats <- resultats[order(resultats$Région, -resultats$Exprimés),]

library(tidyr)
toto <- subset(resultats, select = c("Région", "Nuances", "Exprimés"))
tap <- spread(toto, key=Nuances, value=Exprimés, fill="", drop=TRUE)