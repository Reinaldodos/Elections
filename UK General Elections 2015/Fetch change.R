library(XML)
url <- "http://www.bbc.com/news/politics/constituencies"
to <- htmlParse(url)
constituency <- xpathSApply(to,'//th/a', xmlValue)
ti <- xpathSApply(to,'//th/a', xmlGetAttr, name = "href")
const <- cbind(constituency,ti)

result <- NULL

for(n in 1:nrow(const))
{
	show(n)	
	url <- paste("http://www.bbc.com", const[n,2], sep="")	
	to <- htmlParse(url)
	changes <- xpathSApply(to,'//li[@class="party__result--votesnet essential"]/span[1]', xmlValue)
	parties <- xpathSApply(to,'//div[@id="general_election_data-constituency_result_table"]/div/div[3]/div/div/div[1]', xmlValue)
	
	change <- cbind(parties, changes)
	consti <- consti <- const[n,1]
	change <- merge(consti, change, all=TRUE)
	result <- merge(result, change, all=TRUE)
	result <- merge(result, change, all=TRUE)
	result$changes <- as.numeric(as.character(result$changes))
}

gov <- subset(result, parties %in% c("Conservative", "Liberal Democrat"))
dude <- aggregate(gov$changes, by = list(constituency = gov$x), sum)

lab <- subset(result, parties %in% c("Labour"))

cons <- subset(result, parties %in% c("Conservative"))

ukip <- subset(result, parties %in% c("UKIP"))

snp <- subset(result, parties %in% c("Scottish National Party"))

droite <- subset(result, parties %in% c("Conservative", "Liberal Democrat", "UKIP"))
droite <- aggregate(droite$changes, by = list(constituency = droite$x), sum)

left <- subset(result, parties %in% c("Labour", "Scottish National Party"))
left <- aggregate(left$changes, by = list(constituency = left$x), sum)

png(filename = "outfile.png", width = 800, height = 500); par(mfcol = c(1,3)) ; barplot(sort(dude$x));barplot(sort(left$x));barplot(sort(ukip$changes));dev.off() 