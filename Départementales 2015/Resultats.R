library(XML)

'data <- NULL'
error <- NULL

liens <- read.csv("Liens.csv")

'récupérer le tableau des résultats'
recup <- function(tatat) 
{
	cla <- readHTMLTable(tatat, stringsAsFactors = FALSE)
	cla <- cla[[2]]
	
	cal <- htmlParse(tatat)
	Canton <- xpathSApply(cal, "//div[@class='offset2 span8']/h3", xmlValue)
	Canton <- Canton[1]
	
	show(Canton)
	
	lieu <- strsplit(Canton, " - ")
	lieu <- data.frame(lieu)
	bou <- t(lieu)
	colnames(bou) <- c("Département", "Canton")
	rownames(bou) <- NULL
	
	
	nr <- nrow(cla)
	to <- NULL
	for(n in 1:nr)
	{
		to <- rbind(to, bou)
	}
	result <- cbind(cla, to)
	result <- data.frame(result)
	return(result)
}


'BOUCLE DE RECUP'
repeat
{
	'faire tourner la Recup'
	source("Recup.R")
	
	'liens <- error'
	error <- NULL
	if(is.null(liens))
	{
		break
	}
}
