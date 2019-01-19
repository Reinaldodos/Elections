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
	to <- xpathSApply(to,'//*[@id="general_election_data-constituency_result_headline"]/div/span[1]', xmlValue)
	result <- rbind(result,to)
}

final <- cbind(const,result)