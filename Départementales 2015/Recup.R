'INITIALISER LA MATRICE DATA'

if(is.null(data))

{
	n<-1
	l <- liens[n,]
	tatat <- toString(l)
	data <- merge(data, recup(tatat), all=TRUE)
}


for (n in 1:nrow(liens)) 
{
	tmp <- NULL
	l <- liens[n,]
	tatat <- toString(l)
	try(tmp <- recup(tatat))
  if (is.null(tmp)) 
  {
    error <- rbind(error, tatat)
    rownames(error) <- NULL
  } 
  else 
  {
  	data <- merge(data, recup(tatat), all=TRUE)
  }
  data <- data[data$Elus == "Oui",]
 
  'lo <- subset(data,select=c("Département", "Canton"))
  lo <- unique(lo)
  
  lo <- lo[order(lo$Canton),]
  lo <- lo[order(lo$Département),]
   
  show(lo[nrow(lo),])'
}

elus <- data

toto <- table(elus$Département, elus$Nuances)

results <- data.frame(toto)
colnames(results) <- c("Département", "Nuance", "Elus")

toto <- as.data.frame.matrix(toto)
dep <- rownames(toto)

write.table(toto, "Résultats définitifs.txt")
write.table(elus, "Elus par conton.txt")

