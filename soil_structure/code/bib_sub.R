install.packages("bibtex")
library(bibtex)

test <- read.bib("data/bibs/savedrecs-3.bib", package = "base")
test2 <- read.bib(file = "data/bibs/savedrecs-4.bib", package = "base")
filtered <- read.csv(file = "data/Soil physical properties, SOC, and management.csv")

test


out1 <- test[which(gsub("[\n]|\\{|\\}|\\  ", "", test$title) %in% filtered$citation_title)]
out2 <- test2[which(gsub("[\n]|\\{|\\}|\\  ", "", test2$title) %in% filtered$citation_title)]

write.bib(out1, file = "filtered1.bib")
write.bib(out2, file = "filtered2.bib")
