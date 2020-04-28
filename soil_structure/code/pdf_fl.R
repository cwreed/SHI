library(BibScan)
library(rvest)
library(jsonlite)


article_pdf_download(infilepath = "data/bibs", 
                     outfilepath = "data/pdfs",
                     colandr = "data/Soil physical properties, SOC, and management.csv")
warning