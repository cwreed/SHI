sapply(list.files("packrat/lib/x86_64-apple-darwin15.6.0/3.6.1/"), require, character.only = T)

gs_ls()
d.raw <- gs_title("UCS_SOM_Data")
gs_ws_ls(d.raw)
d.raw <- gs_read(ss=d.raw, ws = "Sheet1")

unique(d.raw$`Data type`)

#write.xlsx(d.raw,"data/UCS_data.xlsx")
names(d.raw)[1] <- "Paper"

## Filter and clean SOC data

d.raw %>%
  filter(grepl("OC|OM",`Data type`)) -> d.soc

d.soc[d.soc$`Data type` %in% "OM", "Value"] <- d.soc[d.soc$`Data type` %in% "OM", "Value"]*0.58

d.soc[d.soc$Units %in% "%", "Value"] <- d.soc[d.soc$Units %in% "%", "Value"]/.1

## Liebig et al. paper has OC values in Mg/ha, need to figure out how to convert
d.soc <- d.soc[-which(d.soc$Units %in% "Mg/ha"),]

names(d.soc)[15] <- "SOC.g.kg"


paste.drop.NA <- function(x, sep = ", ") {
  x <- gsub("^\\s+|\\s+$", "", x) 
  ret <- paste(x[!is.na(x) & !(x %in% "")], collapse = sep)
  is.na(ret) <- ret == ""
  return(ret)
}

d.soc$Trt.combo <- apply(d.soc[,4:10], 1, paste.drop.NA)

d.soc <- d.soc[,c(1:10,16,11,14,15)]

unique(d.soc$Paper)
## Filter and clean IR data

unique(d.raw$`Data type`)

d.raw %>%
  filter(grepl("IR",`Data type`)) -> d.ir

unique(d.ir$Units)



#d.ir <- d.ir %>%
 # mutate(
  #  Value = case_when(
   #   Units == "cm/h"| Units == "cm/hr" ~ Value*10,
    #  Units == "mm/d" ~ Value/24,
     # Units == "cm/min" ~ Value*600,
      #Units == "mm/s" ~ Value*3600,
      #Units == "mm/min" ~ Value*60,
      #Units == "mm/h"|Units == "mm/hr" ~ Value))
#d.ir$Value <- round(d.ir$Value, 4)

d.soc$Paper[d.soc$Paper %in% d.ir$Paper]

names(d.ir)
d.soc %>%
  left_join(d.ir[,c(1,3,4:11,13,15)]) %>%
  rename(.,c("Units"="IR.units",
             "Value"="IR")) -> d


## BD

d.raw %>% 
  filter(grepl("BD",`Data type`)) -> d.bd

names(d.bd)
d %>%
  left_join(d.bd[,c(1,3,4:11,13:15)]) %>%
  rename(c("Units"="BD.units",
             "Value"="BD")) -> d

d.raw %>% 
  filter(grepl("PR",`Data type`)) -> d.pr

d %>%
  left_join(d.pr[,c(1,3,4:11,13:15)]) %>%
  rename(c("Units"="PR.units",
           "Value"="PR")) -> d


## Import 


write.xlsx(d, "data/data_export.xlsx")
write.xlsx(d.soc,"soc_papers.xlsx" )
write.xlsx(d.ir,"IR_papers.xlsx" )

