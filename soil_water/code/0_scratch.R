

test <- melt(d.raw, measure.vars = c("Data type", "Value"))

test2 <- recast(test, formula = variable ~ .)

test2 <- dcast(d.raw, Paper+`DOI/ISSN`+`Year of observation`+
                 `Treatment 1`+`Treatment 2`+`Treatment 3`+
                 `Treatment 4`+ `Treatment 5`+`Method/Site`+Depth ~ `Data type`)


papers.to.check <- unique(d.raw[!is.na(d.raw$`Method/Site`),]$Paper)

unique(d.ir$Paper)


d.ucs <- read.csv("data/IR_meta.csv")

unique(d.ucs$Reference)

test <- d.ucs[d.ucs$Reference %in% d.soc$Paper,]
unique(test$Reference)

base::setdiff(d.soc$Paper, d.ir$Paper)

unique(d.soc$Paper)



names(d)
d.trts <- distinct(d[,c(1,11)])

write.xlsx(d.trts, "data/d.trts.xlsx")

## convert to same IR units

d <- d %>%
  mutate(IR.converted = case_when(
    IR.units == "mm/h"|IR.units == "mm/hr" ~ IR,
    IR.units == "cm/h"| IR.units == "cm/hr" ~ IR*10,
    IR.units == "ln(cm/h)" ~ (exp(IR)*10),
    IR.units == "cm/d" ~ IR/24*10,
    IR.units == "cm/min" ~ IR*600))

d %>%
  filter(IR.units == "mm/h"|IR.units == "mm/hr"|IR.units == "cm/h"| IR.units == "cm/hr"|
           IR.units == "ln(cm/h)"|IR.units == "cm/d"|IR.units == "cm/min") -> test

write.xlsx(test, "test.xlsx")

View(d$IR.converted)


