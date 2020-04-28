sapply(list.files("packrat/lib/x86_64-apple-darwin15.6.0/3.6.1//"), require, character.only = T)
detach("package:plyr")

d <- read.xlsx("data/data_export.xlsx")
d <- d[,-c(4:10)]

## Summarize carbon data
d <- d %>%
  dplyr::group_by(Paper) %>%
  filter(Year.of.observation == max(Year.of.observation)) %>%
  filter(!is.na(IR)) %>%
  separate(Depth, into = c("Top.depth", "Bottom.depth"), sep = "-", remove = F) %>%
  mutate(Depth.increment=as.numeric(Bottom.depth) - as.numeric(Top.depth)) %>%
  do(mutate(., max.bottom = as.numeric(max(as.numeric((Bottom.depth)))))) %>%
  mutate(Depth.proportion =as.numeric(Depth.increment)/max.bottom) %>%
  ungroup(.)

d$Depth.proportion
d[is.na(d$Depth.proportion), "Depth.proportion"] <- 1

d.soc <- d %>%
  group_by(Paper, Trt.combo, `Method/Site`, Depth) %>%
  mutate(SOC.g.kg.weighted = Depth.proportion*SOC.g.kg) %>%
  group_by(Paper, Trt.combo, `Method/Site`) %>%
  dplyr::summarise(SOC.g.kg.weighted =  sum(SOC.g.kg.weighted))

names(d)
d <- distinct(d[,c(1,2,4,5,10,11)]) %>%
  left_join(d.soc)

d.trts <- read.xlsx("data/d.trts.xlsx")

d <- d %>%
  left_join(d.trts) %>%
  distinct(.)

# fix variables in d

d <- d %>%
  mutate(IR.units = case_when(IR.units == "None" ~ "cm/h",
                              IR.units == "cm/hr" ~ "cm/h",
                              IR.units == "mm/hr" ~ "mm/h",
                              TRUE ~ .$IR.units),
         IR = case_when(IR.units == "mm/h" ~ IR/10,
                        IR.units == "ln(cm/h)" ~ exp(IR),
                        TRUE ~ as.numeric(.$IR))) %>%
  mutate(IR.units = case_when(IR.units == "mm/h" ~ "cm/h",
                       IR.units == "ln(cm/h)" ~ "cm/h",
                       TRUE ~ .$IR.units))


## Calculate LRR for IR within each treatment
names(d)


d %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(Tillage == "n") %>%
  summarise(Tillage.IR.control = mean(IR)) -> d.till.control
  
d %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(Cover == "n") %>%
  summarise(Cover.IR.control = mean(IR)) -> d.cover.control

d %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(Org.amend == "n") %>%
  summarise(Org.amend.IR.control = mean(IR)) -> d.org.amend.control


d %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(Tillage == "n") %>%
  summarise(Tillage.SOC.control = mean(SOC.g.kg.weighted)) -> d.till.soc.control

d %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(Cover == "n") %>%
  summarise(Cover.SOC.control = mean(SOC.g.kg.weighted)) -> d.cover.soc.control

d %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(Org.amend == "n") %>%
  summarise(Org.amend.SOC.control = mean(SOC.g.kg.weighted)) -> d.org.amend.soc.control


d <- d %>%
  ungroup(.) %>%
  left_join(d.till.control) %>%
  left_join(d.cover.control) %>%
  left_join(d.org.amend.control) %>%
  left_join(d.till.soc.control) %>%
  left_join(d.cover.soc.control) %>%
  left_join(d.org.amend.soc.control) %>%
  mutate(
    Tillage.IR.LRR = log(IR/Tillage.IR.control),
    Cover.IR.LRR = log(IR/Cover.IR.control),
    Org.amened.IR.LRR = log(IR/Org.amend.IR.control),
    Tillage.SOC.LRR = log(SOC.g.kg.weighted/Tillage.SOC.control),
    Cover.SOC.LRR = log(SOC.g.kg.weighted/Cover.SOC.control),
    Org.amened.SOC.LRR = log(SOC.g.kg.weighted/Org.amend.SOC.control))

# Models


summary(lme(IR ~ SOC.g.kg.weighted, random = ~1|Paper, data = d))

summary(lme(IR ~ SOC.g.kg.weighted+Tillage+Cover+Org.amend, 
            random = ~1|Paper, data = d, na.action = "na.omit"))

summary(lme(IR ~ SOC.g.kg.weighted*Cover, 
            random = ~1|Paper, data = d, na.action = "na.omit"))

summary(lme(IR ~ SOC.g.kg.weighted*Org.amend, 
            random = ~1|Paper, data = d, na.action = "na.omit"))

View(d %>%
       filter(!is.na(Org.amend)))


ggplot(data = d, aes(x=SOC.g.kg.weighted, y=IR))+
  geom_point()+
  facet_grid(IR.units~., scales = "free")

##

summary(gls(Tillage.IR.LRR ~ Tillage.SOC.LRR, data = d[!is.na(d$Tillage),]))

ggplot(data = d, aes(x=Tillage.SOC.LRR, y=Tillage.IR.LRR))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Relative impact of conservation tillage on SOC")+
  ylab("Relative impact of\nconservation tillage on IR")+
  geom_hline(yintercept = 0, color = "grey")+
  geom_vline(xintercept = 0, color = "grey")

test <- d[!is.na(d$Tillage.SOC.LRR),]

summary(gls(Cover.IR.LRR ~ Cover.SOC.LRR, data = d[!is.na(d$Cover),]))

ggplot(data = d, aes(x=Cover.SOC.LRR, y=Cover.IR.LRR))+
  geom_point()+
  geom_hline(yintercept = 0, color = "grey")+
  geom_vline(xintercept = 0, color = "grey")

summary(gls(Org.amened.IR.LRR ~ Org.amened.SOC.LRR, data = d[!is.na(d$Org.amend),]))

ggplot(data = d, aes(x=Org.amened.SOC.LRR, y=Org.amened.IR.LRR))+
  geom_point()+
  geom_hline(yintercept = 0, color = "grey")+
  geom_vline(xintercept = 0, color = "grey")

unique(d$Paper)
 




