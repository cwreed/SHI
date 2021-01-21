source("code/libraries.R")
load(here::here("data", "d.yield.RData"))

################################################################################
### No carbon, non-aggregated yield data
################################################################################

d.yield.nonagg %>%
  group_by(Crop) %>%
  mutate(Yield.var.by.crop = var(Yield.kg.per.hectare)) -> d.yield.nonagg

d.yield.nonagg <- d.yield.nonagg %>%
  group_by(Paper,DOI,Study_name,Crop,end_obs) %>%
  mutate(Yield.weighted = 10000*Yield.kg.per.hectare/(var(.$Yield.kg.per.hectare) + Yield.var.by.crop),
         EI.by.year = mean(Yield.kg.per.hectare),
         EI.by.year.weighted = mean(Yield.weighted)) %>%
  ungroup() %>%
  as.data.frame(.)

  mapping = c("y" = 1, "n" = 0)

  d.yield.nonagg <- d.yield.nonagg %>%
  mutate_at(names(.)[38:44], function(i) mapping[i])


# SPEI data looks normal, so we'll split up by standard deviation
d.yield.nonagg %>%
  ggplot(mapping = aes(SPEI.1)) +
  geom_histogram()
  
  # Data frame representing observations in years with significantly bad drought conditions
  d.yield.filtered1 <- d.yield.nonagg %>%
    filter(SPEI.1 != "-Inf") %>%
    filter(SPEI.1 < (mean(.$SPEI.1) - 2*sd(.$SPEI.1)))

  # Data frame representing observations in all years with drought conditions between 1 and 2 SDs below mean
  d.yield.filtered2 <- d.yield.nonagg %>%
    filter(SPEI.1 != "-Inf") %>%
    filter(SPEI.1 >= (mean(.$SPEI.1) - 2*sd(.$SPEI.1)) & SPEI.1 < (mean(.$SPEI.1) - sd(.$SPEI.1)))
  
  # Data frame representing observations in all years where SPEI is within 1 SD of mean
  d.yield.filtered3 <- d.yield.nonagg %>%
    filter(SPEI.1 != "-Inf") %>%
    filter(SPEI.1 >= (mean(.$SPEI.1) - sd(.$SPEI.1)) & SPEI.1 <= (mean(.$SPEI.1) + sd(.$SPEI.1)))
  
  # Data frame representing observations in years where SPEI is between 1 and 2 SDs above mean
  d.yield.filtered4 <- d.yield.nonagg %>%
    filter(SPEI.1 != "-Inf") %>%
    filter (SPEI.1 > (mean(.$SPEI.1) + sd(.$SPEI.1)) & SPEI.1 <= (mean(.$SPEI.1) + 2*sd(.$SPEI.1)))
  
  # Data frame representing observations in years with significantly wet conditions, where SPEI is at least 2 SDs above mean
  d.yield.filtered5 <- d.yield.nonagg %>%
    filter(SPEI.1 != "-Inf") %>%
    filter(SPEI.1 > (mean(.$SPEI.1) + 2*sd(.$SPEI.1)))

## LME with covariates

# Run for all filtered data sets and treatments
# Results recorded at: https://docs.google.com/spreadsheets/d/1-4Mq3p9yPTGBo2L5b5kDuBktsKRHxabzDJqj80yXzVs/edit#gid=1372771532

new <- lme(Yield.weighted ~ Fertilizer, random = ~1|Paper/Crop,
                 data = d.yield.filtered3, na.action = "na.omit")
new.tidy <- broom.mixed::tidy(new)

new.tidy %>%
  clipr::write_clip()

d.yield.nonagg %>%
  filter(Fertilizer == "y") %>%
  lme(Yield.kg.per.hectare ~ EI.by.year, random = ~1|AEZ/Paper/Crop,
              data = ., na.action = "na.omit") -> test
  
################################################################################
### Create yield models for each crop independently using nested dataframes ####
################################################################################

### Old code, may be irrelevant but keeping as archetype for later if needed ###
tillage_effects_by_crop_general <- d.yield.filtered2 %>%
  filter(!is.na(Tillage)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Tillage, random = ~1|Paper,
                                                    data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

tillage_yield_tidy_general <- tillage_effects_by_crop_general %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy)

tillage_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Tillage)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Tillage, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

tillage_yield_tidy_all <- tillage_effects_by_crop_all %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy)

### Copy to clipboard for pasting into Google Sheet ############################
fert_org_yield_tidy_all %>%
  select(-c(data, model, resids)) %>%
  clipr::write_clip()
