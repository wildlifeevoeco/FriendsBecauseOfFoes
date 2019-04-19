### Generating summary tables for RSFs ====
# Authors: Alec Robitaille, Michel Laforge

### Packages ----
pkgs <- c('data.table', 'piecewiseSEM', 'lme4')
lapply(pkgs, require, character.only = TRUE)

### Input ----
rsfs <- data.table(
  paths = dir('output/2-rsf', '[SpringWinter]RSF.Rds', recursive = TRUE, full.names = TRUE)
)
rsfs[, species := tstrsplit(paths, '/', keep = 3)]

rsfs[grepl('Spring', paths), season := 'spring']
rsfs[grepl('Winter', paths), season := 'winter']


### Processing ----
digits <- 3
out <- rsfs[, {
  r <- readRDS(paths)
  coefs <- coef(r)
  
  se <- sqrt(diag(vcov(r)))
  
  lci <- coefs - (se * 1.96)
  uci <- coefs + (se * 1.96)
  
  
  list(Coefficient = names(coefs),
       Estimate = paste0(round(coefs, digits = digits), " [", 
                         round(lci, digits = digits), ", ",
                         round(uci, digits = digits),"]"),
       RSquared = rsquared(r)$`R.squared`)
}, by = .(season, species)]

### Output ----
saveRDS(out, 'output/2-rsf/summaryRSF.Rds')
