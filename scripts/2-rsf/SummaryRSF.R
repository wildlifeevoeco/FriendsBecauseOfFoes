### Generating summary tables for RSFs ====
# Authors: Alec Robitaille, Michel Laforge

### Packages ----
pkgs <- c('data.table', 'piecewiseSEM', 'lme4')
lapply(pkgs, require, character.only = TRUE)

### Input ----
rsfs <- data.table(
  paths = dir('output/2-rsf', 'RSF', recursive = TRUE, full.names = TRUE)
)
rsfs[, nm := tstrsplit(paths, '/', keep = 4)]
rsfs[, nm := gsub('.Rds', '', nm)]

rsfs[grepl('spring', nm, ignore.case = TRUE), season := 'spring']
rsfs[grepl('winter', nm, ignore.case = TRUE), season := 'winter']

rsfs[, species := gsub('spring|winter|rsf', '', nm, ignore.case = TRUE)]


### Processing ----
digits <- 3
rsfs[, {
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
