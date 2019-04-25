
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Easter Week Challenge

## Rerun

*completed*, (~~deleted scripts/old name~~)

  - scripts
      - 1-data-prep
          - *Bear-Prep.R*
          - *Caribou-Prep.R*
          - *Coyote-Prep.R*
          - *Elk-Prep.R*
          - *Wolf-Prep.R*
          - *Covariate-Prep-NL.R* *Covariate-Prep-RMNP.R*
            (~~RasterPrep.R~~)
          - *RuggCalc.R*
      - 2-rsf
          - *Summary-RSF.R* *paper/summary-rsf-table.Rmd* (~~Generating
            summary tables for RSFs.R~~)
          - *RMNP-RSF.R* (~~WolfRSF.R~~, ~~ElkRSF.R~~)
          - *NL-RSF.R* (~~CaribouRSF.R~~, ~~CoyoteRSF.R~~,
            ~~BearRSF.R~~, ~~NLPredRSFs.R~~)
      - 3-extraction
          - *Caribou-Extraction.R* (~~cariboudata-rsfvalues.R~~)
          - *Elk-Extraction.R* (~~elkdata-rsfvalues.R~~)
      - 4-sociality
          - Calculate-Turn-Angles.R
          - Nearest-Neighbour-Analysis.R
      - 5-model
          - sociality-domain-GLM.R
      - 0-variables
          - *variables.R* \`\`\`

## TODOs/NOTEs

``` bash
grep -rni 'R/' -e 'TODO' || true;
grep -rni 'scripts/' -e 'TODO' || true;
#> R/prep_date.R:16:  # TODO: add a tz argument
#> R/internal.R:1:#TODO: remove? is this used?
#> scripts/5-model/sociality-domain-GLM.R:30:# TODO: add is duplicated column
#> scripts/4-sociality/Nearest-Neighbour-Analysis.R:112:#TODO: where NL + spring, this:     (nl.dyad2$predatorRSF + nl.dyad2$rpredatorRSF) / 2, ?
#> scripts/1-data-prep/Relocations/Bear-Prep.R:101:# TODO: is this really the step length threshold?? 
#> scripts/1-data-prep/Relocations/Coyote-Prep.R:193:# TODO: is this really the step length threshold?? 
#> scripts/2-rsf/RMNP-RSF.R:62:# TODO: number of regular points?
#> scripts/2-rsf/NL-RSF.R:47:# TODO: size of grid?
#> scripts/2-rsf/NL-RSF.R:103:#TODO: warning glm.fit: fitted probabilities numerically 0 or 1 occurred  (bear winter, caribou winter)
```

## Project structure

    .
    ├── graphics
    ├── input
    │   ├── covariates
    │   │   ├── NL
    │   │   └── RMNP
    │   ├── extent
    │   │   ├── NL
    │   │   │   └── NL-Bounds.shp
    │   │   └── RMNP
    │   │       └── RMNPextent.shp
    │   └── locs
    │       ├── RMNP_WolfLocations
    │       ├── AllCaribouDataRaw.csv
    │       ├── Bears.csv
    │       ├── Coyote.csv
    │       └── RMNP_ElkData_clean.csv
    ├── man
    ├── output
    │   ├── 1-data-prep
    │   │   ├── covariates
    │   │   │   ├── NL
    │   │   │   └── RMNP
    │   │   ├── bear.Rds
    │   │   ├── caribou.Rds
    │   │   ├── coyote.Rds
    │   │   ├── elk.Rds
    │   │   └── wolf.Rds
    │   ├── 2-rsf
    │   │   ├── bear
    │   │   ├── caribou
    │   │   ├── coyote
    │   │   ├── elk
    │   │   ├── temp
    │   │   ├── wolf
    │   │   └── summaryRSF.Rds
    │   ├── 3-extraction
    │   │   ├── caribouExtract.Rds
    │   │   └── elkExtract.Rds
    │   └── 4-sociality
    │       ├── elkAngle.Rds
    │       └── elkNNA.Rds
    ├── paper
    │   ├── summary-rsf-table.pdf
    │   └── summary-rsf-table.Rmd
    ├── R
    │   ├── angles.R
    │   ├── calc_di.R
    │   ├── dyad_dist.R
    │   ├── dyad_id.R
    │   ├── generate_grid.R
    │   ├── internal.R
    │   ├── prep_date.R
    │   └── step_length.R
    ├── scripts
    │   ├── 0-variables
    │   │   └── variables.R
    │   ├── 1-data-prep
    │   │   ├── NL
    │   │   │   ├── Bear-Prep.R
    │   │   │   ├── Caribou-Prep.R
    │   │   │   ├── Covariate-Prep-NL.R
    │   │   │   └── Coyote-Prep.R
    │   │   └── RMNP
    │   │       ├── Covariate-Prep-RMNP.R
    │   │       ├── Elk-Prep.R
    │   │       └── Wolf-Prep.R
    │   ├── 2-rsf
    │   │   ├── NL-RSF.R
    │   │   ├── RMNP-RSF.R
    │   │   └── Summary-RSF.R
    │   ├── 3-extraction
    │   │   ├── Caribou-Extraction.R
    │   │   └── Elk-Extraction.R
    │   ├── 4-sociality
    │   │   ├── Calculate-Turn-Angles.R
    │   │   └── Nearest-Neighbour-Analysis.R
    │   └── 5-model
    │       ├── socialitydomainGLM-archive.R
    │       └── sociality-domain-GLM.R
    ├── CONTRIBUTING.md
    ├── DESCRIPTION
    ├── ewc.Rproj
    ├── LICENSE
    ├── NAMESPACE
    ├── README.md
    ├── README.Rmd
