
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Easter Week Challenge

## Rerun

~~*deleted scripts*~~, ~~completed~~, (Old Name)

  - scripts
      - 1-data-prep
          - ~~BearPrep.R~~
          - ~~CaribouPrep.R~~
          - ~~CoyotePrep.R~~
          - ~~ElkPrep.R~~
          - ~~WolfPrep.R~~
          - ~~CovariatePrepNL.R CovariatePrepRMNP.R(RasterPrep.R)~~
          - ~~*RuggCalc.R*~~
      - 2-rsf
          - Generating summary tables for RSFs.R
          - BearRSF.R (NLPredRSFs.R)
          - CoyoteRSF.R (NLPredRSFs.R)
          - ~~WolfRSF.R~~
          - ~~*NLPredRSFs.R*~~
          - CaribouRSF.R (NLPredRSFs.R)
          - ~~ElkRSF.R~~
      - 3-extraction
          - Caribou-Extraction.R (cariboudata-rsfvalues.R)
          - Elk-Extraction.R (elkdata-rsfvalues.R)
      - 4-sociality
          - CalculateTurnAngles.R
          - NearestNeighbourAnalysis.R
      - 5-model
          - socialitydomainGLM.R
      - 0-variables
          - variables.R \`\`\`

## TODOs/NOTEs

``` bash
grep -rni 'R/' -e 'TODO' || true;
grep -rni 'scripts/' -e 'TODO' || true;
#> R/prep_date.R:16:  # TODO: add a tz argument
#> scripts/3-extraction/Caribou-Extraction.R:5:# TODO: rename script/folder to domain? domain extraction? etc
#> scripts/3-extraction/Elk-Extraction.R:5:# TODO: rename script/folder to domain? domain extraction? etc
#> scripts/3-extraction/Elk-Extraction.R:40:# TODO: rename output "rsfvalues"
#> scripts/4-sociality/NearestNeighbourAnalysis.R:4:#TODO: Grab Hance's updated social metrics from modeling script
#> scripts/4-sociality/NearestNeighbourAnalysis.R:127:# TODO: investigate the both coords and ..coords exist in calling scope data.table error
#> scripts/1-data-prep/NL/CovariatePrepNL.R:78:#TODO: why non matching origin
#> scripts/1-data-prep/NL/BearPrep.R:97:# TODO: is this really the step length threshold?? 
#> scripts/1-data-prep/NL/CoyotePrep.R:191:# TODO: is this really the step length threshold?? 
#> scripts/2-rsf/RMNP/ElkRSF.R:48:# TODO: elk - 4.3 regular to 1 observed
#> scripts/2-rsf/RMNP/WolfRSF.R:38:# TODO: wolf - 7 regular to 1 observed
#> scripts/2-rsf/NL/CoyoteRSF.R:78:# TODO: Remove all points with 50% NA data
#> scripts/2-rsf/NL/CoyoteRSF.R:82:# TODO: need ruggedness
#> scripts/2-rsf/NL/CoyoteRSF.R:107:# TODO: need ruggedness
#> scripts/2-rsf/NL/CaribouRSF.R:77:# TODO: Remove all points with 50% NA data
#> scripts/2-rsf/NL/CaribouRSF.R:81:# TODO: need ruggedness
#> scripts/2-rsf/NL/CaribouRSF.R:106:# TODO: need ruggedness
#> scripts/2-rsf/NL/BearRSF.R:37:# TODO: note change to mcps for bear rsf
#> scripts/2-rsf/NL/BearRSF.R:85:#TODO: warning glm.fit: fitted probabilities numerically 0 or 1 occurred
```

## Project structure

    .
    ├── graphics
    │   ├── data-model-prep
    │   └── data-prep
    ├── input
    │   ├── covariates
    │   │   └── RMNP
    │   ├── etc
    │   │   ├── NL-Bounds
    │   │   └── RMNP-extent
    │   └── locs
    │       ├── RMNP_WolfLocations
    │       ├── AllCaribouDataRaw.csv
    │       ├── Bears.csv
    │       ├── Coyote.csv
    │    *  RMNP_ElkData_clean.csv
    ├── output
    │   ├── angles
    │   ├── data-prep
    │   ├── nna
    │   ├── predator-rsf
    │   ├── prey-rsf
    │   └── rsf-values
    ├── R
    │   ├── 0-functions
    │   │   ├── calc_abs_angle.R
    │   │   ├── prep_date.R
    │   │   ├── DyadicDistance.R
    │   │   ├── DyadicID.R
    │   │   ├── FindNumbWithinDistance.R
    │   │   ├── generate_grid.R
    │   │   ├── NumbQuadTreeNeighbours.R
    │   │   ├── PlotLocsByFigure.R
    │   │   ├── calc_rel_angle.R
    │   │   ├── StepLength.R
    │   │   └── TemporalDistributionFigure.R
    │   ├── 0-variables
    │   │   ├── CutOffThresholds.R
    │   │   └── PrepDataOutputVariables.R
    │   ├── 1-data-prep
    │   │   ├── BearPrep.R
    │   │   ├── CaribouPrep.R
    │   │   ├── CoyotePrep.R
    │   │   ├── ElkPrep.R
    │   │   ├── RasterPrep.R
    │   │   ├── RuggCalc.R
    │   │   └── WolfPrep.R
    │   ├── 2-rsf
    │   │   ├── predator-rsf
    │   │   │   ├── BearRSF2.R
    │   │   │   ├── BearRSF.R
    │   │   │   ├── CoyoteRSF2.R
    │   │   │   ├── CoyoteRSF.R
    │   │   │   └── WolfRSF.R
    │   │   ├── prey-rsf
    │   │   │   ├── CaribouRSF2.R
    │   │   │   ├── CaribouRSF.R
    │   │   │   └── ElkRSF.R
    │   │   └── NLPredRSFs.R
    │   ├── 3-extraction
    │   │   ├── cariboudata-rsfvalues.R
    │   │   └── elkdata-rsfvalues.R
    │   ├── 4-sociality
    │   │   ├── 4.1-CalculateTurnAngles.R
    │   │   └── 4.2-NearestNeighbourAnalysis.R
    │   ├── 5-model
    │   │   └── socialitydomainGLM.R
    │   └── Generating summary tables for RSFs.R
    ├── CONTRIBUTING.md
    ├── ewc.Rproj
    ├── LICENSE
    ├── NOTES.md
    ├── README.md
    ├── Spring NL Dyad Distance.jpeg
    ├── TODO.md
    └── Winter NL DI.jpeg

## Output metadata

| script     | output path & name | description       |
| ---------- | ------------------ | ----------------- |
| BearPrep.R | data-prep/bear.Rds | prepped bear data |
|            |                    |                   |
|            |                    |                   |
|            |                    |                   |
|            |                    |                   |
|            |                    |                   |
|            |                    |                   |
|            |                    |                   |
