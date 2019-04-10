
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Easter Week Challenge

## Rerun

*deleted scripts*, ~~completed~~, (Old Name)

  - scripts
      - 1-data-prep
          - ~~BearPrep.R~~
          - ~~CaribouPrep.R~~
          - ~~CoyotePrep.R~~
          - ~~ElkPrep.R~~
          - ~~WolfPrep.R~~
          - ~~CovariatePrep.R (RasterPrep.R)~~
          - ~~*RuggCalc.R*~~
      - 2-rsf
          - Generating summary tables for RSFs.R
          - BearRSF.R
          - CoyoteRSF.R
          - NLPredRSFs.R
          - ~~WolfRSF.R~~
          - CaribouRSF.R
          - ~~ElkRSF.R~~
      - 3-extraction
          - cariboudata-rsfvalues.R
          - ~~Elk-Extraction.R (elkdata-rsfvalues.R)~~
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
#> R/calc_abs_angle.R:44:  # TODO: what did this mean? .. my poor code commenting
#> R/prep_date.R:16:  # TODO: add a tz argument
#> scripts/5-model/socialitydomainGLM.R:6:# TODO:  how to ignore dif in line endings/force git to use one
#> scripts/3-extraction/Caribou-Extraction.R:5:# TODO: rename script/folder to domain? domain extraction? etc
#> scripts/3-extraction/Elk-Extraction.R:5:# TODO: rename script/folder to domain? domain extraction? etc
#> scripts/3-extraction/Elk-Extraction.R:40:# TODO: rename output "rsfvalues"
#> scripts/4-sociality/NearestNeighbourAnalysis.R:7:#TODO: use edge-dist/nn instead
#> scripts/4-sociality/NearestNeighbourAnalysis.R:8:#TODO: Grab Hance's updated social metrics from modeling script
#> scripts/4-sociality/NearestNeighbourAnalysis.R:46:# TODO: investigate the both coords and ..coords exist in calling scope data.table error
#> scripts/4-sociality/NearestNeighbourAnalysis.R:143:# TODO: investigate the both coords and ..coords exist in calling scope data.table error
#> scripts/4-sociality/CalculateTurnAngles.R:5:#TODO: Grab Hance's updated social metrics from modeling script
#> scripts/4-sociality/CalculateTurnAngles.R:30:# TODO: check rleid/ http://stackoverflow.com/q/21421047/559784
#> scripts/4-sociality/CalculateTurnAngles.R:34:# TODO: (Ask Hance) would it be better if we had range from 0-360 instead? for dif abs angle
#> scripts/4-sociality/CalculateTurnAngles.R:37:#TODO: check that yrcol is well handled
#> scripts/1-data-prep/NL/CovariatePrepNL.R:24:#TODO: run prep/processing for ruggedness
#> scripts/1-data-prep/NL/BearPrep.R:97:# TODO: is this really the step length threshold?? 
#> scripts/1-data-prep/NL/CoyotePrep.R:191:# TODO: is this really the step length threshold?? 
#> scripts/2-rsf/RMNP/ElkRSF.R:48:# TODO: elk - 4.3 regular to 1 observed
#> scripts/2-rsf/RMNP/WolfRSF.R:13:# TODO: check need car and piecewiseSEM?
#> scripts/2-rsf/RMNP/WolfRSF.R:47:# TODO: wolf - 7 regular to 1 observed
#> scripts/2-rsf/NL/CoyoteRSF.R:78:# TODO: Remove all points with 50% NA data
#> scripts/2-rsf/NL/CoyoteRSF.R:82:# TODO: need ruggedness
#> scripts/2-rsf/NL/CoyoteRSF.R:107:# TODO: need ruggedness
#> scripts/2-rsf/NL/CaribouRSF.R:77:# TODO: Remove all points with 50% NA data
#> scripts/2-rsf/NL/CaribouRSF.R:81:# TODO: need ruggedness
#> scripts/2-rsf/NL/CaribouRSF.R:106:# TODO: need ruggedness
#> scripts/2-rsf/NL/BearRSF.R:78:# TODO: Remove all points with 50% NA data
#> scripts/2-rsf/NL/BearRSF.R:82:# TODO: need ruggedness
#> scripts/2-rsf/NL/BearRSF.R:107:# TODO: need ruggedness
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
