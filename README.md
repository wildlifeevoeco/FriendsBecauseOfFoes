
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Easter Week Challenge

## TODOs/NOTEs

``` bash
grep -rni 'R/' -e 'TODO';
grep -rni 'scripts/' -e 'TODO';
#> R/calc_abs_angle.R:41:  # TODO: what did this mean? .. my poor code commenting
#> scripts/4-sociality/NearestNeighbourAnalysis.R:8:#TODO: Grab Hance's updated social metrics from modeling script
#> scripts/4-sociality/NearestNeighbourAnalysis.R:45:# TODO: investigate the both coords and ..coords exist in calling scope data.table error
#> scripts/4-sociality/NearestNeighbourAnalysis.R:142:# TODO: investigate the both coords and ..coords exist in calling scope data.table error
#> scripts/4-sociality/CalculateTurnAngles.R:11:#TODO: Grab Hance's updated social metrics from modeling script
#> scripts/4-sociality/CalculateTurnAngles.R:38:#TODO: check that yrcol is well handled
#> scripts/1-data-prep/BearPrep.R:85:# TODO: double check type of step_length
#> scripts/1-data-prep/BearPrep.R:116:# TODO: do we ever need Map_Quality, NAV?
#> scripts/2-rsf/predator-rsf/WolfRSF.R:75:# TODO: why also where season is NA?
#> scripts/2-rsf/predator-rsf/WolfRSF.R:79:# TODO: #### Dist not logged yet - should it be?
#> scripts/2-rsf/predator-rsf/WolfRSF.R:92:# TODO: with intercept of -3.044340?
#> scripts/2-rsf/predator-rsf/WolfRSF.R:106:# TODO: why also na season?
#> scripts/2-rsf/predator-rsf/WolfRSF.R:121:# TODO: with intercept of -2.711875?
#> scripts/2-rsf/predator-rsf/WolfRSF.R:143:# TODO: can this be dropped then?
#> scripts/2-rsf/prey-rsf/ElkRSF.R:77:# TODO: Dist not logged yet - should it be?
#> scripts/2-rsf/prey-rsf/ElkRSF.R:89:#TODO: with intercept of -3.242852 ?
#> scripts/2-rsf/prey-rsf/ElkRSF.R:103:# TODO: why is.na season?
#> scripts/2-rsf/prey-rsf/ElkRSF.R:117:# TODO: with intercept of -2.053773?
#> scripts/2-rsf/prey-rsf/ElkRSF.R:139:# TODO: can this be dropped then?
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
    │       └── RMNP_ElkData_clean.csv
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
