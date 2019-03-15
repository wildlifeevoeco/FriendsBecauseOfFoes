
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Easter Week Challenge

## TODOs/NOTEs

``` bash
grep -rni 'R/' -e 'TODO';
#> R/AbsoluteAngle.R:41:  # TODO: what did this mean? .. my poor code commenting
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
    │   │   ├── AbsoluteAngle.R
    │   │   ├── DatePrep.R
    │   │   ├── DyadicDistance.R
    │   │   ├── DyadicID.R
    │   │   ├── FindNumbWithinDistance.R
    │   │   ├── GenerateGrid.R
    │   │   ├── NumbQuadTreeNeighbours.R
    │   │   ├── PlotLocsByFigure.R
    │   │   ├── RelativeAngle.R
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
