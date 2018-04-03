# Easter Week Challenge




## Project structure
```
.
├── graphics
│   └── data-prep
├── input
│   ├── covariates
│   │   └── RMNP
│   ├── etc
│   │   ├── NL-Bounds
│   │   └── RMNP-extent
│   └── locs
│       ├── RMNP_WolfLocations
├── output
│   ├── data-prep
│   ├── predator-rsf
│   └── prey-rsf
├── R
│   ├── data-prep
│   │   ├── BearPrep.R
│   │   ├── CaribouPrep.R
│   │   ├── CoyotePrep.R
│   │   ├── ElkPrep.R
│   │   ├── RasterPrep.R
│   │   ├── RuggCalc.R
│   │   └── WolfPrep.R
│   ├── functions
│   │   ├── DatePrep.R
│   │   ├── FindMinimumDistance.R
│   │   ├── GenerateGrid.R
│   │   ├── Group-Time-spatsoc.R
│   │   ├── NumbQuadTreeNeighbours.R
│   │   ├── PlotLocsByFigure.R
│   │   ├── StepLength.R
│   │   └── TemporalDistributionFigure.R
│   ├── method-devel
│   │   ├── predator-rsf
│   │   │   ├── BearRSF.R
│   │   │   ├── CoyoteRSF.R
│   │   │   └── WolfRSF.R
│   │   ├── prey-rsf
│   │   │   ├── CaribouRSF.R
│   │   │   └── ElkRSF.R
│   │   └── sociality
│   │       ├── AbsoluteTurnAngle.R
│   │       └── NearestNeighbourAnalysis.R
│   ├── RSFs
│   │   └── NLPredRSFs.R
│   └── variables
│       ├── CutOffThresholds.R
│       └── PrepDataOutputVariables.R
├── CONTRIBUTING.md
├── ewc.Rproj
├── LICENSE
├── NOTES.md
├── README.md
└── TODO.md
```


### /R/method-devel
Method development folder, to isolate development of methods from reproducible analysis. 

### /R/functions
Standalone functions for DRY (don't repeat yourself).

Eg: figures, date handling, step length, etc. 

*Ideally, these will be written defensively with tests*


## Output metadata

script               | output path & name            | description
---------------------|-------------------------------|--------------------
BearPrep.R           | data-prep/bear.Rds            | prepped bear data
                     |                               | 
                     |                               | 
                     |                               | 
                     |                               | 
                     |                               | 
                     |                               | 
                     |                               | 
