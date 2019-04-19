### Makefile
# Alec Robitaille

## 1 - Prep data
output/1-data-prep/*.Rds: scripts/1-data-prep/RMNP/Elk-Prep.R
	Rscript "scripts/1-data-prep/RMNP/Elk-Prep.R"
	
output/1-data-prep/wolf.Rds: scripts/1-data-prep/RMNP/Wolf-Prep.R
	Rscript "scripts/1-data-prep/RMNP/Wolf-Prep.R"

output/1-data-prep/caribou.Rds: scripts/1-data-prep/RMNP/Elk-Prep.R
	Rscript "scripts/1-data-prep/RMNP/Elk-Prep.R"

output/1-data-prep/elk.Rds: scripts/1-data-prep/RMNP/Elk-Prep.R
	Rscript "scripts/1-data-prep/RMNP/Elk-Prep.R"

output/1-data-prep/elk.Rds: scripts/1-data-prep/RMNP/Elk-Prep.R
	Rscript "scripts/1-data-prep/RMNP/Elk-Prep.R"
