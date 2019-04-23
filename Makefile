### Makefile
# Alec Robitaille


## 2 - RSFs
# RMNP
output/2-rsf/models/elkWinterRSF.Rds output/2-rsf/models/elkSpringRSF.Rds: scripts/2-rsf/RMNP-RSF.R output/1-data-prep/elk.Rds
	Rscript scripts/2-rsf/RMNP-RSF.R --arg Elk
	
output/2-rsf/models/wolfWinterRSF.Rds output/2-rsf/models/wolfSpringRSF.Rds: scripts/2-rsf/RMNP-RSF.R output/1-data-prep/wolf.Rds
	Rscript scripts/2-rsf/RMNP-RSF.R --arg Wolf

# NL
output/2-rsf/models/bearWinterRSF.Rds output/2-rsf/models/bearSpringRSF.Rds: scripts/2-rsf/NL-RSF.R output/1-data-prep/bear.Rds
	Rscript scripts/2-rsf/NL-RSF.R --arg Bear
	
output/2-rsf/models/caribouWinterRSF.Rds output/2-rsf/models/caribouSpringRSF.Rds: scripts/2-rsf/NL-RSF.R output/1-data-prep/caribou.Rds
	Rscript scripts/2-rsf/NL-RSF.R --arg Caribou
	
output/2-rsf/models/coyoteWinterRSF.Rds output/2-rsf/models/coyoteSpringRSF.Rds: scripts/2-rsf/NL-RSF.R output/1-data-prep/coyote.Rds
	Rscript scripts/2-rsf/NL-RSF.R --arg Coyote
	


## 1 - Prep data
# RMNP
output/1-data-prep/elk.Rds: scripts/1-data-prep/Relocations/Elk-Prep.R
	Rscript scripts/1-data-prep/Relocations/Elk-Prep.R
	
output/1-data-prep/wolf.Rds: scripts/1-data-prep/Relocations/Wolf-Prep.R
	Rscript scripts/1-data-prep/Relocations/Wolf-Prep.R

# NL
output/1-data-prep/bear.Rds: scripts/1-data-prep/Relocations/Bear-Prep.R
	Rscript scripts/1-data-prep/Relocations/Bear-Prep.R
	
output/1-data-prep/caribou.Rds: scripts/1-data-prep/Relocations/Caribou-Prep.R
	Rscript scripts/1-data-prep/Relocations/Caribou-Prep.R
	
output/1-data-prep/coyote.Rds: scripts/1-data-prep/Relocations/Coyote-Prep.R
	Rscript scripts/1-data-prep/Relocations/Coyote-Prep.R