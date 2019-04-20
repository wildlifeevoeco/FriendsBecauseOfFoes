### Makefile
# Alec Robitaille

SPECIES-NL = Caribou Coyote Bear
SPECIES-MB = Wolf Elk
SPECIES = $(SPECIES-NL) $(SPECIES-MB)


## 2 - RSFs
output/2-rsf/models/*.Rds output/2-rsf/rasters/.tif: scripts/2-rsf/NL-RSF.R scripts/2-rsf/RMNP-RSF.R output/1-data-prep/*.Rds
	$(foreach var,$(SPECIES-MB), Rscript scripts/2-rsf/NL-RSF.R --arg $(var) ;)
	$(foreach var,$(SPECIES-NL), Rscript scripts/2-rsf/RMNP-RSF.R --arg $(var) ;)


## 1 - Prep data
output/1-data-prep/*.Rds: scripts/1-data-prep/Relocations/*.R
	$(foreach var,$(SPECIES), Rscript scripts/1-data-prep/Relocations/$(var)-Prep.R ;)