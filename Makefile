### Makefile
# Alec Robitaille

## 1 - Prep data
output/1-data-prep/*.Rds: scripts/1-data-prep/Relocations/*.R
	$(foreach var,$(SPECIES),Rscript scripts/1-data-prep/Relocations/$(var)-Prep.R &)