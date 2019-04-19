### Makefile
# Alec Robitaille

## 1 - Prep data
output/1-data-prep/*.Rds: scripts/1-data-prep/Relocations/*.R
	#Rscript "scripts/1-data-prep/Relocations/*.R"
	for file in scripts/1-data-prep/Relocations/*.R; do Rscript $file; done