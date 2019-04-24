### Makefile
# Alec Robitaille

## Sociality Summary
paper/summary-sociality.pdf: paper/summary-sociality.Rmd output/3-extraction/elkExtract.Rds output/3-extraction/caribouExtract.Rds
	Rscript -e "rmarkdown::render('paper/summary-sociality.Rmd')"


## 4 - Sociality
# NNA
output/4-sociality/elkNNA.Rds: scripts/4-sociality/Nearest-Neighbour-Analysis.R output/4-sociality/elkAngle.Rds
	Rscript scripts/4-sociality/Nearest-Neighbour-Analysis.R --arg elk

output/4-sociality/caribouNNA.Rds: scripts/4-sociality/Nearest-Neighbour-Analysis.R output/4-sociality/caribouAngle.Rds
	Rscript scripts/4-sociality/Nearest-Neighbour-Analysis.R --arg caribou

# Angles
output/4-sociality/elkAngle.Rds: scripts/4-sociality/Calculate-Turn-Angles.R output/3-extraction/elkExtract.Rds paper/summary-rsf-table.pdf
	Rscript scripts/4-sociality/Calculate-Turn-Angles.R --arg elk

output/4-sociality/caribouAngle.Rds: scripts/4-sociality/Calculate-Turn-Angles.R output/3-extraction/caribouExtract.Rds paper/summary-rsf-table.pdf
	Rscript scripts/4-sociality/Calculate-Turn-Angles.R --arg caribou

## RSF Summary
paper/summary-rsf-table.pdf: paper/summary-rsf-table.Rmd output/3-extraction/elkExtract.Rds output/3-extraction/caribouExtract.Rds
	Rscript -e "rmarkdown::render('paper/summary-rsf-table.Rmd')"


## 3 - Extract
# RMNP
output/3-extraction/elkExtract.Rds: scripts/3-extraction/Elk-Extraction.R output/2-rsf/models/elkWinterModel.Rds output/2-rsf/models/elkSpringModel.Rds output/2-rsf/models/wolfWinterModel.Rds output/2-rsf/models/wolfSpringModel.Rds
	Rscript scripts/3-extraction/Elk-Extraction.R

# NL
output/3-extraction/caribouExtract.Rds: scripts/3-extraction/Caribou-Extraction.R output/2-rsf/models/bearWinterModel.Rds output/2-rsf/models/bearSpringModel.Rds output/2-rsf/models/caribouWinterModel.Rds output/2-rsf/models/caribouSpringModel.Rds output/2-rsf/models/coyoteWinterModel.Rds output/2-rsf/models/coyoteSpringModel.Rds
	Rscript scripts/3-extraction/Caribou-Extraction.R


## 2 - RSFs
# RMNP
output/2-rsf/models/elkWinterModel.Rds output/2-rsf/models/elkSpringModel.Rds: scripts/2-rsf/RMNP-RSF.R output/1-data-prep/elk.Rds
	Rscript scripts/2-rsf/RMNP-RSF.R --arg Elk
	
output/2-rsf/models/wolfWinterModel.Rds output/2-rsf/models/wolfSpringModel.Rds: scripts/2-rsf/RMNP-RSF.R output/1-data-prep/wolf.Rds
	Rscript scripts/2-rsf/RMNP-RSF.R --arg Wolf

# NL
output/2-rsf/models/bearWinterModel.Rds output/2-rsf/models/bearSpringModel.Rds: scripts/2-rsf/NL-RSF.R output/1-data-prep/bear.Rds
	Rscript scripts/2-rsf/NL-RSF.R --arg Bear
	
output/2-rsf/models/caribouWinterModel.Rds output/2-rsf/models/caribouSpringModel.Rds: scripts/2-rsf/NL-RSF.R output/1-data-prep/caribou.Rds
	Rscript scripts/2-rsf/NL-RSF.R --arg Caribou
	
output/2-rsf/models/coyoteWinterModel.Rds output/2-rsf/models/coyoteSpringModel.Rds: scripts/2-rsf/NL-RSF.R output/1-data-prep/coyote.Rds
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