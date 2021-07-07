**Supplement to Todd, N., & Coulmont, B. (2021). Naming for Kin during World War I: Baby Names as Markers for War. _Journal of Interdisciplinary History_, 52(1), 55-67.**

# Naming for Kin during World War 1

Paternal (father to son/daughter) and avuncular (uncle to nephew/niece) name transmission during WW1 using a large genealogy database.

The analysis is based on crowdsourced genealogies from the [Geneanet](https://www.geneanet.org/) website. 

## Deduplication process

The raw data are the ~5.5 million "Geneanet individuals" born 1905-1925 in France. An individual is present in the dataset the number of times one of his descendants included him in his family tree.

The code in folder **R_deduplication** performs the deduplication of the original dataset based on the EM algorithm implemented in the R package [RecordLinkage](https://cran.r-project.org/web/packages/RecordLinkage/index.html). Deduplication is stratified by sex and year of birth and run on a computer cluster.

Start from script **main_dedup.R**.
