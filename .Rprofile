old <- getOption("defaultPackages")

# remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")
new_pkgs <- c("magrittr", "tidyverse", "matPkg")
options(defaultPackages = c(old, new_pkgs))
options(tibble.print_max = 6, tibble.print_min = 5)
options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors
options(dplyr.summarise.inform = FALSE)

s <- sapply(options()$defaultPackages, function(x) suppressMessages(require(x, character.only = TRUE)))
if(!all(s)) warning("Issue with one package...")

## custom functions
gg_width <- 8
gg_height <- 5

`%+c%` <- crayon::`%+%`

prj_info <- function(){
  
  l1 <- crayon::red$bold("\n\tWelcome to Yield gaps!\n\n")
  l2 <- "\tDetails:\n"
  l4 <- "\t -USE renv: no" 
  l4 <- "\t -This project loads by default: " %+c% crayon::blue(paste(paste(new_pkgs, collapse  = ", "), "\n"))
  l5 <- "\t -This project set many options, see .Rprofile\n"
  l6 <- "\t -repos: \n\t   -Github: https://github.com/MatthieuStigler/Yield-Convergence-Analysis\n"
  
  cat(paste(l1, l2, l4, l5, l6, "\n\n"))
}


.First <- function() {
  .First.sys()
  if(interactive()) prj_info()
}
