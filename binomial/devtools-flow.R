# On the menu bar, go to "Session"
# Choose "Set Working Directory"
# Choose "To Project Directory"

# =====================================================
# Using devtools
# =====================================================

library(devtools)

devtools::document()          # generate documentation
devtools::check_man()         # check documentation
devtools::test()              # run tests
devtools::build_vignettes()   # build vignettes
devtools::build()             # build bundle
devtools::install()           # install package
