#######################################################
#                                                     #
# Implementing good practises for creating a package  #
#                                                     #
#######################################################

# why we moved all .R files out of the R folder?

# open a new .R file within the R folder, where the new function will be stored
use_r("dlnorm")
# makes functions available, although it does not exist in the global environment
load_all()
# check if function exists in the Global Environment
exists("dlnorm", where = globalenv(), inherits = FALSE)

# If you see TRUE instead of FALSE, that indicates you’re still using a script-oriented workflow and sourcing your functions. Here’s how to get back on track:

# 1) Clean out the global environment and restart R.
# 2) Re-attach devtools with library(devtools) and re-load regexcite with load_all().
# 3) Redefine the test input x and call strsplit1(x, split = ",") again. This should work!
# 4) Run exists("strsplit1", where = globalenv(), inherits = FALSE) again and you should see FALSE

# gold standard for checking that an R package is in full working order
check()

# converting dlnorm()’s special comment into man/dlnorm.Rd
# updates the NAMESPACE file, based on @export tags found in roxygen comments
document()

# preview of the help file
help(dlnorm)
