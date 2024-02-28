#######################################################
#                                                     #
# Implementing good practises for creating a package  #
#                                                     #
#######################################################

# open a new .R file within the R folder, where the new function will be stored
use_r("dfunctions")
# makes functions available, although it does not exist in the global environment
devtools::load_all()
# check if function exists in the Global Environment
exists("dfunctions", where = globalenv(), inherits = FALSE)

# If you see TRUE instead of FALSE, that indicates you’re still using a script-oriented workflow and sourcing your functions. Here’s how to get back on track:

# 1) Clean out the global environment and restart R.
# 2) Re-attach devtools with library(devtools) and re-load regexcite with load_all().
# 3) Redefine the test input x and call strsplit1(x, split = ",") again. This should work!
# 4) Run exists("strsplit1", where = globalenv(), inherits = FALSE) again and you should see FALSE

# gold standard for checking that an R package is in full working order
check()

# converting dfunctions()’s special comment into man/dfunctions.Rd
# updates the NAMESPACE file, based on @export tags found in roxygen comments
document()

##### REPEAT for other functions

use_r("wtdttt")
devtools::load_all()
exists("wtdttt", where = globalenv(), inherits = FALSE)

check()

document()

#####

use_r("pred_dur_prob")
devtools::load_all()
exists("pred_dur_prob", where = globalenv(), inherits = FALSE)

check()

document()

# preview of the help file
help(dlnorm)

########

use_r("try")

########## UPDATE DESCRIPTION

# IMPORT: Packages listed in Imports are needed by your users at runtime and will be installed (or potentially updated) when users install your package via install.packages()

# to add a package to import
usethis::use_package("haven")
usethis::use_package("data.table")

# run it regularly, to order and format DESCRIPTION fields according to a fixed standard
usethis::use_tidy_description()


########## TESTING

# create test/testthat directories and testthat.R file
usethis::use_testthat(3)

# create and open a .R file with tests
##(not necessary to specify the extension .R)
##(if the target file already exists, it is opened for editing. Otherwise, the target is created and then opened for editing)
usethis::use_test("dfunctions")
usethis::use_test("wtdttt")

# RUN TEST

# i) Micro-iteration: run load_all() often, and then execute individual expectations or whole tests interactively in the console
# ii) Mezzo-iteration: As one file’s-worth of functions and their associated tests start to shape up, you will want to execute the entire file of associated tests, perhaps with testthat::test_file():

testthat::test_file("tests/testthat/test-wtdttt.R")

# iii) Macro-iteration: As you near the completion of a new feature or bug fix, you will want to run the entire test suite. Most frequently, you’ll do this with devtools::test():

devtools::test()

# Then eventually, as part of R CMD check with devtools::check()

devtools::check()

# Tests are organised hierarchically: expectations are grouped into tests which are organised in files

# All expectations have a similar structure:
#
# i) They start with expect_.
#
# ii) They have two main arguments: the first is the actual result, the second is what you expect.
#
# iii) If the actual and expected results don’t agree, testthat throws an error.

# There are more than 40 expectations in the testthat package

