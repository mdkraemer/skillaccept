#### Main script: Personality Change Through Self-Improvement or Self-Acceptance ####


### Preparation
# Tested with R version 4.3.1 and RStudio version 2023.06.1+524 on both Mac and Windows

# Please make sure that you run all files/commands from within the R project 
# ("improveaccept.Rroj") so that your working directory lies within that folder.

# Windows users need to make sure that Rtools4 is installed:
# https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html

# Mac user might need to install a Fortran compiler (if errors occur below with renv),
# as described here: https://stackoverflow.com/questions/77822707/renvrestore-failing-to-restore-library-installing-matrix-compilation-failed


### Package version management
# To bolster future reproducibility, we used the *renv* package
# to manage version control of the used packages and dependencies.
# This 'imports' all R packages as the correct versions from the *renv.lock* file.  
install.packages("renv") # agree to restarting R if asked
renv::activate()
renv::restore(packages = "renv") # answer with "y"/"Y"/"Yes" when prompted
# -> restart R (under "Session")
renv::restore() # to revert to the previous state as encoded in the lockfile
# answer with "y"/"Y"/"Yes" when prompted
# this can take some time!

# You can check that the package management was successful, by executing "renv::restore()" again 
# which should now say "The library is already synchronized with the lockfile." 
# (or by running "renv::diagnostics()" or "renv::status()")
# After this, no packages need to be installed manually!


### Generate cleaned data from SoSci-Survey and Prolific data
# This requires you to have access to the raw data files which we don't upload to the OSF 
# for data protection reasons. Due to the identifier variables and open-response text answers
# participants may be identified otherwise.
# If you have access, then run:
#source("clean_data_st1.R") 
#source("clean_data_st2.R") 
#source("clean_data_st3.R") 


### Run analysis scripts and generate html-documents reporting all results

# Study 1:
quarto::quarto_render(input = "improveaccept_results_st1.qmd", 
                      output_file = "improveaccept_results_st1.html", output_format="html") 
# each may take 5-10 min. depending on your machine

# Study 2:
quarto::quarto_render(input = "improveaccept_results_st2.qmd", 
                      output_file = "improveaccept_results_st2.html", output_format="html") 

# Study 3:
quarto::quarto_render(input = "improveaccept_results_st3.qmd", 
                      output_file = "improveaccept_results_st3.html", output_format="html") 

# If any of these throw an error message, open the script within the R-Project and run by hand
