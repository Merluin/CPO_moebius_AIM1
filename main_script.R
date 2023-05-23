###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        0382022
#  Description: Generate the dataset from Gorilla (https://app.gorilla.sc/) 
#  Experiment CPO_moebius_AMIM1
#
#  Update:      23/08/2022
###########################################################################

rm(list=ls()) # remove all objects

# Functions ---------------------------------------------------------------

devtools::load_all()

# Running the entire analysis

create_dir_structure()

# Pre-processing

run_script("scripts/01_dataset_builder.R")

# Models

run_script("scripts/02a_bais.R")
run_script("scripts/02b_uncertainty.R")
run_script("scripts/02c_intensity.R")
run_script("scripts/02d_accuracy.R")
# Tables

run_script("scripts/03_tables.R")

# Figures

run_script("scripts/04_plots.R")







#################################################
# 
# END
#
#################################################