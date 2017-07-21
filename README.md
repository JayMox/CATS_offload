# CATS_offload
Generalized scripts for downloading, pre-processing, and plotting the data record, trigger flags, and camera codes.  All scripts written by Jerry Moxley, June-July 2017.  These scripts were initalized with data offloaded from May/June deployments on white sharks in South Africa (projID SA2017).

Data offloading is completed via the CATS custom program, available in the Untitled data drive under the folder "CamTag".  Typically data records are downoaded in smaller chunks (e.g., multiple .csv files 128MB each).

CATSCamTag_preprocessing.R is a script to load in & bind individual .csv and reclass important data variables.  As of July 20 2017, this script builds local & UTC time stamps, builds a field representing programmed duty cycle blocks (field name: dc_prog), smooths depth data over 5 secs, calcs & smooths Vertical Velocity over 1s, & simulates the CATS on-board trigger algorithm (field name: trig).  Data is saved as a .RData at the full resolution, and optionally downsampling through the datFreq.desired parameter.  

Future work should focus on ensuring the Camera/Flags codes are factorized properly (stringsAsFactors should be set FALSE on data load in).  Code for this is in CATS_codes_plots.  There should also be a statement for renaming columns with abbreviated colnames.  A field for unique deployment identifiers should also be included.  



CATS_codes_plot takes pre-processed data & plots the time series of camera activity, VV behavioral triggers, and Camera & Trigger Flag codes.  This is done for the full dataset, and then at 3hr blocks for closer inspection.  