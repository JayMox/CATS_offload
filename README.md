# CATS_offload
Scripts for offloading CATS camera tags &amp; conducting basic EDA

Built for files offloaded June 22 2017 on deployments in South Africa.  Plot depth trace & camera recording to evaluate performance of the onboard behavioral trigger for camera recording.  

July 7 Update
-standardized CATS data uploading script is still needs to be developed; currently individualized scripts for reading in data, cleaning up (mostly depth data), and evaluating CATS trigger
-nb.: depth is smoothed w/ a wider moving avg (i.e., filter = rep(1,5*datFreq)/(5*datFreq)); whereas VV is smoothed with a more narrow moving sum (i.e., filter = rep(1,5))
-CATS trigger is designed to fire when the range difference over previous 1s is greater than a threshold (0.2 for most of these)
