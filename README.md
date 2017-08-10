# clip_time_series

Step 1: run "script1_prepare_box.R" : reads the point file and prepares the download areas to be used as fusion tables

Step 2: in your Google Drive, import the 3 KML files as fusion tables, make them public, and replace their FT_ID inside the following script 
https://code.earthengine.google.com/be9308f40b2380eb4640fbcfa4e23ef3

Step 3: run the GEE script and download the data to a hard drive, via your google-drive. Depending on the space available in your google-drive, you may have to breakdown the download into different batches

Step 4: run "script2_clip_time_series.R": reads the points file again, reads the available archive of imagery and produces the clips with the desired parameters

