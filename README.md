### README ###

Final manuscript is Paper_1_v6.pdf, compiled in Overleaf using the .tex file of the same name and the .bib and .png files in the main folder.

To replicate all .png figures as well as all statistical analysis, go to Data and run PMv6code.R. 

In PMv6code.R, code through line 1036 is preprocessing of raw data from sources (as described in manuscript) to create file SSAdata_v6_civwar.csv. 
Running file from the beginning will recreate the initial data cleaning and merging process, with final merges and variable creation from 1037-1078.
Analysis begins at 1080.

For faster replication, one can read in the ACD and GED files (lines 50 and 51), clean them (lines 364-381), then run lines 1038 and onwards. 
This is because code up to line 1036 makes SSAdata_v6_civwar.csv, and recreating from beginning can take time and computation power.
