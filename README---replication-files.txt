
README to accompany the replication files for Michael A. Clemens, Ethan G. Lewis, and Hannah M. Postel, "Immigration Restrictions as Active Labor Market Policy: Evidence from the Mexican Bracero Exclusion", created in Stata version 15.1.

You can replicate all the tables and figures in the main paper and the Online Appendix by using Stata to run the file "bracero_aer_code.do".

Running the code requires, as noted in the code, 1) installing five specified .ado files (-bspline-, -grc1leg-, -xtsemipar-, -sutex-, and -outtable-), 2) changing the filepath near the start of the code to your working directory, 3) creating a folder called "output" in your working directory, and 4) placing all six of the required, accompanying data files in the working directory. The required data files are: 
- bracero_aer_dataset.dta : This contains the main data
- bracero_outflows_from_mex_gonzalez.dta : This contains bracero outflows from Mexico
- cpi_data.dta : Consumer Price Index to convert nominal to real wages
- tomatoes_vandermeer_final.dta : Data on tomator harvester adoption from Vandermeer
- total_braceros_by_year.dta : Data on bracero flows by year
- alston_ferrie_votes.dta : Data on U.S. House of Representatives votes, from Alton and Ferrie

The code generates a single LaTeX file, "bracero_tables.tex", containing all of the tables in the main paper and Online Appendix, *except* Appendix Table A1 (descriptive statistics) which is output to a separate file named "summ_stats.txt". The table numbers are noted both in the Stata code and in the LaTeX output.

The code generates the figures, in both the main paper and Online Appendix, as a series of separate PDF files. The correspondence between these files and the figure numbers in the text and Appendix is visually clear, but is also annotated in the Stata code.

Published December 11, 2017



