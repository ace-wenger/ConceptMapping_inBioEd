source("renv/activate.R")

# # It seems that sometimes renv does not use the same download method as that which R is configured to use
# # How to find out what R is using:
# getOption("download.file.method")
# # How to find out what renv is using:
# renv:::renv_download_method()

# Setting the method to libcurl was working but now I can't seem to download a particular package
Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")

# # This was another suggestion to automatically set the download method to match the R system setting
# Sys.setenv(RENV_DOWNLOAD_METHOD = getOption("download.file.method"))
