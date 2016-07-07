
#' @title Download RStudio CRAN mirror data files into a folder(not package data but R data)
#' @export
#' @description
#' @param START the defaults is 5 days before today. A character string of the START date for files to be downloaded. The date format is "YYYY-MM-DD".
#' @param END the defaults is today. A character string of the END date for files to be downloaded. The date format is "YYYY-MM-DD".
#' @param log_folder the folder into which we would like the files to be downloaded to. Default is the temporary folder picked by \link{tempdir}.
#' @param trunc_END_date_to_today default is TRUE. Makes sure that if END date is later then today,
#'  the END date will be change to today
#'  (since otherwise, we will only get many 404 errors)
#' @param override boolean (default is FALSE) - should the function download files that
#' are already available in the temp folder
#' @param message boolean (default is TRUE) - should a message be printed in interesting cases.
#' @param ... not in use.
#' @return Returns the value of log_folder.
download_RStudio_CRAN_data_r <- function(START = as.Date(Sys.time())-5,
                                         END = as.Date(Sys.time()),
                                         log_folder = tempdir(),
                                         trunc_END_date_to_today = TRUE,
                                         override = FALSE,
                                         message = TRUE,
                                         ...) {
    # Here's an easy way to get all the URLs in R
    START <- as.Date(START)
    END <- as.Date(END)

    # If END date is much further away than today (based on system definitions), it should be tka
    if((END > as.Date(Sys.time())+1) & trunc_END_date_to_today) END <- as.Date(Sys.time())+1 # the +1 is just for the case of a difference in times between the computer and the RStudio server.

    all_days <- seq(START, END, by = 'day')

    year <- as.POSIXlt(all_days)$year + 1900
    urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '-r.csv.gz')
    # You can then use download.file to download into a directory.

    # If you only want to download the files you don't have, try:
    missing_days <- setdiff(all_days, tools::file_path_sans_ext(dir(), TRUE))


    avilable_files <- list.files(log_folder)

    # download files
    for(i in seq_along(urls)) {
        zip_filename <- file.path(file.name.from.url(urls[i]))
        zip_filename_path <- file.path(log_folder, zip_filename)

        # if the file is here, and I should NOT override - then skip
        if(zip_filename %in% avilable_files & !override) {
            if(message) message("The file: ", zip_filename, " is already available in the folder - skipping it")
            # do nothing - skip
        } else { # download
            tryCatch(download.file(urls[i], destfile=zip_filename_path, mode = 'wb'), error = function(e) e)
        }
    }

    if(message) message("Files were downloaded to: ", log_folder)

    return(invisible(log_folder))
}



#' @title Reads RStudio CRAN mirror data files from a folder(not package data but R data)
#' @export
#' @description
#' @param log_folder the folder which contains the RStudio CRAN log files that were downloaded to. Default is the temporary folder picked by \link{tempdir}.
#' @param use_data_table default is TRUE. A switch for wether or not to use the data.table package
#'        in order to merge the log files using rbindlist. This function is MUCH faster then the alternative.
#' @param packages a character vector containing the names of packages for which information is extracted.
#'        If not specified, all packages are included, but this can cause out-of-memory problems if
#'        there are many log files.
#' @param ... not in use.
#' @return Returns the combined data file.
read_RStudio_CRAN_data_r <- function(log_folder = tempdir(), use_data_table = TRUE, packages,  ...) {
    file_list <- file.path(log_folder, list.files(log_folder))
    file_list <- file_list [ grep("[0-9]+-[0-9]+-[0-9]+-r.csv.gz", file_list)] # include only the relevant type of files, such as: "2013-04-02.csv.gz"

    # removes empty files
    file_list_info <- file.info(file_list)
    #    colnames(file_list_info)
    ss_non_0_files <- file_list_info$size > 0
    file_list <- file_list[ss_non_0_files]
    # this version is slower

    # read files
    logs <- list()
    for (file in file_list) {
        cat(paste("Reading", file, "...\n")); flush.console()
        logfile <- read.table(file, header = TRUE, sep = ",", quote = "\"",
                              dec = ".", fill = TRUE, stringsAsFactors = FALSE,
                              comment.char = "", as.is=TRUE)
        logs[[file]] <- logfile
    }


    # rbind the files.
    if(use_data_table) is_data_table_loaded <- require2("data.table")
    if(use_data_table & is_data_table_loaded) {
        dataset <- data.table::rbindlist(logs) # MUCH faster...
    } else {
        dataset <- do.call("rbind",logs)
    }

    if(("data.table" %in% class(dataset))) {
        dataset <- as.data.frame(dataset)
    }

    return(dataset)
}







