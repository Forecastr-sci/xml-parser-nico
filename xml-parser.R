options(repos = c(CRAN = "https://cloud.r-project.org"))
packages <- c("xml2", "optparse")
for (pack in packages) {
  if (!require(pack)) {
    install.packages(pack)
  }
}
library(xml2)
library(optparse)

PREFIX <- "df"
EXTENSION <- ".RData"
SLEEP_TIME <- 5

main <- function() {
  option_list <- list(
    make_option(
      c("-i", "--input"),
      type = "character",
      help = "Append results to existing .RData file",
      default = NULL
    ),
    make_option(
      c("-o", "--output"),
      type = "character",
      help = "Output RData file",
      default = NULL
    )
  )
  parser <-
    OptionParser(usage = "%prog <PATH TO XML|TXT> [OPTIONS]", option_list = option_list)
  
  args <- parse_args(parser, positional_arguments = 1)
  
  paths <- prepare_paths_for_parsing(args$args[[1]])
  
  df <- generate_df(paths)
  
  input_file <- args$options$input
  output_file <- args$options$output
  
  # Parse other CLI arguments
  if (!is.null(input_file) &&
      file.exists(input_file) &&
      tools::file_ext(input_file) == "RData") {
    df_new <- df
    load(input_file)
    df <- rbind(df, df_new)
  }
  fname <- ""
  if (!is.null(output_file)) {
    if (!file.exists(output_file)) {
      fname <- output_file
    } else {
      print("Careful, this file exists and will be replaced")
      print(
        sprintf(
          "Press Ctrl-C in %i seconds to abort the program if you want to prevent this",
          SLEEP_TIME
        )
      )
      Sys.sleep(SLEEP_TIME)
      fname <- output_file
    }
  }
  
  
  if (fname == "") {
    fname <-
      sprintf("%s%s%s",
              format(Sys.time(), "%Y-%m-%d_%H%M%S"),
              PREFIX,
              EXTENSION)
  }
  framename <- sprintf("%s%s", format(Sys.time(), "%Y-%m-%d_%H%M%S"), "df")
  save(list = c(framename = "df"), file = fname)
  rm(list = ls(all.names = TRUE))
}


prepare_paths_for_parsing <-
  function(args, valid_extensions = c("xml")) {
    #` Checks for the existence of a file presented in argument.
    #` If it's a txt or list file, assumes it's a list of paths,
    #` checks validity of each path as xml file and adds them to the list of paths.
    #` If it's an xml file, checks for it validity and adds it to the list of paths.
    #` If at any point it encounters invalid path, it prompts user to continue by pressing (y)es
    #` or stops the program if person pressed (n)o.
    #`
    #` Function returns vector of strings representing paths to be parsed
    
    paths <- c()
    if (file.exists(args)) {
      extension <- tools::file_ext(args)
      if (extension %in% valid_extensions) {
        paths <- c(paths, args)
      }
      else if (extension == "txt" || extension == "list") {
        list_ <- readLines(args)
        for (el in list_) {
          valid_path <- prepare_paths_for_parsing(el)
          paths <- c(paths, valid_path)
        }
      }
    } else {
      warning <-
        sprintf("File doesn't exist or can't be parsed: %s\n", args)
      print(warning)
      print(
        sprintf(
          "Press Ctrl-C in %i seconds, to stop the program, or files will be skipped",
          SLEEP_TIME
        )
      )
      Sys.sleep(SLEEP_TIME)
    }
    return(paths)
  }

xml_parse <- function(path) {
  # ...
  msg <- sprintf("Now parsing: %s.", path)
  print(msg)
  in_file <- read_xml(path)
  
  # Get experiment name from XML
  experiment_name <- xml_text(xml_child(xml_child(in_file,
                                                  search = "Image_Properties"),
                                        search = "Image_Filename"))
  
  # Get list of markers from XML
  markers <-
    xml_find_all(xml_child(in_file, search = "Marker_Data"), ".//Marker_Type")
  
  marker_names <- c()
  cell_count <- c()
  for (marker in markers) {
    marker_names <-
      c(marker_names, xml_text(xml_child(marker, search = "Name")))
    cell_count <-
      c(cell_count, length(xml_find_all(marker, ".//Marker")))
    marked_counts <- setNames(cell_count, marker_names)
  }
  
  # Create dataframe
  df <- data.frame(t(marked_counts))
  rownames(df) <- experiment_name
  
  
  return(df)
}

generate_df <- function(paths) {
  df <- data.frame()
  for (path in paths) {
    addition <- xml_parse(path)
    df <- rbind(df, addition)
  }
  return(df)
}


main()
