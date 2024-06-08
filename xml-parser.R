options(repos = c(CRAN = "https://cloud.r-project.org"))
packages <- c("xml2", "optparse")
for (pack in packages) {
  if (!require(pack)) {
    install.packages(pack)
  }
}
library(xml2)
library(optparse)

SUFFIX <- "df"
EXTENSION <- ".RData"
SLEEP_TIME <- 5


take_input <- function(prompt) {
  if (interactive()) {
    input <- readline(prompt)
  }
  else {
    cat(prompt)
    input <- readLines("stdin", n = 1)
  }
  input <- gsub("^[\'\" ]+|[\'\" \n]+$", "", input)
  return(input)
}

main <- function() {
  # Sessions will only differ by the mean of taking input and necessity to parse command-line args
  
  if (!(interactive())) {
    # Parse command-line arguments
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
    
    input_file <- args$options$input
    output_file <- args$options$output
    
    paths <- prepare_paths_for_parsing(args$args[[1]])
    df <- generate_df(paths)
    
    # Append data frame to dataframe from input file
    if (!is.null(input_file) &&
        file.exists(input_file) &&
        tools::file_ext(input_file) == "RData") {
      df_new <- df
      load(input_file)
      df <- rbind(df, df_new)
    }
    
    # Generate default name for .RData file (timestamp+suffix+extension)
    if (is.null(output_file)) {
      output_file <- generate_filename(SUFFIX, EXTENSION)
    }
    else {
      # Check for attempt to overwrite existing .RData file
      if (file.exists(output_file)) {
        overwrite_query <- get_yn("Careful, this file exists and will be replaced. Proceed?\n(y)es/(n)o: ")
        if (!overwrite_query) {
          output_file <- generate_filename(SUFFIX, EXTENSION)
        }
      }
    }
    save(df, file = output_file)
    rm(list = ls(all.names = TRUE))
  }
  else {
    # Get necessary values from the console when in interactive session
    path <- take_input("select path: ")
    
    valid_paths <- prepare_paths_for_parsing(path, "xml")
    
    df <- generate_df(valid_paths)
    
    input_file <- take_input("Write a name of .RData file to append dataframe to (skip to create new): ")
    
    if (input_file != "" &&
        file.exists(input_file) &&
        tolower(tools::file_ext(input_file)) == "rdata") {
      df_new <- df
      load(input_file)
      df <- rbind(df, df_new)
    }
    
    output_file <- take_input("Write a name of .RData file to save dataframe to (skip for default name): ")
    
    # Generate timestamp if there is no predetermined name
    if (output_file == "") {
      output_file <- generate_filename(SUFFIX, EXTENSION)
    }
    else {
      # Check for attempt to overwrite existing .RData file
      if (file.exists(output_file)) {
        overwrite_query <- get_yn("Careful, this file exists and will be replaced. Proceed?\n(y)es/(n)o: ")
        if (!overwrite_query) {
          output_file <- generate_filename(SUFFIX, EXTENSION)
        }
      }
    }
    save(df, file = output_file)
    rm(list = ls(all.names = TRUE))
    
  }
}


generate_filename <- function(suffix, extension) {
  # create timestamp
  now <- sprintf("%s%s%s",
                 format(Sys.time(), "%Y-%m-%d_%H%M%S"),
                 suffix,
                 extension)
}

get_yn <- function(prompt) {
  repeat {
    answer <- take_input(prompt)
    
    if (answer %in% c('y', 'yes')) {
      return(TRUE)
    }
    else if (answer %in% c('n', 'no')) {
      return(FALSE)
    }
  }
}

resolve_invalid_path <- function(path) {
  print(sprintf("File doesn't exist or can't be parsed: %s\n", path))
  repeat {
    choice <- tolower(take_input("(s)kip the file or (q)uit: "))
    if (choice %in% c("s", "skip")) {
      return(TRUE)
    }
    else if (choice %in% c("q", "quit")) {
      stop() # ? change to return
    }
  }
}

prepare_paths_for_parsing <-
  function(path, valid_extensions = c("xml")) {
    valid_paths <- c()
    if (!file.exists(path)) {
      skip <- resolve_invalid_path(path)
      if (skip) {
        return(valid_paths)
      }
    }
    extension <- tools::file_ext(path)
    if (extension %in% valid_extensions) {
      valid_paths <- c(valid_paths, path)
    }
    else if (extension %in% c("txt", "list")) {
      list_ <- readLines(path)
      for (el in list_) {
        valid_path <- prepare_paths_for_parsing(el)
        valid_paths <- c(paths, valid_path)
      }
    }
    else {
      resolve_invalid_path(path)
    }
    return(valid_paths)
  }

xml_parse <- function(path) {
  # ...
  msg <- sprintf("Now parsing: %s.", path)
  print(msg)
  in_file <- read_xml(path)
  
  # Get experiment name from XML
  experiment_name <- xml_text(xml_child(xml_child(in_file, search = "Image_Properties"), search = "Image_Filename"))
  
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
