options(repos = c(CRAN = "https://cloud.r-project.org"))
packages <- c("xml2")
for (pack in packages) {
  if (!require(pack)) {
    install.packages(pack)
  }
}
library(xml2)

SUFFIX <- "df"

main <- function() {
  path <- readline("select path: ")
  path <- gsub("^[\'\" ]+|[\'\" \n]+$", "", path)
  
  valid_paths <- prepare_paths_for_parsing(path, "xml")
  
  df <- generate_df(valid_paths)
  
  input_file <- readline("Write a name of .RData file to append dataframe to (skip to create new): ")
  input_file <- gsub("^[\'\" ]+|[\'\" \n]+$", "", input_file)
  
  if (input_file != "" &&
      file.exists(input_file) &&
      tolower(tools::file_ext(input_file)) == "rdata") {
    df_new <- df
    load(input_file)
    df <- rbind(df, df_new)
  }
  
  # Handle output file
  output_file <- readline("Write a name of .RData file to save dataframe to (skip for default name): ")
  output_file <- gsub("^[\'\" ]+|[\'\" \n]+$", "", output_file)
  
  # Generate timestamp if there is no predetermined name
  if (output_file == "") {
    output_file <- generate_filename(SUFFIX)
  }
  # TODO: Confirm overwrite if file with output_file name already exists

  
  save(df, file = output_file)
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
      return(valid_paths)
    }
    else if (extension %in% c("txt", "list")) {
      list_ <- readLines(path)
      for (el in list_) {
        valid_path <- prepare_paths_for_parsing(el)
        valid_paths <- c(valid_paths, valid_path)
      }
      return(valid_paths)
    }
    else {
      skip <- resolve_invalid_path(path)
      if (skip) {
        return(valid_paths)
      }
    }
  }

resolve_invalid_path <- function(path) {
  print(sprintf("File doesn't exist or can't be parsed: %s\n", path))
  repeat {
    choice <- tolower(readline("(s)kip the file or (q)uit: "))
    if (choice %in% c("s", "skip")) {
      return(TRUE)
    }
    else if (choice %in% c("q", "quit")) {
      stop() # change to early return
    }
  }
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
  
  # Create data frame
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



generate_filename <- function(suffix) {
  # create timestamp
  now <- sprintf("%s%s%s", 
                 format(Sys.time(), "%Y-%m-%d_%H%M%S"),
                 suffix,
                 ".RData")
}


main()
rm(list = ls(all.names = TRUE))
