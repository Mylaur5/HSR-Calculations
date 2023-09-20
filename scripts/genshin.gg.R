librarian::shelf(magrittr, rvest, httr2, tidyverse, vroom)

# Source this file once to update database
extract_genshin_data <- function(url) {
  # Make an HTTP GET request to the URL
  req <- request(url)

  # Execute the request and get the response
  response <- req %>% req_perform()

  # Extract the HTML content as a character string
  html_body <- response %>% resp_body_string()

  # Parse the HTML content using rvest
  html_document <- read_html(html_body)

  # Work with HTML elements
  extracted_text <- html_document %>%
    html_element(".content") %>%
    html_text2()

  # Split the string into lines
  lines <- strsplit(extracted_text, "\n")[[1]]
  lines <- lines[lines != ""] # Remove empty lines
  return(lines)
}

process_stats <- function(data, output_file) {
  # Determine the number of header rows
  # Remove first and last element
  data <- data[-c(1, length(data))]

  temp_data <- stringr::str_detect(data, "[:alpha:]")

  # Find the position of the first FALSE
  first_false_position <- which(!temp_data)[1]

  # Determine the number of header rows
  num_header_rows <- first_false_position - 2
  # 2 Because the FALSE is a number and the last TRUE is a LC/Character data
  column_names <- unlist(strsplit(data[1:num_header_rows], "\t"))

  # Data processing
  # Remove headers
  data <- data[-c(1:num_header_rows)]
  # Iterate on the vector
  chunks <- split(data, rep(1:ceiling(length(data) / num_header_rows), each = num_header_rows))
  final_data_tibble <- purrr::map(chunks, ~ tibble::as_tibble_row(.x, .name_repair = "minimal") %>%
    magrittr::set_colnames(column_names)) %>%
    purrr::list_rbind() %>%
    dplyr::arrange(across(1))

  # Get the name of the first column
  first_column_name <- names(final_data_tibble)[1]

  # Remove "NEW" from the first column using stringr
  final_data_tibble <- final_data_tibble %>%
    mutate(!!first_column_name := str_replace(.data[[first_column_name]], "NEW$", ""))

  # Output file
  readr::write_tsv(final_data_tibble, output_file)
}

# Match and extract data for light cone
match_and_extract_data <- function(N, data_vector, file_path, output_file_path) {
  # Read data from the file
  lc_stats <- read_tsv(file_path)

  # Function to match and extract the n+1 text
  match_and_extract <- function(data, data_vector, offset) {
    map_chr(data, ~ {
      match_index <- match(.x, data_vector)
      if (!is.na(match_index) && match_index + offset <= length(data_vector)) {
        return(data_vector[match_index + offset])
      } else {
        return(NA_character_)
      }
    })
  }

  # Apply the function to lc_stats
  lc_stats <- lc_stats %>%
    mutate(lc_path = match_and_extract(`Max Lv. Light Cones`, data_vector, N))
  write_tsv(lc_stats, output_file_path)

  return(lc_stats)
}


# Get character stats and light cone stats

extract_genshin_data("https://genshin.gg/star-rail/character-stats/") %>%
  process_stats("data/character_stats_df.tsv")
extract_genshin_data("https://genshin.gg/star-rail/light-cone-stats/") %>%
  process_stats("data/light_cone_stats_df.tsv")


# Get light_cone_data with Path
lc_data <- extract_genshin_data("https://genshin.gg/star-rail/light-cones/")
lc_stats <- vroom("data/light_cone_stats_df.tsv")

lc_data <-
  extract_genshin_data("https://genshin.gg/star-rail/light-cones/")
lc_stats_result <-
  match_and_extract_data(
    N = 1,
    data_vector = lc_data,
    file_path = "data/light_cone_stats_df.tsv",
    output_file_path = "data/light_cone_data.tsv"
  )



# Extract character list and data to get Path
extract_character_list <- function(url, file_path) {
  req <- request(url)

  response <- req %>% req_perform()

  html_body <- response %>% resp_body_string()

  html_document <- read_html(html_body)

  extracted_url <-
    html_document %>%
    html_elements(".character-list a") %>%
    html_attr("href") %>%
    paste0("https://genshin.gg", .) %>%
    tibble(url = .) %>%
    arrange(across(1))

  character_data <- vroom("data/character_stats_df.tsv") %>%
    bind_cols(extracted_url)


  extract_genshin_data(character_data$url[1])[2]

  character_data <- character_data %>%
    mutate(character_path = map_chr(url, ~ extract_genshin_data(.x)[2]))
  write_tsv(character_data, file_path)
}

extract_character_list("https://genshin.gg/star-rail/", "data/character_data.tsv")
