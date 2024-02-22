

scrapeR <- function(url) {
  # Use tryCatch to handle potential errors during web scraping
  page <- tryCatch({
    # Fetch the webpage content with a timeout
    response <- GET(url, timeout(8))
    # Check for HTTP errors
    if (http_error(response)) {
      stop("HTTP request error.")
    }
    # Read and parse the HTML content
    read_html(httr::content(response, "text", encoding = "UTF-8"))
  }, error = function(e) {
    return(NA)  # Return NA if there's an error (including timeout)
  })

  # Check if the page is NA (which indicates an error occurred)
  if (is.na(page)) {
    return(NA)
  }

  # Extract text from specified HTML nodes
  text_elements <- page %>%
    html_nodes("h1, h2, h3, h4, h5, h6, p, title") %>%
    html_text(trim = TRUE)

  # Combine all text elements into one and clean up the text
  combined_text <- paste(text_elements, collapse = " ")
  combined_text <- gsub("\\s+", " ", combined_text)

  return(combined_text)
}

## loop to batch and extract content

scrapeR_in_batches <- function(df, url_column, extract_contacts = FALSE) {
  # function for extracting contact info
  extract_contact_info <- function(combined_text) {
    email_pattern <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"
    phone_pattern <- "\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4}"

    emails <- str_extract_all(combined_text, email_pattern) %>% unlist()
    phones <- str_extract_all(combined_text, phone_pattern) %>% unlist()

    list(emails = paste(unique(emails), collapse = '; '),
         phone_numbers = paste(unique(phones), collapse = '; '))
  }

  batch_size <- 100
  num_rows <- nrow(df)
  num_batches <- ceiling(num_rows / batch_size)
  all_batches <- list()

  for (i in 1:num_batches) {
    message(sprintf("Processing batch %d of %d", i, num_batches))
    batch <- df[((i-1)*batch_size + 1):min(i*batch_size, num_rows), , drop = FALSE]

    batch$content <- sapply(batch[[url_column]], scrapeR, USE.NAMES = FALSE)

    if (extract_contacts) {
      # Directly call extract_contact_info here on the combined_text
      contact_info <- lapply(batch$content, function(combined_text) {
        if (!is.na(combined_text)) {
          return(extract_contact_info(combined_text))
        } else {
          return(list(emails = NA, phone_numbers = NA))
        }
      })

      # Extract emails and phone numbers and add them to the batch dataframe
      batch$emails <- sapply(contact_info, function(info) info$emails)
      batch$phone_numbers <- sapply(contact_info, function(info) info$phone_numbers)
    }

    all_batches[[i]] <- batch

    gc()
    Sys.sleep(1)
  }

  result_df <- do.call(rbind, all_batches)
  message("Processing complete.")
  return(result_df)
}

