

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

scrapeR_in_batches <- function(df, url_column, output_file) {
  batch_size <- 100
  num_rows <- nrow(df)
  num_batches <- ceiling(num_rows / batch_size)

  for (i in 1:num_batches) {
    message("Processing batch ", i, " of ", num_batches)
    start_row <- (i - 1) * batch_size + 1
    end_row <- min(i * batch_size, num_rows)

    batch <- df[start_row:end_row, ]
    batch$content <- sapply(batch[[url_column]], scrapeR, USE.NAMES = FALSE)

    write.table(batch, file = output_file, sep = ",", row.names = FALSE,
                col.names = i == 1, append = i != 1)

    # Clear memory
    rm(batch)
    gc()

    # Throttle requests
    Sys.sleep(1)
  }
  message("Processing complete.")
}



