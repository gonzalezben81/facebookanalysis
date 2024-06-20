library(testthat)
library(tm)
library(syuzhet)
library(stringr)

# Mock version_replace function
version_replace <- function(major, minor) {
  version_major <- major
  version_minor <- gsub("\\..*", "", minor)
  version_replacement <- paste(version_major, version_minor, sep = '.', collapse = "")
  version_replaced <- as.name(version_replacement)
  return(version_replaced)
}

# Mock rmarkdown::render function
mock_render <- function(input, params, output_file, output_dir, quiet, clean) {
  # Simulate rendering without actually generating a file
  message("Rendering report to: ", file.path(output_dir, output_file))
}

test_that("facebook_sentiment_calculator processes text files correctly", {
  # Create temporary directory and text files
  temp_dir <- tempdir()
  temp_folder <- file.path(temp_dir, "facebook_messages")
  dir.create(temp_folder)
  writeLines("This is a sample text for testing sentiment analysis.", file.path(temp_folder, "message1.txt"))
  writeLines("Another sample text with different content.", file.path(temp_folder, "message2.txt"))
  
  # Mock dependencies
  with_mocked_bindings(
    `rmarkdown::render` = mock_render,
    {
      # Define the function under test
      facebook_sentiment_calculator <- function(folder) {
        filelist <- list.files(path = paste0("./", folder), pattern = '.txt', full.names = TRUE)
        version_type <- version_replace(major = "4", minor = "1.0")
        
        for (f in 1:length(filelist)) {
          text_file_path <- file.path(paste0("./", folder))
          document <- Corpus(DirSource(text_file_path))
          docs <- unlist(document)
          docs <- docs[f]
          file_name <- filelist[f]
          
          value <- get_nrc_sentiment(docs)
          value[is.na(value)] <- 0
          sentimentscores <- round(colSums(prop.table((value[, 1:8])))*100, digits = 1)
          sentimentscores <- as.data.frame(sentimentscores)
          colnames(sentimentscores) <- c("Percentages")
          
          if (!dir.exists("./nrc_sentiment") || !dir.exists('./image')) {
            dir.create("./nrc_sentiment")
            dir.create("./image")
          } else {
            name <- gsub(pattern = './messages', replacement = "", x = file_name)
            name <- gsub(pattern = ".txt*", replacement = "", x = name)
          }
          
          if (is.na(sentimentscores$Percentages)) {
            message("No bar plot created")
          } else {
            name <- gsub(pattern = './messages', replacement = "", x = file_name)
            name <- gsub(pattern = ".txt*", replacement = "", x = name)
            name <- gsub(pattern = "/", replacement = "", x = name)
            myfile_path <- file.path(".", "image", paste0(name, " Positive Vs Negative.pdf"))
            pdf(file = myfile_path)
            barplot_two <- barplot(
              sort(colSums(prop.table(value[, 9:10]))),
              cex.names = 0.7,
              las = 1,
              xlim = c(0, 3),
              ylim = c(0, 1),
              main = paste0(toupper(name), " Positive vs. Negative Sentiment"),
              col = "blue"
            )
            dev.off()
            myfile_path <- file.path(".", "image", paste0(name, " Emotional Sentiment.pdf"))
            pdf(file = myfile_path)
            barplot_one <- barplot(
              sort(colSums(prop.table(value[, 1:8]))),
              cex.names = 0.7,
              las = 1,
              xlim = c(0, 9),
              ylim = c(0, .40),
              main = paste0(toupper(name), " Emotional Sentiment by Word"),
              col = "lightgreen"
            )
            text(barplot_one, 0, round(sort(colSums(prop.table(value[, 1:8]))), 2), cex = 1, pos = 3)
            dev.off()
          }
          
          rmarkdown::render(
            input = paste0("~\\R\\win-library\\", version_type, "\\facebookanalysis\\rmd\\facebook.Rmd"),
            params = list(table = value, docs = docs, sentiment = sentimentscores, set_author = name),
            output_file = paste0(name, " Rmarkdown"),
            output_dir = "nrc_sentiment", quiet = TRUE, clean = TRUE
          )
          message("Facebook Analysis Report for ", paste0(name, " pdf file created"))
        }
      }
      
      # Run the function with the temporary folder
      facebook_sentiment_calculator("facebook_messages")
      
      # Check if output directories and files are created
      expect_true(dir.exists(file.path(".", "nrc_sentiment")))
      expect_true(dir.exists(file.path(".", "image")))
      
      # Clean up temporary files and directories
      unlink(temp_folder, recursive = TRUE)
    }
  )
})
