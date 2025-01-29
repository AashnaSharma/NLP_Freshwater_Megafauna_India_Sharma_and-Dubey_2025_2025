####                       ### LOOPING FOR MULTIPLE PDFS ###                ####

#### Step A : Before Python: Extract the pages from each PDF containing just the discussion section ####
install.packages("openxlsx")

library(pdftools)
library(stringr)
library(openxlsx)

# Function to extract sections from PDF and create new PDF
extract_sections_and_crop <- function(pdf_file, output_folder) {
  # Error handling
  tryCatch({
    # Read all pages of the PDF
    pdf_text <- pdf_text(pdf_file)
    
    # Find the starting page
    start_page <- min(which(grepl("(?is)\\b(Methods\\s+and\\s+Discussion|Discussion|Results\\s+and\\s+Discussion)\\b", pdf_text, perl = TRUE)))
    if (is.infinite(start_page)) {
      cat("Start page not found in file:", basename(pdf_file), "\n")
      return(NULL)
    }
    
    # Find the ending page
    end_page <- min(which(grepl("(?is)\\b(References|Literature\\s+Cited|Acknowledgments)\\b", pdf_text, perl = TRUE)))
    if (is.infinite(end_page)) {
      cat("End page not found in file:", basename(pdf_file), "\n")
      return(NULL)
    }
    
    # Crop PDF to pages containing the section
    pdf_subset(pdf_file, pages = start_page:end_page, output = file.path(output_folder, basename(pdf_file)))
    cat("Section extracted and saved to:", basename(pdf_file), "\n")
    
  }, error = function(e) {
    cat("Error processing file:", basename(pdf_file), "\n")
  })
}

# Main function to process PDF files
main <- function(input_folder, output_folder) {
  pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)
  for (pdf_file in pdf_files) {
    extract_sections_and_crop(pdf_file, output_folder)
  }
}

# Set working directory and folder paths
input_folder <- "G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/New_species_added"
output_folder <- "G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Additional/Additional_output"

main(input_folder, output_folder)
##### Add all PDFs into a common folder with the naming convention "Species name_Author name_year"
#### Step B : Code to run on Python: Splitting the PDFs and rearranging the columns ####
#### The code snippet that follows is to be run in python and not R
import fitz  # PyMuPDF
import os

def split_pdf_vertically(input_pdf_path, merged_output_pdf_path):
  # Open the input PDF
  input_pdf = fitz.open(input_pdf_path)

# Create new PDF documents for the left and right halves
left_pdf = fitz.open()
right_pdf = fitz.open()

# Iterate through all the pages
for page_num in range(len(input_pdf)):
  # Get the page
  page = input_pdf.load_page(page_num)

# Get the media box (the size of the page)
width, height = page.rect.width, page.rect.height

# Define the rectangles for the left and right halves
left_rect = fitz.Rect(0, 0, width / 2, height)
right_rect = fitz.Rect(width / 2, 0, width, height)

# Create new pages for the left and right halves
left_page = left_pdf.new_page(width=width / 2, height=height)
right_page = right_pdf.new_page(width=width / 2, height=height)

# Copy content from the original page to the left and right pages
left_page.show_pdf_page(left_rect, input_pdf, page_num, clip=left_rect)
right_page.show_pdf_page(fitz.Rect(0, 0, width / 2, height), input_pdf, page_num, clip=right_rect)

# Create a new PDF for merging
merged_pdf = fitz.open()

# Add pages from left and right PDFs alternatively
for page_num in range(len(left_pdf)):
  merged_pdf.insert_pdf(left_pdf, from_page=page_num, to_page=page_num)
merged_pdf.insert_pdf(right_pdf, from_page=page_num, to_page=page_num)

# Save the merged PDF
merged_pdf.save(merged_output_pdf_path)
print(f"Merged PDF saved: {merged_output_pdf_path}")

def process_pdfs_in_folder(input_folder_path, output_folder_path):
  # Create the output folder if it does not exist
  os.makedirs(output_folder_path, exist_ok=True)

# Process each PDF file in the input folder
for filename in os.listdir(input_folder_path):
  if filename.lower().endswith('.pdf'):
  input_pdf_path = os.path.join(input_folder_path, filename)
merged_output_pdf_path = os.path.join(output_folder_path, filename)

print(f"Processing {filename}...")
split_pdf_vertically(input_pdf_path, merged_output_pdf_path)

# Usage
input_folder_path = r'G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Additional_output'
output_folder_path = r'G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Additional_Split_PDFs'

process_pdfs_in_folder(input_folder_path, output_folder_path)

####



#### Step 1 : Removing headers and footers, extracting the discussion sections after PDF splitting and column rearrangement in Python####
# Load required packages
library(pdftools)
library(stringr)

# Function to detect common header lines
detect_header <- function(pdf_content, num_lines = 5) {
  # Extract first few lines from each page
  first_lines <- lapply(pdf_content, function(page_text) {
    lines <- unlist(strsplit(page_text, "\n"))
    return(lines[1:num_lines])
  })
  
  # Find common lines among the first few lines
  common_lines <- Reduce(intersect, first_lines)
  
  return(common_lines)
}

# Function to detect common footer lines
detect_footer <- function(pdf_content, num_lines = 5) {
  # Extract last few lines from each page
  last_lines <- lapply(pdf_content, function(page_text) {
    lines <- unlist(strsplit(page_text, "\n"))
    return(tail(lines, num_lines))
  })
  
  # Find common lines among the last few lines
  common_lines <- Reduce(intersect, last_lines)
  
  return(common_lines)
}

# Function to crop header and footer from PDF
crop_header_footer <- function(pdf_content, header_lines, footer_lines) {
  # Determine header and footer height
  header_height <- length(header_lines)
  footer_height <- length(footer_lines)
  
  # Crop header and footer from each page
  cropped_content <- lapply(pdf_content, function(page_text) {
    lines <- unlist(strsplit(page_text, "\n"))
    cropped_lines <- lines[(header_height + 1):(length(lines) - footer_height)]
    return(paste(cropped_lines, collapse = "\n"))
  })
  
  # Convert cropped content to a single character vector
  cropped_content <- unlist(cropped_content)
  
  return(cropped_content)
}

# Function to extract discussion section from PDF
extract_discussion <- function(pdf_text) {
  discussion <- str_extract(pdf_text, "(?is)(?:\\bmethods\\s+and\\s+discussion\\b|\\bresults\\s+and\\s+discussion\\b|\\bmethodology\\s+and\\s+discussion\\b|\\bdiscussion\\b)(.*?)(?:\\breferences\\b$|\\bAcknowledgments\\b|\\bAcknowledgements\\b|\\bConclusion\\b|\\bliterature\\s+cited\\b|\\bEthical\\s+Approval\\b|\\bData\\s+Availability\\b|$)")
  return(discussion)
}

# Main function to process a single PDF file
process_single_pdf <- function(input_file, output_file) {
  # Read PDF content
  pdf_content <- pdf_text(input_file)
  
  # Detect header and footer lines
  header_lines <- detect_header(pdf_content)
  footer_lines <- detect_footer(pdf_content)
  
  # Crop header and footer from PDF content
  cropped_content <- crop_header_footer(pdf_content, header_lines, footer_lines)
  
  # Concatenate cropped content into a single text
  cropped_text <- paste(cropped_content, collapse = " ")
  
  # Extract discussion section
  discussion <- extract_discussion(cropped_text)
  
  # Save the discussion section to a text file
  if (!is.null(discussion)) {
    writeLines(discussion, output_file)
    cat("Discussion section extracted and saved to:", output_file, "\n")
  } else {
    cat("Discussion section missing. Skipping file:", basename(output_file), "\n")
  }
}

# Set the paths for the input PDF and output text files
input_folder <- "G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Additional/Additional_Split_PDFs"
output_folder <- "G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Additional/Additional_Step_1"

# Get list of PDF files in the input folder
pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)

# Loop through each PDF file
for (pdf_file in pdf_files) {
  # Create output file path
  output_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(pdf_file)), ".txt"))
  
  # Process the single PDF file
  process_single_pdf(pdf_file, output_file)
}

#### Step 2 : Removing the Figure Captions, Table legends and the Tables ####

# Load necessary library
library(stringr)

# Function to remove captions and legends from a text file
remove_captions_and_legends <- function(file_path, output_file_path) {
  lines <- readLines(file_path)
  modified_lines <- character(0)
  skip_next_line <- FALSE
  for (line in lines) {
    # Check if line starts with "Table", "Figure", or "Fig." (case insensitive)
    if (grepl("^\\s*(table|figure|fig\\.)", line, ignore.case = TRUE)) {
      skip_next_line <- TRUE
    } else if (!skip_next_line) {
      modified_lines <- c(modified_lines, line)
    } else {
      skip_next_line <- FALSE
    }
  }
  writeLines(modified_lines, output_file_path)
}

# Function to count the number of columns in each line
count_columns <- function(line) {
  # Split the line by whitespace
  columns <- strsplit(line, "(?<=\\S)\\s{5,}(?=\\S)", perl = TRUE)[[1]]  # separated by 5 or more consecutive spaces preceded and followed by a non-space character
  
  # Return the count of columns
  return(length(columns))
}

# Function to filter lines with 2 or more columns
filter_columns <- function(line) {
  return(count_columns(line) <= 1) #CHANGED >=2 to >=1 AS THE NUMBER OF COLUMNS REDUCES TO 1 AFTER PDF SPLITTING AND COLUMN REARRANGEMENT
}

# Function to process a single text file
process_text_file <- function(input_file, output_file) {
  # Read file content 
  text <- readLines(input_file)
  
  # Remove captions and legends
  filtered_lines <- character(0)
  skip_next_line <- FALSE
  for (line in text) {
    # Check if line starts with "Table", "Figure", or "Fig." (case insensitive)
    if (grepl("^\\s*(table|figure|fig\\.)", line, ignore.case = TRUE)) {
      skip_next_line <- TRUE
    } else if (!skip_next_line) {
      filtered_lines <- c(filtered_lines, line)
    } else {
      skip_next_line <- FALSE
    }
  }
  
  # Filter lines with 1 or more columns
  filtered_lines <- filtered_lines[sapply(filtered_lines, filter_columns)]
  
  # Write the filtered lines to a new file
  writeLines(filtered_lines, output_file)
  
  cat("Filtered text saved to:", output_file, "\n")
}

# Set the paths for the input and output folders
input_folder <- "G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Additional/Additional_Step_1"
output_folder <- "G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Additional/Additional_Step_2"

# Get list of text files in the input folder
text_files <- list.files(input_folder, pattern = "\\.txt$", full.names = TRUE)

# Loop through each text file
for (text_file in text_files) {
  # Create output file path
  output_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(text_file)), ".txt"))
  
  # Process the single text file
  process_text_file(text_file, output_file)
}

#### Step 3 : Temporal Trend Analysis : Appending the year and species information to the corpus ####

# Define the folder path
library(readxl)
library(stringr)
folder_path <- "G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Step_2"

# Get a list of all files in the folder
file_names <- list.files(folder_path, full.names = FALSE)

# Extract species name and year from file names
metadata <- lapply(file_names, function(file) {
  # Extract species name
  species <- str_extract(basename(file), "^[^_]+")
  # Extract year
  year <- str_extract(basename(file), "\\d{4}(?!.*\\d{4})")
  # Return species name, year, and file name
  return(c(file, species, year))
})

# Convert the list to a data frame
metadata_df <- as.data.frame(do.call(rbind, metadata))

#renaming the column names in the dataframe
colnames(metadata_df)<-c("File_name", "Species", "Year")

#Adding more information to the documents from the base data of megafauna, reading the excel directly
# Specify the range of columns to read
range_of_interest <- "B1:W36"  # Adjust the range to cover the columns you want to read

# Read the selected columns from the Excel sheet
selected_additional_traits <- read_excel("G:/My Drive/K/Postdoc at UW/Indias Megafauna/Datasets/Base_Sheet_For_Analysis.xlsx", 
                                         sheet = "Sheet1", 
                                         range = range_of_interest)

# Check the structure of the selected data
str(selected_additional_traits)

#Merging the two dataframes
merged_df <- merge(metadata_df, selected_additional_traits, by.x = "Species", by.y = "Binomial Name", all.x = TRUE)

# Check the structure of the merged dataframe
str(merged_df)

####
#### Step 4 : Text mining: Pre-processing the text ####

# Loading the libraries
install.packages("knitr")
install.packages("pdftools")
install.packages("tm")
install.packages ("stringr")
install.packages ("tidyr")
install.packages ("stringr")
install.packages("DataCombine")
install.packages("ggplot2")
install.packages("wordcloud2")
install.packages("pheatmap")
install.packages("dendextend")
install.packages("qgraph")
install.packages("dplyr")
install.packages("ldatuning")
install.packages("SnowballC")


library("pdftools")
library("tm")
library("stringr")
library("tidyr")
library("DataCombine")
library("ggplot2")
library('wordcloud2')
library("pheatmap")
library("dendextend")
library("qgraph")
library("dplyr")
library("ldatuning")
library("topicmodels")
library("viridis")
library("SnowballC")


# Get list of text files in the directory
text_files <- list.files(path="G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Step_2", pattern = "\\.txt$", full.names = TRUE)

# Initialize a list to store topics for each text
all_topics <- list()

# Load all text files in the directory in a list and store them as as a Tm text corpus
corpus <-  Corpus(URISource(text_files)) #Can be replaced by the function Pcorpus, once the code is ready to ensure reduced computational time)


# THIS SECTION NEEDS EDIT, WORDS ARE NOT BEING REPLACED WITH THE FOLLOWING CODE : Custom word replacement function (inspect the final list of frequent words, and use this to manually stem words missed out by automated stemming)
# Define the corpus transformation function

change_words <- function(corpus, replacements) {
  for (word_to_replace in names(replacements)) {
    replacement_word <- replacements[[word_to_replace]]
    corpus <- tm_map(corpus, content_transformer(function(x) gsub(paste0('\\<',word_to_replace,'\\>'), replacement_word, x)))
  }
  return(corpus)
}

# Define the words to replace and their replacements
replacements <- list(
  "environmental" = "environ",
  "experimental" = "experiment",
  "higher" = "high",
  "movement" = "move",
  "treatment" = "treat",
  "exposur" = "expos",
  "action" = "act",
  "dietaries" = "diet",
  "fed" = "feed", 
  "university" = ""
)

# Apply the replacements to the entire corpus
corpus <- change_words(corpus, replacements)

###UNTIL the above point

# Load corpus, without filtering or altering text
corpus_crude <- tm_map(corpus, FUN = function(x) x)

# Map and clean the corpus text (With modifications from Zondervan and Tolentino-Zondervan, 2023 & Lynch et al., 2023)
content.tdm <- TermDocumentMatrix(corpus_crude, 
                                  control = 
                                    list(removePunctuation = TRUE,
                                         stopwords = c(stopwords("en"), stopwords("SMART")),
                                         tolower = TRUE,
                                         stemming = TRUE,
                                         removeNumbers = TRUE,
                                         bounds = list(global = c(3, Inf),
                                                       content_transformer(tolower),
                                                       tm_map(corpus_crude, content_transformer(tolower))
                                         )))


# To explore the data, have a look at a part or the whole of the Term Document Matrix

inspect(content.tdm[1:10,])
View(as.matrix(content.tdm)) 

# Binarizing the Term Document Matrix
tdm.binary <- content.tdm # Create a copy to binarize
tdm.binary[content.tdm>=1] <- 1 # Replace terms count with a 1, to convert the data into presence-absence

#### Step 4a: Processing: Frequency count and Terms-Terms matrix ####
#(With modifications from Zondervan and Tolentino-Zondervan, 2023 & Lynch et al., 2023)

treshold_low = 100 # Lower frequency bound 
treshold_up = Inf # Upper frequency bound 
findFreqTerms(content.tdm, lowfreq = treshold_low, highfreq = treshold_up)

## Function to get frequent terms from tdm, this time as data frame
find_freq_terms_fun <- function(tdm){
  freq_terms <- findFreqTerms(content.tdm)[1:max(tdm$nrow)]
  terms_grouped <- tdm[freq_terms,] %>%
    as.matrix() %>%
    rowSums() %>%
    data.frame(Term=freq_terms, Frequency = .) %>%
    arrange(desc(Frequency)) %>%
    mutate(prop_term_to_total_terms=Frequency/nrow(.))
  return(data.frame(terms_grouped))
}

## Dataframe of the Frequent terms
df_terms_freq <- find_freq_terms_fun(tdm.binary)


#### Step 4b: Processing: Word lists to be removed from the corpus ####
# Four lists in all

#Listing the species binomial names and common taxonomic terminologies
speciesnames<- c("turtle", "amphibian", "reptile", "crocodil", "mammal", "dolphin","teleost", "bull", "shark", "fish", "catfish", "carp", "chitra", "indica", "Tor", "remadevii", "putitora", "Wallago", "attu", "mosal", "Bagarius", "yarrelli", "Channa", "marulius", "Glyphis", "gangeticus", "Carcharhinus", "leucas", "Hemibagrus", "maydelli", "microphthalmus", "Himantura", "uarnak", "Hypselobarbus", "mussullah", "Pangasius", "Pristis", "microdon", "Anoxypristis", "cuspidata", "Sperata", "aor", "seenghala", "Silonia", "Eleutheronema", "tetradactylum", "Gibelion", "catla", "labeo", "rohita","cyprinus", "carpio", "Urogymnus", "polylepis", "Pelochelys", "cantorii", "Chitra", "indica", "Amyda", "cartilaginea", "Nilssonia", "nigricans", "gangetica", "Manouria", "emys", "Gavialis", "gangeticus", "Crocodylus", "porosus", "palustris", "Orcaella", "brevirostris", "Platanista", "minor", "gangetica", "Bubalus", "arnee", "Rucervus", "eldii", "channa", "tilapia", "trout", "rainbow", "niloticus")


#Listing some additional common names based on preliminary results
freqwords<- c("observ", "similar", "report", "level", "refer", "follow", "anim", "addit", "respect", "group", "avail", "possibl", "method", "tion", "conclus", "term", "show", "work", "type", "sourc", "case", "interest", "close", "accord", "ing", "note", "pro", "con", "made", "characterist", "ment", "continu", "allow", "year", "abstract", "introduct", "materi", "publish", "acknowledg", "known", "regard", "research", "reveal", "Sampl", "scienc", "part", "pakistan", "khan", "speci", "abstract", "keyword", "discuss", "observ", "shown", "appear", "contrast", "analys", "repres", "design", "basi", "final", "confirm", "slight", "calcul", "obtain", "dri", "reason", "record", "applic", "red", "serv", "complet", "pre", "line", "establish", "rapid", "detect", "previous", "natur", "posit", "select", "reflect", "tive", "altern", "remov", "attribut", "abil", "exhibit", "hand", "highest", "lowest", "standard", "carri", "extract", "indian", "util", "conclud", "format", "free", "agreement", "andor", "appar", "clear", "typic", "play", "ture", "iti", "initi", "account", "aproxim", "substanti", "proport", "consider", "techniqu", "detail", "assum", "appli", "view", "propos", "absenc", "recommend", "subsequ", "approx")


#listing common words regarding the publication dates and author affiliations that are picked up with parsed abstract/acknowledgement sections
firstpage<- c("author", "affiliation", "correspond", "date", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "publish", "articl", "download", "onlin", "journal", "term", "access", "university", "institute", "thank", "receiv", "washington", "india", "australia", "pakistan", "bangladesh", "nepal", "public", "contribut", "wiley", "creative", "commons", "licence", "govern", "rule","wiley", "librari")

## Extra Cleaning of terms not meaningful for the analysis (with additions from the lists of Zondervan and Tolentino-Zondervan, 2023 & Lynch et al., 2023)

remove <- c('can','will','make','also','use','may','implement','author', 'figur', 'includ', 'howev', 'journal', 'studi',"tabl","fig","say","one","way","use","also","howev","tell","will","much","need","take","tend","even","like","particular","rather","said","get","well","make","ask","come","end","first","two","help","often","may","might","see","someth","thing","point","post","look","right","now","think","âve","âre","anoth","put","set","new","good","across","furthermore","given","greater","want","sure","kind","large","yes,","day","etc","quit","sinc","attempt","lack","seen","aware","little","ever","moreover","though","found","able","elsevier","whether","enough","far","early","away","achieve","draw","suggest","last","never","brief","bit","entire","brief","use", "two","base", "great","lot", "also","may","results","used", "using", "three", "however", "one", "among","based", "â€“", "affect", "along", "alter", "analysi", "approach", "assess", "associ", "averag", "caus", "chang", "combin", "common", "compar", "condit", "consequ", "consid", "consist", "correl", "coupl", "current", "data", "declin", "decreas", "demonstr", "depend", "determin", "differ", "direct", "due", "effect", "estim", "evalu", "evid", "examin", "expect", "explain", "factor", "find", "four", "water", "general", "gradient", "high", "higher", "identifi", "impact", "implic", "import", "includ", "increas", "indic", "induc", "influenc", "inform", "integr", "investig", "lead", "like", "loss", "low", "lower", "main", "major", "mani", "maximum", "mean", "measur", "method", "multipl", "near", "negat", "network", "number", "overal", "particular", "present", "probabl", "process", "product", "provid", "recent", "reduc", "reduct", "regress", "relat", "relationship", "remain", "requir", "respond", "result", "risk", "run", "scale", "seri", "sever", "show", "signific", "site", "small", "specif", "strong", "studi", "substani", "suggest", "support", "test", "total", "trend", "uncertainti", "understand", "upper", "usa", "vari", "variabl", "variat", "within", "chen", "acta", "ali", "york", "provided", "subsequent", "approx")

df_terms_freq <- df_terms_freq[!df_terms_freq$Term %in% speciesnames, ]
df_terms_freq <- df_terms_freq[!df_terms_freq$Term %in% freqwords, ]
df_terms_freq <- df_terms_freq[!df_terms_freq$Term %in% firstpage, ]
df_terms_freq <- df_terms_freq[!df_terms_freq$Term %in% remove, ]

# inspecting the results

head(df_terms_freq)
tail(df_terms_freq)
View(df_terms_freq)

#### Step 4c: Inspecting the corpus dataframe for the best threshold on dropping a set of terms ####

#Creating a copy of the term-document matrix to exclude 10 terms at a time
tdm_copy <- tdm.binary
tdm_copy_matrix <- as.matrix(tdm_copy)


# Add terms_df as the first column to result_df
terms <- rownames(tdm_copy_matrix)
tdm_copy_with_terms <- cbind(Term = terms, tdm_copy_matrix)

# Convert the matrix to a data frame
tdm_copy_updated <- as.data.frame(tdm_copy_with_terms)

#Removing  speciesnames and other stopword lists from the tdm
tdm_copy_updated <- tdm_copy_updated[!tdm_copy_updated$Term %in% speciesnames, ]
tdm_copy_updated <- tdm_copy_updated[!tdm_copy_updated$Term %in% freqwords, ]
tdm_copy_updated <- tdm_copy_updated[!tdm_copy_updated$Term %in% firstpage, ]
tdm_copy_updated <- tdm_copy_updated[!tdm_copy_updated$Term %in% remove, ]

## Calculating the percentage of terms across the papers to estimate the threshold
#total_documents <- ncol(tdm_copy_updated)
## Initial number of columns and rows (just to compare)
#ncol(tdm.binary)
#ncol(tdm_copy_updated)
#nrow (tdm.binary)
#nrow(tdm_copy_updated)

## Calculate the total number of times each term has a 1 in the cells
# Convert all elements in tdm_matrix from character to numeric
tdm_matrix <- as.matrix(tdm_copy_updated[, -1], rownames=TRUE)

#TRIAL ########## %%%%%%%% ######### %%%%%%
#tdm_matrix <- as.matrix(tdm_copy_updated)



#no of docs
total_documents <- ncol(tdm_matrix)
tdm_matrix <- apply(tdm_matrix, 2, as.numeric)

# Check the data type after conversion
print(class(tdm_matrix))
print(typeof(tdm_matrix))

# Calculate row sums excluding the "Term" column
term_counts <- rowSums(tdm_matrix)


# % of terms across documents
numerator <- term_counts
result <- (numerator / total_documents) * 100

plot(numerator, result)

# Looping the removal of terms to deduce a threshold 
# For all the documents in the corpus ( (1) Percentage method OR based on the (2) Number of terms- you can choose either of it, the number method would be a better choice if you want a finer resolution) 
# 


#### Step 4c(1) Percentage method ####


# Initialize variables to store results
terms_removed <- numeric()       # Vector to store the count of terms removed
docs_remaining <- numeric()      # Vector to store the number of documents remaining

# Get the total number of terms (rows) in the term-document matrix
total_terms <- nrow(tdm_matrix)

# Initialize the target percentage increment (1% in each iteration)
target_percent <- 1

# Loop through each term removal step
while (length(terms_removed) < total_terms) {
  # Calculate the number of terms to remove in this iteration based on target percentage
  terms_to_remove <- max(1, round(target_percent / 100 * total_terms))
  
  # Identify the indices of the least occurring terms based on term counts
  least_occurring_terms <- order(rowSums(as.matrix(tdm_matrix)))[1:terms_to_remove]
  
  # Remove NA values from least_occurring_terms (if any)
  least_occurring_terms <- na.omit(least_occurring_terms)
  
  # Remove the least occurring terms from tdm_matrix (only if valid indices are present)
  if (length(least_occurring_terms) > 0) {
    tdm_matrix <- tdm_matrix[-least_occurring_terms, , drop = FALSE]
  }
  
  # Calculate the number of documents remaining (those with at least one term)
  docs_with_terms <- colSums(tdm_matrix > 0) > 0
  num_docs_remaining <- sum(docs_with_terms)
  
  # Store the count of removed terms and number of documents remaining
  terms_removed <- c(terms_removed, length(least_occurring_terms))
  docs_remaining <- c(docs_remaining, num_docs_remaining)
  
  # Increment the target percentage for the next iteration
  target_percent <- target_percent + 1
}

# Calculate the cumulative percentage of terms removed relative to total_terms
cumulative_percent_removed <- cumsum(terms_removed) / total_terms * 100
total_value <- cumulative_percent_removed[length(cumulative_percent_removed)]
percentages <- (cumulative_percent_removed / total_value) * 100

# Create a data frame to store the results
result_df <- data.frame(
  Percent_Removed = percentages,
  Docs_Remaining = docs_remaining
)

# Print the resulting data frame
print(result_df)

# Plot the results using ggplot2
library(ggplot2)
ggplot(result_df, aes(x = Percent_Removed, y = Docs_Remaining)) +
  geom_line() +
  labs(x = "Percentage of Terms Removed", y = "Number of Documents Remaining") +
  ggtitle("Effect of Removing Least Occurring Terms on Document Coverage")




#### Step 4c(2) Based on term counts ####

##(make sure to run the tdm_matrix vector command again before running this excerpt, if you ran the Percentage method code snippet already)##
#Calculate initial term counts (sum of counts across all documents for each term)
#term_counts <- rowSums(as.matrix(tdm_copy))

# Initialize variables to store results
terms_removed_num <- numeric()  # Vector to store the count of terms removed
docs_remaining_num <- numeric()  # Vector to store the number of documents remaining

# Get the total number of terms (rows) in the term-document matrix
total_terms <- nrow(tdm_matrix)

# Loop through each term removal step
while (length(term_counts) > 5) {
  # Identify the 10 least occurring terms based on term_counts
  least_occurring_terms_num <- order(term_counts)[1:5]
  
  # Remove the least occurring terms from tdm_matrix
  tdm_matrix <- tdm_matrix[-least_occurring_terms_num, ]
  
  # Update term_counts after removal
  term_counts <- rowSums(as.matrix(tdm_matrix, rownames=TRUE))
  
  # Store the count of removed terms and number of documents remaining
  terms_removed_num <- c(terms_removed_num, length(least_occurring_terms_num))
  docs_with_terms_num <- colSums(as.matrix(tdm_matrix, rownames=TRUE)) > 0
  docs_remaining_num <- c(docs_remaining_num, sum(docs_with_terms_num))
}

# Create a data frame with results
result_df_num <- data.frame(Terms_Removed = cumsum(terms_removed_num), Docs_Remaining = docs_remaining_num)

# Print the resulting data frame
print(result_df_num)

# Plot the results
library(ggplot2)
ggplot(result_df_num, aes(x = Terms_Removed, y = Docs_Remaining)) +
  geom_line() +
  labs(x = "Number of Terms Removed", y = "Number of Documents Remaining") +
  ggtitle("Effect of Removing Least Occurring Terms on Document Coverage")

setwd("G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Figures")
getwd()

library(utils)
write.csv(result_df_num, "dataframe_num_docs.csv", quote = FALSE, sep = " ", eol = "\n", na= "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = "escape")




#### Step 5: Assigning topics  #### 

words_selection <- head(df_terms_freq$Term, 200) 

## Filter both the normal and the binary TDM on words you are interested in 
tdm.select <- content.tdm[rownames(content.tdm) %in% words_selection,]
tdm.binary.select <- tdm.binary[rownames(tdm.binary) %in% words_selection,]

## Remove NaN, remove rows and columns with only zero values, useful in case you searched for a Term and it was not found
m <- as.matrix(tdm.binary.select)
m[is.nan(m)] = 0
m <- m[rowSums(m[])>0,]
m <- m[,colSums(m[])>0]

# Convert TermDocumentMatrix to a numeric matrix
content_matrix <- as.matrix(tdm.binary.select)

# Calculate Term-Term matrix to see which words are co-occurring a lot
term_term_matrix <- content_matrix %*% t(content_matrix) 

# Calculate Pearson's correlation matrix based on the co-occurrence of terms in the binary matrix
cor_matrix <- cor(term_term_matrix, method = "pearson") 

# Sort terms by frequency in descending order and select the top 200
top_terms <- df_terms_freq$Term[order(-df_terms_freq$Frequency)][1:200]

# Subset the correlation matrix
top_term_indices <- match(top_terms, rownames(cor_matrix))  # Get indices of top terms
cor_matrix_subset <- cor_matrix[top_term_indices, top_term_indices]
#handling the NAs
#cor_matrix_subset[is.na(cor_matrix_subset)] <- 0

# Hierarchical clustering using "ward.D2" method
hc <- hclust(as.dist(1 - cor_matrix_subset), method = "ward.D2")

# Choosing the optimal number of clusters/deciding the cut point in the dendrogram 
#install.packages("NbClust")
library(NbClust)
library(cluster)
library(factoextra)

#### Step 5a: Finding the optimal number of clusters ####

### Elbow Method ###

# Calculate within-cluster sum of squares (WSS) for different numbers of clusters
wss <- sapply(2:10, function(k) {
  # Perform hierarchical clustering
  hc <- hclust(as.dist(1 - cor_matrix_subset), method = "ward.D2")
  # Cut the dendrogram into 'k' clusters
  cluster_groups <- cutree(hc, k = k)
  # Calculate WSS
  wss <- sum(apply(cor_matrix_subset, 1, function(row) sum((row - mean(row[cluster_groups == k]))^2)))
  return(wss)
})

### Silhouette Method ###

# Calculate silhouette widths for different numbers of clusters
sil_widths <- sapply(2:10, function(k) {
  # Perform hierarchical clustering
  hc <- hclust(as.dist(1 - cor_matrix_subset), method = "ward.D2")
  # Cut the dendrogram into 'k' clusters
  cluster_groups <- cutree(hc, k = k)
  # Calculate silhouette widths
  silhouette_width <- silhouette(cluster_groups, dist(1 - cor_matrix_subset))
  # Calculate mean silhouette width
  mean(silhouette_width[, "sil_width"])
})

### Gap Statistic Method ###

# Define a custom hierarchical clustering function similar to hclust
hclust_custom <- function(x, k) {
  dist_mat <- as.dist(x)
  hc <- hclust(dist_mat, method = "ward.D2")
  cluster_groups <- cutree(hc, k = k)
  return(list(data = x, cluster = cluster_groups))
}

# Calculate gap statistics for different numbers of clusters using hierarchical clustering
gap_stat <- clusGap(1 - cor_matrix_subset, FUNcluster = hclust_custom, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


##Plotting the results for the three methods ##
### |||| Run the proceeding code segment, outside of the chunks to export if using R markdown |||| ###


setwd("G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Figures")


# Plot the elbow plot
tiff("Elbow_method.tif", width=800, height=600, units="px", pointsize = 6, compression="lzw", res = 300)
plot(wss, type = "b", frame = TRUE, pch = 19, main = "Elbow Method",
     xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares (WSS)")
dev.off()

#Plot the Silhouette Plot
tiff("Silhouette_method.tif", width=800, height=600, units="px", pointsize=6, compression="lzw", res=300)
plot(2:10, sil_widths, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Mean Silhouette Width",
     main = "Silhouette Method")
dev.off()

#Plotting the gap statistics
tiff("Gap_Statistics_method.tif", width=800, height=600, units="px", pointsize=2, compression="lzw", res=300)
fviz_gap_stat(gap_stat) + theme(text = element_text(size = 8), plot.title = element_text(hjust=0.5))
dev.off()

#Plotting the dendrogram

#install.packages("ggplot2")
#install.packages("ggdendro")
#install.packages("dendextend")
library(dendextend)
library(ggplot2)
library(ggdendro)

# Perform hierarchical clustering
hc <- hclust(as.dist(1 - cor_matrix_subset), method = "ward.D2")

# Convert hclust object to dendrogram
dend <- as.dendrogram(hc)

# Customize the dendrogram
dend <- dend %>%
  set("branches_k_color", k = 5) %>%  # Color branches by clusters
  set("branches_lwd", 1) %>%          # Set branch line width
  set("labels_cex", 0.2)              # Set label size

# Plot the customized dendrogram
par(family = "sans")
tiff(filename="Dendrogram_5_clusters.tif", width=12, height=4, units = "in", res = 800, compression="lzw")
plot(dend, main = "Dendrogram of topics")
dev.off()


#### Step 5b: Find the text-mined topics based on the optimal number of clusters ####
### |||| Run the preceeding code segment, outside of the chunks to export- if using R markdown |||| ###

# Deciding the optimal number of clusters based on a consensus of Silhouette, Elbow and Gap statistics method
optimal_num_clusters <- 5

# Cut the dendrogram into distinct groups using the optimal number of clusters
cluster_groups <- cutree(hc, k = optimal_num_clusters)

# Combine cluster assignments with terms
clustered_terms <- data.frame(Term = rownames(cor_matrix_subset), Cluster = cluster_groups)

# Extract top words for each cluster based on Pearson's correlation
topics <- lapply(1:optimal_num_clusters, function(i) {
  cluster_terms <- clustered_terms$Term[clustered_terms$Cluster == i]
  top_words <- head(cluster_terms, 55)  # Adjust the number of top words as needed
  return(top_words)
})

#### Step 6: Assigning topics per document ####

# Convert term_doc_matrix to a matrix
term_doc_matrix <- as.matrix(tdm_copy_updated)

# Create a list of unique terms from all topics
unique_terms <- unique(unlist(topics))

# Initialize a dataframe with columns for each term in the topics list and each topic
wide_form_df <- data.frame(File_name = merged_df$File_name,
                           matrix(0, nrow = nrow(merged_df), ncol = length(unique_terms) + 2 * length(topics)),
                           stringsAsFactors = FALSE)

# Name the columns: one for each term and one for each topic (both unscaled and scaled)
colnames(wide_form_df)[2:(length(unique_terms) + 1)] <- unique_terms
colnames(wide_form_df)[(length(unique_terms) + 2):(length(unique_terms) + length(topics) + 1)] <- paste0("Unscaled_Topic_", seq_along(topics))
colnames(wide_form_df)[(length(unique_terms) + length(topics) + 2):ncol(wide_form_df)] <- paste0("Topic_", seq_along(topics))

# Iterate through each document
for (i in seq_len(nrow(merged_df))) {
  # Get the document name
  doc_name <- merged_df$File_name[i]
  
  # Get the document index
  doc_index <- which(colnames(term_doc_matrix) == doc_name)
  
  # If the document exists in the term_doc_matrix
  if (length(doc_index) > 0) {
    # Get the terms in the document
    document_terms <- rownames(term_doc_matrix)[term_doc_matrix[, doc_index] > 0]
    
    # Filter document_terms to only include those in unique_terms
    filtered_terms <- document_terms[document_terms %in% unique_terms]
    
    # Count occurrences of each filtered term in the document and ensure term_counts is numeric
    term_counts <- as.numeric(term_doc_matrix[filtered_terms, doc_index])
    names(term_counts) <- filtered_terms
    
    # Set the term columns to the counts of terms present in the document
    for (term in filtered_terms) {
      wide_form_df[i, term] <- term_counts[term]
    }
    
    # Initialize a vector to hold the count of terms for each topic
    topic_counts <- numeric(length(topics))
    
    # Iterate through each topic
    for (topic_index in seq_along(topics)) {
      # Get the terms of the current topic
      topic_terms <- topics[[topic_index]]
      
      # Count the number of topic terms present in the document
      topic_counts[topic_index] <- sum(term_counts[names(term_counts) %in% topic_terms])
      
      # Calculate the scaled topic count and round to 3 decimal places
      scaled_topic_count <- round(topic_counts[topic_index] / length(topic_terms), 3)
      
      # Assign the unscaled and scaled topic counts to the respective columns
      wide_form_df[i, length(unique_terms) + topic_index + 1] <- topic_counts[topic_index]
      wide_form_df[i, length(unique_terms) + length(topics) + topic_index + 1] <- scaled_topic_count
    }
  }
}

# Merge with other columns from merged_df
wide_form_df <- merge(wide_form_df, merged_df, by = "File_name")

# Print wide_form_df to check if it's in the desired format
print(wide_form_df)
View(wide_form_df)

# Export wide_form_df as a CSV file
setwd("G:/My Drive/K/Postdoc at UW/Papers from the project/Policy Analysis/Fairing/Figures")
getwd()
write.csv(wide_form_df, "terms_topics_wide_dataframe_5topics.csv", row.names = FALSE)









