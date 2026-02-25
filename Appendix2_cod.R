# --- Load Required Packages ---
library(rvest)
library(magrittr)
library(httr)
library(reticulate)
library(xml2)
library(dplyr)
library(knitr)
library(ggplot2)
library(officer)
library(flextable)

# --- Python/OpenAI Configuration ---
use_python("C:/Users/Emanuel Cordeiro/OneDrive/Documentos Antigos/.virtualenvs/r-reticulate/Scripts/python.exe", required = TRUE)
openai <- import("openai")
# ---
Sys.setenv(OPENAI_API_KEY = "YOUR_API_KEY_HERE")  

# --- Web Scraping Functions ---
extract_image_descriptions <- function(url) {
  webpage <- read_html(url)
  webpage %>% html_elements("script, style") %>% xml_remove()
  images <- webpage %>% html_elements("img") %>% html_attrs()
  descriptions <- sapply(images, function(x) x["alt"])
  return(descriptions)
}

extract_text <- function(url) {
  webpage <- read_html(url)
  webpage %>% html_elements("script, style") %>% xml_remove()
  text <- webpage %>% html_element("body") %>% html_text(trim = TRUE)
  return(text)
}

# --- Analysis and Processing Functions ---
analyze_text_or_image_description <- function(content, prompt) {
  client <- openai$OpenAI(api_key = Sys.getenv("OPENAI_API_KEY"))
  
  full_prompt <- paste(prompt, content)
  
  response <- client$chat$completions$create(
    model = "gpt-4",
    messages = list(
      reticulate::dict(role = "system", content = "You are a scientific reviewer analyzing website content related to cosmetic genital surgery."),
      reticulate::dict(role = "user", content = full_prompt)
    ),
    max_tokens = as.integer(3000),
    temperature = 0
  )
  
  return(response$choices[[1]]$message$content)
}

# ---- Content Filtering and Truncation Function-----
filter_and_truncate_content <- function(content) {
  max_length <- 12000
  truncated <- substr(content, 1, min(nchar(content), max_length))
  unwanted <- c("logo", "icon", "ads", "advertisement", "Email Us Call Us")
  for (u in unwanted) {
    truncated <- gsub(u, "", truncated, ignore.case = TRUE)
  }
  return(truncated)
}

create_narrative <- function(type, prompt, analysis) {
  paste("Section:", type, "\n\nPrompt: ", prompt, "\n\nResponse: ", analysis)
}

# --- PROMPTS ---
prompts <- c(
  "Analyze the content of a website that offers labiaplasty and identify language or imagery that promotes unrealistic or harmful ideals of female genital appearance, perpetuates standards of appearance, or leads to body image issues and anxiety.",
  "Examine the website content for language or imagery in before-and-after photos that show how the labiaplasty can make someone look better or suggest enhancing sexual responsiveness, regaining a youthful appearance, addressing sexual issues, or improving sexual pleasure as motivations for the procedure.",
  "Identify examples of language or imagery that inaccurately label natural variations in vulvar or vaginal appearance as abnormal, endorse aesthetic procedures for altering the appearance, or present genital cosmetic procedures as a means to increase confidence or empowerment."
)

# --- Target Website URL---
url <- "https://meridiansurgery.com/labiaplasty-austin-tx/"

# --- Execute Web Scraping ---
text_content <- extract_text(url)
image_descriptions <- extract_image_descriptions(url)
image_text <- paste(image_descriptions, collapse = " ")

# --- Textual Analysis Pipeline ---
text_results_df <- data.frame(prompt = character(), result = character(), stringsAsFactors = FALSE)
for (prompt in prompts) {
  filtered_text <- filter_and_truncate_content(text_content)
  if (filtered_text != "") {
    text_result <- analyze_text_or_image_description(filtered_text, prompt)
    text_results_df <- bind_rows(text_results_df, data.frame(prompt = prompt, result = text_result, stringsAsFactors = FALSE))
  }
}

# --- Merge Results and Export to Word Document ---
image_results_df <- data.frame(prompt = character(), result = character(), stringsAsFactors = FALSE)
if (image_text != "") {
  filtered_images <- filter_and_truncate_content(image_text)
  for (prompt in prompts) {
    image_result <- analyze_text_or_image_description(filtered_images, prompt)
    image_results_df <- bind_rows(image_results_df, data.frame(prompt = prompt, result = image_result, stringsAsFactors = FALSE))
  }
}

# --- Merge Results and Save as Word Document ---
text_results_df$result <- mapply(create_narrative, "text", text_results_df$prompt, text_results_df$result, SIMPLIFY = FALSE)
image_results_df$result <- mapply(create_narrative, "image", image_results_df$prompt, image_results_df$result, SIMPLIFY = FALSE)
combined_results_df <- bind_rows(text_results_df, image_results_df)

output_file <- "C:/Users/Emanuel Cordeiro/OneDrive/Artigos/Artigo_Artur/Novas_analises/GPT4_Analysis_Save_Word_Site038.docx"

doc <- read_docx()
doc <- body_add_par(doc, "Website Analysis Report", style = "heading 1")
doc <- body_add_par(doc, paste("Date:", Sys.Date()), style = "Normal")
doc <- body_add_par(doc, paste("URL:", url), style = "Normal")

for (i in seq_len(nrow(combined_results_df))) {
  doc <- body_add_par(doc, combined_results_df$prompt[i], style = "heading 2")
  doc <- body_add_par(doc, combined_results_df$result[i], style = "Normal")
}

print(doc, target = output_file)
cat("✅ Analysis completed and saved to:", output_file)
