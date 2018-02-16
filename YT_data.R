##-----------------------------------------------------------------------------##
##                          MINING YOUTUBE COMMENTS                            ##
##-----------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Video: https://www.youtube.com/watch?v=0MhjKT20PF0 (ID: 0MhjKT20PF0)


#-------#
# Setup #
#-------#

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(data.table, quanteda, reshape2, tuber, tidytext, tidyverse, wordcloud)


#---------------------#
# Import YouTube data #
#---------------------#

## YouTube Data Tools: https://tools.digitalmethods.net/netvizz/youtube/mod_video_info.php

# Import data
info <- read.table("videoinfo_0MhjKT20PF0_2018_02_16-10_58_54_basicinfo.tab", header = FALSE, quote = "", comment = "", sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
authors <- read.table("videoinfo_0MhjKT20PF0_2018_02_16-10_58_54_authors.tab", header = FALSE, quote = "", comment = "", sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
comments <- read.table("videoinfo_0MhjKT20PF0_2018_02_16-10_58_54_comments.tab", header = TRUE, quote = "", comment = "", sep = "\t", fill = TRUE, stringsAsFactors = FALSE)


#---------------#
# Text cleaning #
#---------------#

# Function to remove HTML tags
## Source: https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
clean_html <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# Remove HTML tags
comments$test <- clean_html(comments$text)

# Remove URLs
comments$test <- gsub("http[[:alnum:][:punct:]]*", "", comments$test) 

# Remove emojis
comments$test <- iconv(comments$test, to = "UTF-8-MAC", sub = "byte")

# Remove HTML encodings
comments$test <- gsub("&quot;|&#39;|&lt;", "", comments$test)

# Convert to lowercase
comments$test <- tolower(comments$test)

# Remove punctuation (except for apostrophes) and numbers
comments$test <- gsub("[^[:alpha:][:blank:]']", "", comments$test)
## NOTE: Remove apostrophes (previous removal of HTML encodings)?

# Remove br, cr and 39
## DOES NOT WORK YET
comments$test <- gsub("br|39", "", comments$test)

# Unnest and tokenize text
comments_tidy <- comments %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


#-------#
# Theme #
#-------#

# Set theme for visualizations
viz_theme <- theme(
  strip.background = element_rect(colour = "transparent", fill = "grey90"),
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  strip.text = element_text(size = rel(1), face = "bold"),
  plot.caption = element_text(colour = "grey50"),
  text = element_text(family = "Avenir"))


#------------------#
# Word frequencies #
#------------------#

# Find most common words
comments_wordfreq <- comments_tidy %>%
  count(word, sort = TRUE)

# Plot words
comments_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  theme(text = element_text(size = 30)) + 
  xlab("") + ylab("") + ggtitle("Most common words in Youtube comments", subtitle = " ") +
  ylim(0, 600) + coord_flip() + viz_theme 

ggsave("plot_words.png", width = 12, height = 8, units = "in", dpi = 100)


#--------------------#
# Sentiment analysis #
#--------------------#

# Calculate and plot total sentiment scores (nrc)
comments_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  ggplot(aes(sentiment, n)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(text = element_text(size = 30), axis.text.x = element_text(angle = 65, vjust = 0.5)) +
  xlab("") + ylab("") + ggtitle("Total sentiment scores in Youtube comments", subtitle = " ") +
  ylim(0, 5000) + theme(legend.position = "none") + viz_theme 

ggsave("plot_sentiments.png", width = 12, height = 8, units = "in", dpi = 100)


#-------------------------#
# Positive/negative words #
#-------------------------#

# Calculate positive and negative sentiments (bing)  
bing_counts <- comments_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Calculate top word contributors
bing_counts_plot <- bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 

# Plot most common positive and negative words
ggplot(bing_counts_plot, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab("") + ylab("") + 
  theme(text = element_text(size = 30)) + 
  ggtitle("Most common +/- words in youtube comments", subtitle = " ") +
  coord_flip() + viz_theme

ggsave("plot_pos_neg_words.png", width = 12, height = 8, units = "in", dpi = 100)


#-----------#
# Wordcloud #
#-----------#

#words_rm = data.table("trump")
#colnames(words_rm) <- "word"

# Plot comparison cloud
## Remove TRUMP (does not fit anymore)
png("wordcloud.png", width = 3.5, height = 3.5, units = 'in', res = 300)
comments_tidy %>%
  #anti_join(words_rm) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 60)
dev.off()