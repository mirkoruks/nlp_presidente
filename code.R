rm(list = ls())
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

library(tidytext)
library(pdftools)
library(tidyverse)
library(tidyverse)
library(stopwords)
library(tabulizer)
library(tesseract)
require(quanteda)
require(quanteda.textmodels)
require(quanteda.textstats)
require(quanteda.textplots)
require(readtext)
require(devtools)
require(quanteda.corpora)
require(newsmap)
require(seededlda)
quanteda_options("threads" = 8)

use_stemming <- TRUE
files <- list.files("Data",
                    recursive = TRUE)
files <- files %>% 
    str_remove_all("(.*)(_Anexo.pdf$)")
files <- files[files != ""]

period <- files %>% 
    str_extract_all("(^.*)/") %>% 
    str_remove_all("/") %>% 
    unlist()

years <- files %>% 
    str_extract_all("_\\d\\d\\d\\d") %>% 
    str_remove_all("_") %>% 
    unlist() 

names <- files %>% 
    str_replace_all(pattern = "(^.*_\\d*_)(.*)(.pdf$)",
                    replacement = "\\2") %>% 
    str_replace_all(pattern = "_",
                    replacement = " ")

type <- files %>% 
    str_replace_all(pattern = "(^.*/)(.*)(_\\d.*)",
                    replacement = "\\2") %>% 
    str_replace_all(pattern = "_",
                    replacement = " ")

meta_info <- tibble(period = period,
                    year = years,
                    president = names,
                    doc_type = type) %>% 
    mutate(president = case_when(type %in% c("Mensaje de la junta gubernativa al congreso constituyente",
                                             "Inauguración del Congreso") ~ NA,
                                 TRUE ~ president))
# make sure that names are coherent within period
meta_info <- meta_info %>% 
    mutate(special = case_when(is.na(president) ~ 1)) %>% 
    group_by(period, special) %>% 
    mutate(president = dplyr::first(president)) %>% 
    ungroup() %>% 
    select(-special)

text_files <- paste0("Data/",files)


# import text files using tabulizer
text_vec_orig <- map_chr(text_files, extract_text)

# drop some header and footer texts from Alvarado Quesada
# 2020: estado político de la república y los asuntos de la administración mensaje presidencial
# 2021: estado político de la república y los asuntos de la administración mensaje presidencial

# 248, 249
text_vec_orig[248] <- str_remove_all(string = text_vec_orig[248],
                                     pattern = "ESTADO POLÍTICO DE LA REPÚBLICA\r\nY LOS ASUNTOS DE LA ADMINISTRACIÓN\r\nMENSAJE PRESIDENCIAL, 2019 - 2020")
text_vec_orig[249] <- str_remove_all(string = text_vec_orig[249],
                                     pattern = "ESTADO POLÍTICO DE LA REPÚBLICA\r\nY LOS ASUNTOS DE LA ADMINISTRACIÓN\r\nMENSAJE PRESIDENCIAL, 2020 - 2021")
text_vec <- text_vec_orig %>% 
    parse_character(locale = locale(encoding = "UTF-8")) %>% # parse to utf8
    str_remove_all("\\d|\\r") %>% # remove numbers
    str_replace_all(pattern = "\n|\t", replacement = "") %>% 
    str_replace_all(pattern = "([a-zA-Z])(\\-)([a-zA-Z])", replacement = "\\2") %>% 
    #str_replace_all(pattern = "( )([[:punct:]])", replacement = "\\2") %>% 
    #str_replace_all(pattern = "[^(a-z|A-z|\\s)]", replacement = "") %>% # problem: non ascii worte werden rausgefiltert
    str_replace_all(pattern = "[^[[:alnum:]]|\\s]", replacement = "") %>% 
    #gsub(., pattern = "\\-\\n", replacement = "") %>%
    #gsub(., pattern = "\n|\t", replacement = " ") %>% 
    #gsub(., pattern = "( )([[:punct:]])", replacement = "\\2") %>% # put punctuation in order, i.e. do not "bla . bla" but do "bla. bla"
    str_squish() %>% # remove whitespace
    tolower() # all lower case



text_df <- tibble(doc_id = 1:length(text_vec),
                  text = text_vec) %>% 
    bind_cols(meta_info)


# create corpus object
text_corpus <- corpus(text_df, text_field = "text")
text_corpus

# create token object
text_tokens <- tokens(text_corpus, remove_punct = TRUE, remove_symbols = TRUE, padding = TRUE)
text_tokens

# filter spanish stopwords (https://github.com/Alir3z4/stop-words/blob/master/spanish.txt)
spanish_stop <- read_file("spanish_stopwords.txt") %>% 
    str_split("\\n") %>% 
    unlist() %>% 
    str_subset(".+")
spanish_stop

text_tokens_nostop <- text_tokens %>% 
    tokens_remove(spanish_stop, padding = TRUE) %>% 
    tokens_select(min_nchar = 3, padding = TRUE)
text_tokens_nostop

# bigrams that often occur
tstat_col_cap <- textstat_collocations(text_tokens_nostop, min_count = 10, tolower = FALSE)
tail(tstat_col_cap, 50)

tstat_col_cap
table(str_count(string = tstat_col_cap$collocation,
                pattern = "\\S+")) # only two word combinations


text_tokens_final <- text_tokens_nostop %>% 
    tokens_compound(pattern = tstat_col_cap[tstat_col_cap$z > 8,],
                    case_insensitive = FALSE,
                    join = FALSE) %>% 
    tokens_remove(pattern = "")



# create document-feature matrix
text_dfm <- dfm(text_tokens_final)
text_dfm

# group by name of president
test <- text_dfm %>% 
    dfm_group(groups = interaction(period,president)) %>% 
    dfm_weight(scheme = "prop") %>% 
    textstat_frequency(n = 10, groups = interaction(period, president))


test_df <- test %>% 
    mutate(group = str_replace(group, 
                               pattern = "(.*\\d)(\\.)(.*)",
                               replacement = "\\3 (\\1)"),
           group = str_replace(group,
                               pattern = "\\s\\.",
                               replacement = "\\."),
           feature = str_replace(feature, 
                                 pattern = "_",
                                 replacement = " "),
           maxyear = str_replace(group,
                                 pattern = "(.*)(\\d\\d\\d\\d)(\\))$",
                                 replacement = "\\2"),
           maxyear = as.numeric(maxyear),
           group_factor = fct_reorder(group, maxyear)) 
str(test_df)
pal_test <- colorRampPalette(wes_palette(n = 4, name="GrandBudapest1"))(100)

ggplot(data = test_df,
       mapping = aes(x = reorder_within(feature, frequency, group_factor),
                     y = frequency,
                     fill = group_factor)) + 
    scale_fill_manual(values= pal_test) + 
    geom_col(show.legend = FALSE) +
    facet_wrap(.~group_factor, 
               scales = "free_y") + 
    scale_y_continuous(labels = scales::label_percent()) +
    coord_flip() +
    theme_bw() +
    scale_x_reordered() + 
    labs(
        x = "",
        y = "Relative Word Frequency",
        title = "Top 10 relative word frequencies by government",
    )




# lexical diversity
test2 <- text_dfm %>% 
    dfm_group(groups = interaction(period,president)) %>% 
    textstat_lexdiv()
View(test2)

test2_df <- test2 %>% 
    mutate(document = str_replace(document, 
                                  pattern = "(.*\\d)(\\.)(.*)",
                                  replacement = "\\3 (\\1)"),
           document = str_replace(document,
                                  pattern = "\\s\\.",
                                  replacement = "\\."),
           maxyear = str_replace(document,
                                 pattern = "(.*)(\\d\\d\\d\\d)(\\))$",
                                 replacement = "\\2"),
           maxyear = as.numeric(maxyear),
           document_factor = fct_reorder(document, maxyear)) 


ggplot(data = test2_df,
       mapping = aes(x = document_factor,
                     y = TTR,
                     group = 1)) + 
    coord_flip() +
    geom_point() +
    geom_line() +
    theme_bw()


