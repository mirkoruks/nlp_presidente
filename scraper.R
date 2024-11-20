rm(list = ls())
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

library(RSelenium)
library(wdman)
library(tidyverse)
library(netstat)
library(tidyverse)


# scrape the presidents speeches 
selenium()

selenium_object <- selenium(retcommand = TRUE, check = FALSE)
selenium_object


# close the server
remote_driver$server$stop()


# use firefox 
remote_driver <- rsDriver(browser = "firefox",
                          chromever = NULL,
                          verbose = TRUE,
                          #port = netstat::free_port())
                          port = 4545L)

remDr <- remote_driver$client
#remDr$open()


remDr$navigate("http://www.asamblea.go.cr/sd/mensajes%20presidenciales/forms/allitems.aspx")

meta_data_df <- tibble(label = NA,
                       period = NA,
                       Nspeech = NA)

for (p in 1:2) {
    
    total_entries_raw <- remDr$findElement(using = 'xpath',
                                           paste0('/html/body/form/div[4]/div[3]/div/div/div[4]/div/div/div/div/div[2]/div/div[1]/div[2]/div/div/table[2]/tbody/tr[3]/td/table/tbody/tr/td[',p,']'))
    total_entries <- total_entries_raw$getElementText()[[1]] %>% 
        str_extract_all("[0-9]+") %>% 
        unlist() %>% 
        as.numeric()
    total_entries
    
    if (total_entries[1] != 1) {
        total_entries <- (total_entries-(total_entries[1]-1))
    }
    
    for (j in total_entries[1]:total_entries[2]) {
        cat("\n\n\n\nScrape meta data of president No", j)
        Sys.sleep(10)
        meta_data_raw <- remDr$findElement(using = 'xpath',
                                           paste0('/html/body/form/div[4]/div[3]/div/div/div[4]/div/div/div/div/div[2]/div/div[1]/div[2]/div/div/table[1]/tbody/tr/td/table/tbody[',(j*2),']/tr/td'))
        Sys.sleep(10)
        meta_data <- 
            meta_data_raw$getElementText()[[1]] %>% 
            str_remove_all(pattern = c('\\)')) %>%
            str_split(c("\\:|\\("), simplify = TRUE) %>% 
            map_dfc(str_trim) 
        names(meta_data) <- c("label","period","Nspeech")
        
        speech_count <- meta_data %>% 
            select(Nspeech) %>% 
            as.numeric()
        
        meta_data_df <- bind_rows(meta_data_df, meta_data)
        
        
        
        
        for (i in 1:speech_count) {
            Sys.sleep(10)
            if (p == 1) {
                remDr$navigate("http://www.asamblea.go.cr/sd/mensajes%20presidenciales/forms/allitems.aspx")
            } else if (p == 2) {
                url_page2 <- remDr$getCurrentUrl()[[1]]
                remDr$navigate(url_page2) 
            }
            Sys.sleep(10)
            if (i == 1) {
                Sys.sleep(10)
                cat("\n\nExpand speeches of president No",j)
                
                goto <- remDr$findElement(using = 'xpath',
                                          paste0('/html/body/form/div[4]/div[3]/div/div/div[4]/div/div/div/div/div[2]/div/div[1]/div[2]/div/div/table[1]/tbody/tr/td/table/tbody[',(j*2),']/tr/td/a'))
                goto$clickElement()
                Sys.sleep(10)
            }
            Sys.sleep(5)
            cat("\n\nScrape document name of speech No",i,"of president No",j,"\n\n\n\n")
            document_name_raw <- remDr$findElement(using = 'xpath',
                                                   paste0('/html/body/form/div[4]/div[3]/div/div/div[4]/div/div/div/div/div[2]/div/div[1]/div[2]/div/div/table[1]/tbody/tr/td/table/tbody[',((2*j)+1),']/tr[',i,']/td[2]/div[1]/a'))
            Sys.sleep(5)
            
            document_name <- document_name_raw$getElementText()[[1]]
            
            document_name_raw$clickElement()
            
            pdf_link <- document_name_raw$getCurrentUrl()[[1]]
            document_name <- document_name %>% 
                str_replace_all(pattern = " ",
                                replacement = "_")
            const_period <- meta_data %>% select(period) %>% as.character()
            if (!(dir.exists(paste0('C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/',const_period)))) {
                dir.create(paste0('C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/',const_period))
            }
            download.file(pdf_link, destfile = paste0('C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/',const_period,'/',document_name,'.pdf'), mode = "wb")
            remDr$goBack()
            Sys.sleep(5)
            
        }
        if (j == total_entries[2] & p == 1) {
            Sys.sleep(20)
            next_page <- remDr$findElement(using = 'xpath',
                                           '/html/body/form/div[4]/div[3]/div/div/div[4]/div/div/div/div/div[2]/div/div[1]/div[2]/div/div/table[2]/tbody/tr[3]/td/table/tbody/tr/td[2]/a/img')
            next_page$clickElement()
            Sys.sleep(20)
        }
    }
}

file.remove("C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/El_control_parlamentario_en_Costa_Rica,_una_perspectiva_de_derecho_comparado.pdf")

file.rename(from = "C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/1824-1833/MENSAJE_DE_LA_JUNTA_GUBERNATIVA_AL_CONGRESO_CONSTITUYENTE_8_SET_1824.pdf",
            to = "C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/1824-1833/Mensaje_de_la_junta_gubernativa_al_congreso_constituyente_1824.pdf")

file.rename(from = "C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/1920-1924/Discurso_Entrega_Poder_1925_J._Acosta_García.pdf",
            to = "C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/1920-1924/Discurso_Entrega_Poder_1924_J._Acosta_García.pdf")

file.rename(from = "C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/1920-1924/Discurso_Toma_Posesión_J._Acosta_García.pdf",
            to = "C:/Users/Mirko/Documents/Data Science/presidentes_nlp/Data/1920-1924/Discurso_Toma_Posesión_1920_J._Acosta_García.pdf")