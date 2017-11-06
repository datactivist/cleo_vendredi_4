load("~/data/frequentation/journalsAgrege20170101-20170420.Rdata")
load("~/data/frequentation/booksAgrege20170101-20170420.Rdata")
load("~/data/frequentation/hypoAgrege20170101-20170420.Rdata")
library(ineq)
library(locur)
library(stringr)
library(tidyverse)


urls_booksAgrege <- urls_booksAgrege %>% 
  filter(!is.na(url)) %>% 
  filter(!label %in% "index")
urls_hypoAgrege <- urls_hypoAgrege %>% 
  filter(!is.na(url)) %>% 
  filter(!label %in% "index")
urls_journalsAgrege <- urls_journalsAgrege %>% 
  filter(!is.na(url)) %>% 
  filter(!label %in% "index")

df <- bind_rows(
  urls_journalsAgrege,
  urls_booksAgrege,
  urls_hypoAgrege, 
  .id = "plateforme"
)


dist <- Lc(df$nb_visits)
dist_journals <- Lc(urls_journalsAgrege$nb_visits)
dist_books <- Lc(urls_booksAgrege$nb_visits)
dist_hypo <- Lc(urls_hypoAgrege$nb_visits)

dist <- bind_rows(
  data_frame(
    plateforme = "journals",
    p = dist_journals$p,
    L = dist_journals$L
  ),
  data_frame(
    plateforme = "books",
    p = dist_books$p,
    L = dist_books$L
  ),
  data_frame(
    plateforme = "hypo",
    p = dist_hypo$p,
    L = dist_hypo$L
  )
)


dist %>% 
  ggplot(aes(x= 1 - p, y = L)) +
  geom_line(aes(color = plateforme)) +
  scale_x_continuous(name="Part cumulée des documents", limits=c(0,1)) + 
  scale_y_continuous(name="Part cumulée des visites", limits=c(0,1)) +
  geom_abline(slope = -1, intercept = 1)

###

source("../scripts/fonctions.R")
library(httr)
library(purrr)
library(lubridate)
library(tidyverse)


source("auth.R", local = TRUE)

urls_journalsAgrege <- getPageUrls(idSite = 3, date = paste0(ymd("20170421"), ",", ymd("20170601")), period = "range")

urls_booksAgrege <- getPageUrls(idSite = 5, date = paste0(ymd("20170421"), ",", ymd("20170601")), period = "range")


## parser les urls ?

library(rex)

rex_mode()

valid_chars <- rex(except_some_of(".", "/", " ", "-"))


regex_journal <- rex(
  group("http", maybe("s"), "://"),
  capture(name = "revue",
          zero_or_more(valid_chars, zero_or_more("-"))
  ),
  maybe("."),
  "revues.org",
  maybe("/",
        capture(
          name = "article",
          one_or_more(numbers))
  )
)

urls_journalsAgrege <- bind_cols(urls_journalsAgrege, re_matches(urls_journalsAgrege$url, regex_journal))

urls_journalsAgrege %>% 
  group_by(revue, article) %>% 
  summarise_at(vars(contains("visits")), funs(sum(.))) %>% 
  filter(!revue %in% "www") %>% 
  filter(!is.na(revue)) %>% 
  arrange(desc(nb_visits)) %>% 
  mutate(url = paste0("http://", revue, ".revues.org/", article))

save(urls_journalsAgrege, file = "../data/frequentation/journalsAgrege20170421-20170601.Rdata")
write_csv(urls_journalsAgrege, path = "../data/frequentation/journalsAgrege20170421-20170601.csv")

## books

regex_books <- rex(
  group("http", maybe("s"), "://"),
  "books.openedition.org",
  except_any_of("/"),
  maybe(
    "/",
    capture(name = "collection",
            zero_or_more(valid_chars, zero_or_more("-"))
    ),
    maybe("/",    
          capture(name = "chapitre",
                  numbers
          )
    )
  )
)

urls_booksAgrege <- bind_cols(urls_booksAgrege, re_matches(urls_booksAgrege$url, regex_books))

urls_booksAgrege <- urls_booksAgrege %>% 
  filter(!collection %in% "") %>% 
  filter(!str_detect(collection, "\\?")) %>% 
  filter(!chapitre %in% "") %>% 
  group_by(collection, chapitre) %>% 
  summarise_at(vars(contains("visits")), funs(sum(.))) %>% 
  mutate(url = paste0("http://books.openedition.org/", collection, "/", chapitre))


save(urls_booksAgrege, file = "../data/frequentation/booksAgrege20170421-20170601.Rdata")
write_csv(urls_booksAgrege, path = "../data/frequentation/booksAgrege20170421-20170601.csv")

## hypothèses

urls_hypoAgrege <- getPageUrls(idSite = 4, date = paste0(ymd("20170101"), ",", ymd("20170420")), period = "range")

regex_hypo <- rex(
  group("http", maybe("s"), "://"),
  capture(name = "carnet",
          zero_or_more(valid_chars, zero_or_more("-"))
  ),
  maybe("."),
  "hypotheses.org",
  maybe("/",
        capture(
          name = "billet",
          one_or_more(numbers))
  )
)

urls_hypoAgrege <- bind_cols(urls_hypoAgrege, re_matches(urls_hypoAgrege$url, regex_hypo))
save(urls_hypoAgrege, file = "../data/frequentation/hypoAgrege20170101-20170420.Rdata")
write_csv(urls_hypoAgrege, path = "../data/frequentation/hypoAgrege20170101-20170420.csv")

## Calenda

urls_calendaAgrege <- getPageUrls(idSite = 6, date = paste0(ymd("20170101"), ",", ymd("20170420")), period = "range")

regex_calenda <- rex(
  group("http", maybe("s"), "://"),
  "calenda.org",
  maybe("/",
        capture(
          name = "evenement",
          one_or_more(numbers))
  )
)

urls_calendaAgrege <- bind_cols(urls_calendaAgrege, re_matches(urls_calendaAgrege$url, regex_calenda))
save(urls_calendaAgrege, file = "../data/frequentation/calendaAgrege20170101-20170420.Rdata")
write_csv(urls_calendaAgrege, path = "../data/frequentation/calendaAgrege20170101-20170420.csv")
