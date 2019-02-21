
##GO TO LINE 300

library(tidyverse)
library(tidytext)
library(wordcloud)
library(countrycode)

##Tamas's data
db <- read.table(file="un_raw.csv")
db <- as_data_frame(db)
db

db <- db %>% arrange(country, year)


sentences <- db %>% 
  unnest_tokens(sentence, text, token = "sentences")

sentences_words <- sentences %>% mutate(line = 1, linen = cumsum(line))%>% select(-line) %>% 
  unnest_tokens(word, sentence) %>%
  mutate(id = paste(as.character(year), as.character(country), as.character(linen)) )


#write_csv(sentences_words, path = "all_words_with_sentencenum.csv")

data("stop_words")
##to keep "united states" and "great britain"
stop_words2 <- stop_words %>% filter(!(word %in% c("states","great")))

cleaned <- sentences_words %>% anti_join(stop_words2)

##to recover states' names
cleaned <- cleaned %>%
  mutate(word2 = str_extract(word, "[a-z]+")) %>%  filter( !is.na(word2) ) %>% 
  select(-word) %>%
  mutate(nextword = lead(word2)) 

cleaned <- cleaned %>% mutate(check = if_else(word2 == "united" & nextword == "states", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="saudi" & nextword == "arabia", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="viet" & nextword == "nam", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="soviet" & nextword == "union", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="south" & nextword == "korea", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="north" & nextword == "korea", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="south" & nextword == "korean", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="north" & nextword == "korean", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="united" & nextword == "kingdom", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="great" & nextword == "britain", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="european" & nextword == "union", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="east" & nextword == "germany", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% mutate(check = if_else(word2 =="west" & nextword == "germany", T, F)) %>% 
  mutate(word2 = if_else(check, paste(word2, nextword, sep = ""), word2)) %>% 
  mutate(checklag = lag(check)) %>% filter(checklag != T) %>% 
  select(-check, -checklag)

cleaned <- cleaned %>% rename(word=word2) %>% select(-nextword) %>% anti_join(stop_words)

#write_csv(cleaned, "cleaned.csv")

##--------China------------
cleaned <- read_csv("cleaned.csv", col_types = "iicdcc") 

china <- cleaned %>% filter(country != "CHN") %>%
  mutate(china = if_else(str_detect(word, "china|chinese"), 1, 0))  

china2 <- china %>%  select(china, id) %>% group_by(id) %>% 
  summarise(chinasum = sum(china)) %>% 
  filter(chinasum > 0) %>% select(id)

china <- china %>% semi_join(china2, by = "id")

cihnacount <- china %>% group_by(year) %>% summarise(chinacount = sum(china))
#cihnacount2 <- china %>% group_by(year, country) %>% summarise(chinacount = sum(china))



#cihnacount2 %>% ungroup() %>% group_by(year) %>% arrange(year, desc(chinacount)) %>% top_n(1) %>%
#  mutate(country2 = countrycode(country, "iso3c", "iso.name.en")) %>% .$country2


#?countrycode()



cihnacount %>% ggplot(aes(year,chinacount)) + geom_line()

chinaword <- china %>% filter(!(word %in% c("china",  "republic", "united", "nations", "people", "peoples", "chinese", "country", "countries")) ) %>%
  group_by(year, word) %>% summarise(freqword = n() ) %>% arrange(year, desc(freqword))

chinawordsum <- chinaword %>% group_by(year) %>% summarise(freqsum = sum(freqword))

chinaword <- chinaword %>% inner_join(chinawordsum, by = "year") %>% mutate(tf = freqword/freqsum) %>% filter(tf > 0.004)



chinaword %>% filter(year==2015)%>% with(wordcloud(word, freqword, max.words = 20))




chinaword %>% inner_join(get_sentiments("bing")) %>% 
  mutate(sentiment2 = if_else(sentiment=="positive", 1, -1) ) %>% 
  mutate(score = freqword*sentiment2) %>% group_by(year) %>% 
  summarise(totalscore = sum(score)) %>% ggplot(aes(year, totalscore)) + geom_line()


##--------russia------------


russia <- cleaned %>% filter(country != "RUS") %>%
  mutate(russia = if_else(str_detect(word, "russia|soviet"), 1, 0)) 

russia2 <- russia %>%  select(russia, id) %>% group_by(id) %>% 
  summarise(russiasum = sum(russia)) %>% 
  filter(russiasum > 0) %>% select(id)

russia <- russia %>% semi_join(russia2, by = "id")

russiacount <- russia %>% group_by(year) %>% summarise(russiacount = sum(russia))

russiacount %>% ggplot(aes(year,russiacount)) + geom_line()

russiaword <- russia %>% filter(!(word %in% c("russia", "russian", "sovietunion", "soviet", "union", "united", "nations", "countries", "country")) ) %>%
  group_by(year, word) %>% summarise(freqword = n() ) %>% arrange(year, desc(freqword))

russiawordsum <- russiaword %>% group_by(year) %>% summarise(freqsum = sum(freqword))

russiaword <- russiaword %>% inner_join(russiawordsum, by = "year") %>% mutate(tf = freqword/freqsum) %>% filter(tf > 0.004)



russiaword %>% filter(year==1978)%>% with(wordcloud(word, freqword, max.words = 10))


russiaword %>% inner_join(get_sentiments("bing")) %>% 
  mutate(sentiment2 = if_else(sentiment=="positive", 1, -1) ) %>% 
  mutate(score = freqword*sentiment2) %>% group_by(year) %>% 
  summarise(totalscore = sum(score)) %>% ggplot(aes(year, totalscore)) + geom_line()



##--------USA------------


usa <- cleaned %>% filter(country != "USA") %>%
  mutate(usa = if_else(str_detect(word, "america|unitedstates|u.s|usa"), 1, 0)) 

usa2 <- usa %>%  select(usa, id) %>% group_by(id) %>% 
  summarise(usasum = sum(usa)) %>% 
  filter(usasum > 0) %>% select(id)

usa <- usa %>% semi_join(usa2, by = "id")

usacount <- usa %>% group_by(year) %>% summarise(usacount = sum(usa))

usacount %>% ggplot(aes(year,usacount)) + geom_line()

usaword <- usa %>% filter(!(word %in% c("usa", "u.s", "america", "unitedstates",  "united", "nations", "countries", "country")) ) %>%
  group_by(year, word) %>% summarise(freqword = n() ) %>% arrange(year, desc(freqword))

usawordsum <- usaword %>% group_by(year) %>% summarise(freqsum = sum(freqword))

usaword <- usaword %>% inner_join(usawordsum, by = "year") %>% mutate(tf = freqword/freqsum) %>% filter(tf > 0.004)




usaword %>% filter(year==2004)%>% with(wordcloud(word, freqword, max.words = 10))


usaword %>% inner_join(get_sentiments("bing")) %>% 
  mutate(sentiment2 = if_else(sentiment=="positive", 1, -1) ) %>% 
  mutate(score = freqword*sentiment2) %>% group_by(year) %>% 
  summarise(totalscore = sum(score)) %>% ggplot(aes(year, totalscore)) + geom_line()


##--------GBR------------


uk <- cleaned %>% filter(country != "GBR") %>%
  mutate(uk = if_else(str_detect(word, "greatbritain|unitedkingdom|u.k|uk|british"), 1, 0)) 

uk2 <- uk %>%  select(uk, id) %>% group_by(id) %>% 
  summarise(uksum = sum(uk)) %>% 
  filter(uksum > 0) %>% select(id)

uk <- uk %>% semi_join(uk, by = "id")

ukcount <- uk %>% group_by(year) %>% summarise(ukcount = sum(uk))

ukcount %>% ggplot(aes(year,ukcount)) + geom_line()

ukword <- uk %>% filter(!(word %in% c("uk", "u.k", "greatbritain", "unitedkingdom","british",  "united", "nations", "countries", "country")) ) %>%
  group_by(year, word) %>% summarise(freqword = n() ) %>% arrange(year, desc(freqword))

ukwordsum <- ukword %>% group_by(year) %>% summarise(freqsum = sum(freqword))

ukword <- ukword %>% inner_join(ukwordsum, by = "year") %>% mutate(tf = freqword/freqsum) %>% filter(tf > 0.004)



ukword %>% filter(year==2004)%>% with(wordcloud(word, freqword, max.words = 10))


ukword %>% inner_join(get_sentiments("bing")) %>% 
  mutate(sentiment2 = if_else(sentiment=="positive", 1, -1) ) %>% 
  mutate(score = freqword*sentiment2) %>% group_by(year) %>% 
  summarise(totalscore = sum(score)) %>% ggplot(aes(year, totalscore)) + geom_line()


##---------------France----------

fra <- cleaned %>% filter(country != "FRA") %>%
  mutate(fra = if_else(str_detect(word, "france|french"), 1, 0)) 

fra2 <- fra %>%  select(fra, id) %>% group_by(id) %>% 
  summarise(frasum = sum(fra)) %>% 
  filter(frasum > 0) %>% select(id)

fra <- fra %>% semi_join(fra2, by = "id")

fracount <- fra %>% group_by(year) %>% summarise(fracount = sum(fra))

fracount %>% ggplot(aes(year,fracount)) + geom_line()

fraword <- fra %>% filter(!(word %in% c("france", "french", "united", "nations", "countries", "country")) ) %>%
  group_by(year, word) %>% summarise(freqword = n() ) %>% arrange(year, desc(freqword))

frawordsum <- fraword %>% group_by(year) %>% summarise(freqsum = sum(freqword))

fraword <- fraword %>% inner_join(frawordsum, by = "year") %>% mutate(tf = freqword/freqsum) %>% filter(tf > 0.004)




fraword %>% filter(year==2003)%>% with(wordcloud(word, freqword, max.words = 10))


fraword %>% inner_join(get_sentiments("bing")) %>% 
  mutate(sentiment2 = if_else(sentiment=="positive", 1, -1) ) %>% 
  mutate(score = freqword*sentiment2) %>% group_by(year) %>% 
  summarise(totalscore = sum(score)) %>% ggplot(aes(year, totalscore)) + geom_line()


##------ combine
chinaword <- chinaword %>% mutate(country="China")
russiaword <- russiaword %>% mutate(country="Russia")
usaword <- usaword %>% mutate(country="USA")
ukword <- ukword %>% mutate(country="UK")
fraword <- fraword %>% mutate(country="France")

securitycouncil_words <- bind_rows(chinaword, russiaword, usaword, ukword, fraword)


cihnacount <- cihnacount %>% rename(count=chinacount) %>% mutate(country = "China")
russiacount <- russiacount %>% rename(count=russiacount) %>% mutate(country = "Russia")
usacount <- usacount %>% rename(count=usacount) %>% mutate(country = "USA")
ukcount <- ukcount %>% rename(count=ukcount) %>% mutate(country = "UK")
fracount <- fracount %>% rename(count=fracount) %>% mutate(country = "France")

securitycouncil_count <- bind_rows(cihnacount, russiacount, usacount, ukcount, fracount)

############################

#write_csv(securitycouncil_words, "securitycouncil_words.csv")
#write_csv(securitycouncil_count, "securitycouncil_count.csv")

securitycouncil_words <- read_csv("securitycouncil_words.csv")
securitycouncil_count <- read_csv("securitycouncil_count.csv")

securitycouncil_count %>% filter(country == "China") %>% ggplot(aes(year,count)) + geom_line()
securitycouncil_words %>% filter(country == "Russia") %>% filter(year==2015)%>% with(wordcloud(word, freqword, max.words = 10))



securitycouncil_words %>%
  filter(country == "China") %>% 
  filter(year==2014) %>%
  top_n(10, tf) %>%
  mutate(word = reorder(word, tf)) %>%
  ggplot(aes(word, tf)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

########################

scores <- securitycouncil_words %>% mutate(id = paste(as.character(year), country) ) %>%
  #filter(country == "China") %>%
  inner_join(get_sentiments("bing")) %>% 
  mutate(sentiment2 = if_else(sentiment=="positive", 1, -1) ) %>%
  mutate(score = freqword*sentiment2) %>% group_by(id) %>% 
  summarise(totalscore = sum(score)) 

securitycouncil_words %>% mutate(id = paste(as.character(year), country) ) %>% inner_join(scores, by = "id")


total <- securitycouncil_words %>% mutate(id = paste(as.character(year), country) ) %>%
  #filter(country == "China") %>%
  inner_join(get_sentiments("bing")) %>% 
  mutate(sentiment2 = if_else(sentiment=="positive", 1, -1) ) %>%
  mutate(score = freqword*sentiment2) %>% mutate(score2 = abs(score)) %>% group_by(id, sentiment) %>% 
  summarise(score3 = sum(score2)) %>% ungroup() %>% group_by(id) %>% summarise(total = sum(score3))


a <- securitycouncil_words %>% mutate(id = paste(as.character(year), country) ) %>%
  #filter(country == "China") %>%
  inner_join(get_sentiments("bing")) %>% 
  mutate(sentiment2 = if_else(sentiment=="positive", 1, -1) ) %>%
  mutate(score = freqword*sentiment2) %>% mutate(score2 = abs(score)) %>% group_by(id, sentiment) %>% 
  summarise(score3 = sum(score2)) %>% inner_join(total) %>% mutate(share = score3/total)


b <- tibble(trash = 0, sentiment = c("positive", "negative") )

c<- a %>% mutate(trash = 0) %>% select(id, trash) %>% distinct() %>% inner_join(b)  %>% rename(sentiment2 = sentiment) %>%
  mutate(id2 = paste(id, sentiment2) )

c

?inner_join()

a <- a %>% mutate(id2 = paste(id, sentiment))


#a %>% mutate(id2 = paste(id, sentiment)) 
#a


sentiment <- a %>% full_join(c, by = "id2") %>% ungroup() %>% arrange(id2) %>% mutate(id.x = if_else(is.na(share), id.y, id.x),
                                                                                      sentiment = if_else(is.na(sentiment), sentiment2, sentiment),
                                                                                      share = if_else(is.na(share), 0, share)) %>%
  select(id.x, sentiment, share) %>% rename(id = id.x) %>% separate(id, c("year", "country"), sep = " ")


sentiment 


