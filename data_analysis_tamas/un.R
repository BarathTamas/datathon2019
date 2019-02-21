library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(gridExtra)
library(scales)
library(reshape2)
library("wordcloud") # word-cloud generator 
library("RColorBrewer") # color palettes
library(stringr)
library(purrr)
library(broom)
library(tidyquant)
library(pheatmap)

#read in the csv, its too big for github so you have to manually select the data folder
text_df_raw=read.table("C:/KU Leuven/Datathon/UN/un_raw.csv") %>%
  mutate(day="09-15") %>%          #for plotting its better to have concrete dates instead of years, the sessions have a
  unite(date,year,day,sep="-") %>% #moving starting date around the third Tuesday of September, so I approximate with 09.15
  mutate(date=as.POSIXct(date))    #transforms the year into POSIXct date format, this is best for plotting timeseries

text_df_raw <- text_df_raw %>% mutate_at(4, as.character) #transform texts into string from factor

text_df <- text_df_raw %>% unnest_tokens(word, text)      #splitting into words


####finding trending words---------------------
#count the frequency of words for every year
words_ts <- text_df %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  na.omit() %>%
  anti_join(stop_words) %>% #remove functional words
  count(date,word) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(year_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  filter(word_total > 10000) #only consider words that appear more than 10000 times

#fitting glm to find trends
nested_data <- words_ts %>%
  nest(-word)
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(n, year_total-n) ~ date, .,
                                  family = "binomial")))
#most significant trends
slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "date") %>%
  mutate(adjusted.p.value = p.adjust(p.value)) %>%
  filter(adjusted.p.value < 0.01) %>%  #only take words that are p<1%
  select(word, estimate, adjusted.p.value)

top_slopes <- slopes %>% top_n(-50)

top_words_ts <- words_ts %>%
  select(date,word,n) %>% 
  filter(word %in% top_slopes$word) %>% 
  spread(key = word, value = n)
word_corr <- cor(top_words_ts[,2:50])

#correlated trends
pal=colorRampPalette(rev(brewer.pal(11,"RdYlBu")))(100)
pheatmap(word_corr,color=pal,treeheight_row = 0, treeheight_col = 0)

#custom selection of trending words
my_words1=data_frame(word=c("africa","disarmament","terrorism","independence","poverty",
                            "powers","south","financial","arab","military",
                            "east","negotiations","developing","conflict","israel","middle","nuclear"))

my_words2=data_frame(word=c("arab","israel","arms","middle","force"))

my_words3=data_frame(word=c("terrorism","independence","financial","nuclear"))

#plotting the trends
words_ts %>%
  inner_join(my_words3, by = "word") %>%
  ggplot(aes(date, n, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency") +
  scale_x_datetime(breaks = seq(as.POSIXct("1970-01-01"),
                                as.POSIXct("2015-12-31"), "4 years"),
                   minor_breaks=seq(as.POSIXct("1970-01-01"),
                                    as.POSIXct("2015-12-31"), "1 years"),
                   expand = c(0, 0),
                   limits = c(as.POSIXct("1969-12-01"),
                              as.POSIXct("2016-01-31"))) +
  theme_bw()+
  theme(panel.grid.minor.y = element_blank())

#save the dataframes for the webapp
write.table(words_ts,"shiny app/words_ts.csv",row.names=F)
write.table(top_slopes["word"],"shiny app/trending words.csv",row.names=F)
write.table(word_corr,"shiny app/word_correlations.csv")




####session info------------------

session_theme <- read.table("shiny app/session_descr.csv",header = TRUE, stringsAsFactors = FALSE)

session_info=tibble(
  date=seq(as.POSIXct("1970-09-15"),as.POSIXct("2015-09-15"), "1 years"),
                    session=as.integer(seq(25,70,1))) %>%
  left_join(session_theme,by="session") %>% 
  mutate(link=paste("http://research.un.org/en/docs/ga/quick/regular/",session,sep=""))

write.table(session_info, "shiny app/session_info.csv")


####word users--------------

#who used the words the most in a given year?
keyword="financial"
keydate=as.POSIXct("1998-09-15")

country_codes=read.table("shiny app/country_codes.csv")

date_country_word <- text_df %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  na.omit() %>%
  anti_join(stop_words) %>% #remove functional words
  count(date,country,word) %>%
  ungroup() %>%
  filter(word %in% as.matrix(top_slopes["word"])) %>% 
  left_join(country_codes)

#save the dataframe for the webapp
write.table(date_country_word,"shiny app/date_country_word.csv")

result <- date_country_word %>%
  filter(word==keyword) %>%
  filter(date==keydate) %>% 
  count(country)

keycountries=result %>%
  top_n(5) %>% 
  mutate_at(1, as.character)

#extracting sentences of interest with the keyword
text_df2 <-text_df_raw %>%
  unnest_tokens(sentence, text, token="sentences") %>%  #tokenize into sentences
  filter(nchar(sentence)>10)  #remove errors

keytext <- text_df2 %>% 
  filter(date==keydate) %>% 
  filter(country %in% keycountries$country) %>%
  filter(grepl(keyword,sentence))



#--------------not needed-----------------
#financial crisis
sp500 <- tq_get(x = "^GSPC",from="1970-01-01",to="2016-01-01") %>% 
  mutate(date=as.POSIXct(date))

sp500_y <- sp500 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               type       = "log",
               period     = "yearly")


p1 <- sp500 %>%
  ggplot(aes(as.POSIXct(date), adjusted)) +
  geom_line(size = 1.3,color="#5b92e5") +
  scale_x_datetime(breaks = seq(as.POSIXct("1970-01-01"),
                                as.POSIXct("2015-12-31"), "4 years"),
                   minor_breaks=seq(as.POSIXct("1970-01-01"),
                                    as.POSIXct("2015-12-31"), "1 years"),
                   expand = c(0, 0),
                   limits = c(as.POSIXct("1969-12-01"),
                              as.POSIXct("2016-01-31"))) +
  theme_bw()+
  theme(panel.grid.minor.y = element_blank())
p2 <- filter(words_ts,word=="financial") %>%
  ggplot(aes(as.POSIXct(date), n)) +
  geom_line(size = 1.3,color="#5b92e5") +
  geom_point(size=5,alpha=0.5,color="dark blue")+
  scale_x_datetime(breaks = seq(as.POSIXct("1970-01-01"),
                                as.POSIXct("2015-12-31"), "4 years"),
                   minor_breaks=seq(as.POSIXct("1970-01-01"),
                                    as.POSIXct("2015-12-31"), "1 years"),
                   expand = c(0, 0),
                   limits = c(as.POSIXct("1969-12-01"),
                              as.POSIXct("2016-01-31"))) +
  theme_bw()+
  theme(panel.grid.minor.y = element_blank())
grid.arrange(p1,p2,nrow=2)

un_df <- text_df %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  anti_join(stop_words) %>% #removing function words
  count(word, sort = TRUE)
#maybe united, nations and international should be removed as well

un_df[1:20,] %>%
  mutate(word = reorder(word, n)) %>% #reorder neccesary as ggplot doesnt care about sorting
  ggplot(aes(word, n,label=word)) +
  geom_col(fill="#5b92e5",color="#5b92e5") +
  xlab(NULL) +
  ylab(NULL) +
  geom_text(size = 5, position = position_stack(vjust = 0.5),
            color="white",fontface = "bold") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


wordcloud(words = un_df$word, freq = un_df$n,
          max.words=20, random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"))



#tf-idf
un_idf <- text_df %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  group_by(date) %>%
  count(date,word) %>%
  bind_tf_idf(word, date, n) %>%
  arrange(desc(tf_idf))

ts_idfs <- un_idf %>%
  top_n(5) %>%
  arrange(desc(date,tf_idf))
#--------------------------------

