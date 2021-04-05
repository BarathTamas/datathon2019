## Strike Force p<0.05
### About our team

We are all first year students of the Master of Statistics here at KU Leuven. The team consists of: Tamas Barath, Atsuki Kojima, Blazej Makosa, Nicola Schwemmle, Pascal Wiltschko.


This is our creation for the 2019 Datathon competition at KU Leuven. It ended up winning "Best Data Visualization".


### About the tool

"Transparency is important, so we created a tool to investigate the content of UN addresses. "

The dataset was a text dump of all the speeches at UN sessions to date. Most of the data was likely generated by digitalizing printed records, using outdated software (doing character level recognition without NLP), as it was full of mistakes, like a latin character replaced by similar cyrillic characters in the middle of a word with latin alphabet. This meant that a really long time had to be spent preprocessing (mostly by me), taking time from developing the app :').

The tool consists of two dashboards. The first one displays the most "trending" words among those mentioned at least 10,000 times in the dataset. Trending in this context means that the frequency changes over time, the motivation being that words that are always used equally frequently are not terribly interesting when looking for patterns. To find the trends, a binomial GLM was fitted to the time series of every word's relative frequency (usage in given year/total usage over all years). To be exact the year was used as the predictor, with the relative word frequency being the [0-1] probability of success being predicted. The words where the regression coefficient had p<0.01 were loaded into the app. Indeed, the words extracted this way were mostly interesting (eg. terrorism, arms, etc). The user can select a set of these words and their frequency over time will be plotted as a simple line chart. When a data point is clicked (word in a given year) 3 tables on the bottom display the session of the year with a link to it's documents, the top 5 countries using the word the most and the option to look for the word in the speeches of these countries to see the context. Finally, there is a clustered correlation heatmap visualizing which words tend to be used together in the same year. Hovering over a point shows the words and their exact correlation to help navigate the plot.

![Alt text](https://github.com/BarathTamas/datathon2019/blob/master/dash1.JPG "Dash1")
![Alt text](https://github.com/BarathTamas/datathon2019/blob/master/findquote.JPG "Quotes")

## Hindsight
Unfortunately the Datathon of 2021 never happened at KU Leuven, most likely due to corona. This app was my first time working with Shiny (developed as part of a team effort over 2 days), and I would do many things differently today. My second attempt with Shiny, working on the "Telraam Police Tool" (this time implementing the app on my own) in 2020 (https://github.com/BarathTamas/Datathon2020-Telraam-Police-Tool) was already far better, and I was really hoping to deliver an even better dashboard in 2021, this time with a more sophisticated tool than Shiny, but oh well...

Be warned the Shiny app is clunky! The dataset is pretty big and stored in .csv, and we did not really have the time to optimize things. 
The main problem with app is that due to everything being text data, short of compressing things there wasn't really any way to reduce the size, but that would of course not help with being faster. Most of the heavy lifting with the data is already done, all the app does is some lookups and joins, and that is still not fast enough. It also looks pretty barebones, and now also severely outdated.
