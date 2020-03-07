# Basketball-Analytics-App
These are the scripts I have use in my "Basketball-Analytics-App" that I have submitted for the Shiny Contest 2020. 
In this respository you will find the following: 


## 1. App Scripts
You will find both server.R and ui.R scripts.
Beisdes, I have also created a helper.R file which helpes loading packages. 
Regarding this last file, I don't remember which library gave me an error (I think it was plotly), but this file solved the problem.

## 2. Data Sources & how I have get them
You will find all .csv files that I have used. Besides on the "Extraccion Datos.R" file you will find the code I have used to get all that data. 
In this regard, I have done two things: 
  1. Use MySportsFeed API. It's free for non-commercial/educational uses, so you will get your own API Key if you request it. 
  2. Scrap basketball reference website to get Kobe Bryant data. 

Maybe there are some differences between the raw data you get from this data intake (either API or scraping).
In case these differences do exist, they will be minor, so I don't think you will have any problem converting them into de final .csv format. 
In any case, if you just want to replicate, you have the csv file. 

## 3. Scripts & Styles
On the www folder you will find some JavaScript files that I have used, together with the styles.css files. 
The JavaScripts do the following:
  1. **confetti.js**: creates the confetti effect when a comparison is undertaken in the second tabset. Basically what it does is check if there is a canvas element on the DOM. If not it creates it. Obviously it also creates all functions to lunch, stop, toggle, etc. the confetti.
  2. **kobe.js**: this JS creates and deletes classes depending on whether you are on the Kobe tab or not. This enables me to give a different style to the whole App when the Kobe tab is activated.
  3. **prueba.js**: basically creates an observer that launches confetti when a comparison element changes in the DOM, that is, when a comparison is undertaken. 
  
Credits: mathusummut for developing confetti.js. Thanks! 
  

