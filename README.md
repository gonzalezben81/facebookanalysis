# facebookanalysis

Facebookanalysis allows a user to retrieve and quanitfy the emotions present within their Facebook messenger data. The package retrieves the messages from the downloadable json file from their account. The package will unpackage the data and make it more human readable for the user. The data is able to be exported to a csv file as well as have an rmarkdown pdf file created for each conversation the user downloads. 


### How to pull the message data from your downloaded .zip file.

This function allows you to pull the data from the **.zip** file that Facebook generates for you. This function will pull the data and create a new folder/directory called **facebookmessages** where your data will have been extracted to.
```r
facebook_unzip(path = 'facebook.zip')
```


### Create the message csv and txt file to be used with other functions.

This function allows you to pull the messages from your the directory that was created before utilizing the **facebook_unzip** function. The messages that are in **json** format will be in the **inbox** folder that was extracted using the **facebook_unzip** function. The data will be put into a folder called **messages** to later be used with the **facebook_sentiment_calculator**,**facebook_report**, and **facebook_heartbeat** functions in this package.

```r
facebook_message_pull(folder = './facebookmessages/messages/inbox')
```


### Retrieve and create the sentiment from your message data.

The sentiment that is in the messages you have downloaded can be retrieved utilizing this function. The user will be able to create a PDF report as well as separate Emotional Sentiment and Positive Vs. Negative sentiment PDF's for each individual conversation. The Emotional Sentiment and Positive Vs. Negative sentiment PDF's will be placed in a separate **image** folder when utilizing this function. 
```r
facebook_sentiment_calculator(folder = 'messages')
```

### Heartbeat Plots.

The heartbeat plot that is created using this function shows the emotional valence of the message that took place. The plot shows the valence over time from the beginning of the conversation until the last words. 

```r
facebook_heartbeat(folder = 'messages')
```