# facebookanalysis

Facebookanalysis allows a user to retrieve and quanitfy the emotions present within their Facebook messenger data. The package retrieves the messages from the downloadable json file from their account. The package will unpackage the data and make it more human readable for the user. The data is able to be exported to a csv file as well as have an rmarkdown pdf file created for each conversation the user downloads. 


### How to pull the message data from your downloaded .zip file.
```r
facebook_unzip(path = data)
```


### Create the message csv file.
```r
facebook_message_pull(folder = 'messages')
```


### Retrieve and create the sentiment from your message data.
```r
facebook_sentiment_calculator(folder = data)
```