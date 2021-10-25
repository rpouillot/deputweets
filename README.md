# deputweets
 Code from the <a href="http://www.thesearemyapps.com/deputweets">deputweets</a> app and 
 <a href="https://twitter.com/deputweets">@deputweets</a> bot.

This repository includes: 
<ul>
  <li>the R code from the <a href="http://www.thesearemyapps.com/deputweets">deputweets</a> R shiny app 
(see the Deputweets directory);</li> 
  <li>the R code that fetches photos, account information and tweets from Twitter to feed the app (see the `fetch.R`);</li> 
  <li>the R code that publishes the tweets from the <a href="https://twitter.com/deputweets">@deputweets</a>
account (see the `post_best_of.R` file);</li>
  <li>An MS Excel file including public data on the French Government and members of 
  parliament ("*D&eacute;put&eacute;s*").</li> 
</ul>

Important note: the code won't work as is. You will need to obtain a Twitter token, 
then run `fetch.r` once to get the database ready.

