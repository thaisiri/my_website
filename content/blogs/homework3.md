---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2021-09-30"
description: Money in UK politics # the title that will show up once someone gets to this page
draft: false
image: uk.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: homework3 # slug is the shorthand URL address... no spaces plz
title: Money in UK politics
---





[The Westminster Accounts](https://news.sky.com/story/the-westminster-accounts-12786091), a recent collaboration between Sky News and Tortoise Media, examines the flow of money through UK politics. It does so by combining data from three key sources:

1.  [Register of Members' Financial Interests](https://www.parliament.uk/mps-lords-and-offices/standards-and-financial-interests/parliamentary-commissioner-for-standards/registers-of-interests/register-of-members-financial-interests/),
2.  [Electoral Commission records of donations to parties](http://search.electoralcommission.org.uk/English/Search/Donations), and
3.  [Register of All-Party Parliamentary Groups](https://www.parliament.uk/mps-lords-and-offices/standards-and-financial-interests/parliamentary-commissioner-for-standards/registers-of-interests/register-of-all-party-party-parliamentary-groups/).

You can [search and explore the results](https://news.sky.com/story/westminster-accounts-search-for-your-mp-or-enter-your-full-postcode-12771627) through the collaboration's interactive database. Simon Willison [has extracted a database](https://til.simonwillison.net/shot-scraper/scraping-flourish) and this is what we will be working with. If you want to read more about [the project's methodology](https://www.tortoisemedia.com/2023/01/08/the-westminster-accounts-methodology/).

## Open a connection to the database

The database made available by Simon Willison is an `SQLite` database


```r
sky_westminster <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = here::here("data", "sky-westminster-files.db")
)
```

How many tables does the database have?


```r
library(tidyverse)
library(wbstats)
library(tictoc)
library(skimr)
library(countrycode)
library(here)
library(DBI)
library(dbplyr)
library(arrow)
library(rvest)
library(robotstxt) # check if we're allowed to scrape the data
library(scales)
library(sf)
library(readxl)

sky_westminster <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = here::here("data", "sky-westminster-files.db")
)

DBI::dbListTables(sky_westminster)
```

```
## [1] "appg_donations"  "appgs"           "member_appgs"    "members"        
## [5] "parties"         "party_donations" "payments"
```

```r
#There are 7 tables in the database.
```

## Which MP has received the most amount of money?

You need to work with the `payments` and `members` tables and for now we just want the total among all years. To insert a new, blank chunk of code where you can write your beautiful code (and comments!), please use the following shortcut: `Ctrl + Alt + I` (Windows) or `cmd + option + I` (mac)


```r
payments_db <- dplyr::tbl(sky_westminster,"payments")
members_db <- dplyr::tbl(sky_westminster,"members")

glimpse(members_db)
```

```
## Rows: ??
## Columns: 7
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ id           <chr> "m8", "m1508", "m1423", "m4514", "m1211", "m3958", "m14",…
## $ name         <chr> "Theresa May", "Sir Geoffrey Cox", "Boris Johnson", "Keir…
## $ gender       <chr> "F", "M", "M", "M", "M", "F", "M", "M", "F", "M", "F", "M…
## $ constituency <chr> "Maidenhead", "Torridge and West Devon", "Uxbridge and So…
## $ party_id     <chr> "p4", "p4", "p4", "p15", "p4", "p4", "p4", "p4", "p4", "p…
## $ short_name   <chr> "Mrs May", "Sir Geoffrey", "Mr Johnson", "Mr Starmer", "M…
## $ status       <chr> "active", "active", "active", "active", "active", "active…
```

```r
glimpse(payments_db)
```

```
## Rows: ??
## Columns: 13
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ category             <chr> "4. Visits outside the UK", "2. (b) Any other sup…
## $ category_name        <chr> "Gifts and other benefits", "Cash donations", "Gi…
## $ charity              <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
## $ date                 <chr> "Registered in November 2021", "Registered in Jan…
## $ date_visited         <chr> "Dates of visit: 5-12 November 2021", "", "Dates …
## $ description          <chr> "International flights £805.07; accommodation £1,…
## $ destination_of_visit <chr> "Accra, Ghana", "", "Kingston, Jamaica", "", "", …
## $ entity               <chr> "GUBA Foundation", "Mahir Kilic", "People's Natio…
## $ hours                <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
## $ id                   <chr> "44a5c7f837d9df230b8c1e7f72eea188", "b9f40bd69ac2…
## $ member_id            <chr> "m172", "m172", "m172", "m172", "m172", "m44", "m…
## $ purpose_of_visit     <chr> "To participate in the GUBA Foundation Yaa Asante…
## $ value                <dbl> 2631.51, 2000.00, 2574.57, 2000.00, 500.00, 1800.…
```

```r
members_df <- members_db %>% 
  select(name, id) %>% 
  collect()

payments_df <- payments_db %>% 
  select(member_id, value) %>%
  collect()

members_payments_df <- members_df %>% 
  left_join(payments_df, by = c("id" = "member_id")) %>% 
  filter(!is.na(name)) %>%
  group_by(name) %>% 
  summarise(total_payment = sum(value)) %>% 
  arrange(desc(total_payment))

#print result
members_payments_df
```

```
## # A tibble: 660 × 2
##    name             total_payment
##    <chr>                    <dbl>
##  1 Theresa May           2809765.
##  2 Sir Geoffrey Cox      2191387.
##  3 Boris Johnson         1282402 
##  4 Keir Starmer           799936.
##  5 Andrew Mitchell        769373.
##  6 Fiona Bruce            712321.
##  7 John Redwood           692438.
##  8 Rishi Sunak            546043 
##  9 Liz Truss              538678.
## 10 Ed Davey               441681.
## # ℹ 650 more rows
```

```r
# Theresa May has received the most amount of money.
```

## Any `entity` that accounts for more than 5% of all donations?

Is there any `entity` whose donations account for more than 5% of the total payments given to MPs over the 2020-2022 interval? Who are they and who did they give money to?


```r
payments_db <- dplyr::tbl(sky_westminster,"payments")
members_db <- dplyr::tbl(sky_westminster,"members")

glimpse(members_db)
```

```
## Rows: ??
## Columns: 7
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ id           <chr> "m8", "m1508", "m1423", "m4514", "m1211", "m3958", "m14",…
## $ name         <chr> "Theresa May", "Sir Geoffrey Cox", "Boris Johnson", "Keir…
## $ gender       <chr> "F", "M", "M", "M", "M", "F", "M", "M", "F", "M", "F", "M…
## $ constituency <chr> "Maidenhead", "Torridge and West Devon", "Uxbridge and So…
## $ party_id     <chr> "p4", "p4", "p4", "p15", "p4", "p4", "p4", "p4", "p4", "p…
## $ short_name   <chr> "Mrs May", "Sir Geoffrey", "Mr Johnson", "Mr Starmer", "M…
## $ status       <chr> "active", "active", "active", "active", "active", "active…
```

```r
glimpse(payments_db)
```

```
## Rows: ??
## Columns: 13
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ category             <chr> "4. Visits outside the UK", "2. (b) Any other sup…
## $ category_name        <chr> "Gifts and other benefits", "Cash donations", "Gi…
## $ charity              <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
## $ date                 <chr> "Registered in November 2021", "Registered in Jan…
## $ date_visited         <chr> "Dates of visit: 5-12 November 2021", "", "Dates …
## $ description          <chr> "International flights £805.07; accommodation £1,…
## $ destination_of_visit <chr> "Accra, Ghana", "", "Kingston, Jamaica", "", "", …
## $ entity               <chr> "GUBA Foundation", "Mahir Kilic", "People's Natio…
## $ hours                <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
## $ id                   <chr> "44a5c7f837d9df230b8c1e7f72eea188", "b9f40bd69ac2…
## $ member_id            <chr> "m172", "m172", "m172", "m172", "m172", "m44", "m…
## $ purpose_of_visit     <chr> "To participate in the GUBA Foundation Yaa Asante…
## $ value                <dbl> 2631.51, 2000.00, 2574.57, 2000.00, 500.00, 1800.…
```

```r
members_df <- members_db %>% 
  select(name, id) %>% 
  collect()

payments_df <- payments_db %>% 
  select(member_id, entity, value) %>%
  collect()

members_payments_df <- members_df %>% 
  left_join(payments_df, by = c("id" = "member_id")) %>% 
  filter(!is.na(entity)) %>%
  group_by(entity) %>% 
  summarise(total_payment = sum(value)) %>% 
  filter((total_payment / sum(total_payment))*100 > 5)

members_df %>% 
  left_join(payments_df, by = c("id" = "member_id")) %>% 
  select(entity, name) %>% 
  filter(entity == "Withers LLP")
```

```
## # A tibble: 24 × 2
##    entity      name            
##    <chr>       <chr>           
##  1 Withers LLP Sir Geoffrey Cox
##  2 Withers LLP Sir Geoffrey Cox
##  3 Withers LLP Sir Geoffrey Cox
##  4 Withers LLP Sir Geoffrey Cox
##  5 Withers LLP Sir Geoffrey Cox
##  6 Withers LLP Sir Geoffrey Cox
##  7 Withers LLP Sir Geoffrey Cox
##  8 Withers LLP Sir Geoffrey Cox
##  9 Withers LLP Sir Geoffrey Cox
## 10 Withers LLP Sir Geoffrey Cox
## # ℹ 14 more rows
```

```r
#print result
members_payments_df
```

```
## # A tibble: 1 × 2
##   entity      total_payment
##   <chr>               <dbl>
## 1 Withers LLP      1812732.
```

```r
# Withers LLP accounts for more than 5% of all donations, giving to "Sir Geoffrey Cox"
```

## Do `entity` donors give to a single party or not?

-   How many distinct entities who paid money to MPS are there?
-   How many (as a number and %) donated to MPs belonging to a single party only?


```r
payments_db <- dplyr::tbl(sky_westminster,"payments")
members_db <- dplyr::tbl(sky_westminster,"members")

glimpse(members_db)
```

```
## Rows: ??
## Columns: 7
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ id           <chr> "m8", "m1508", "m1423", "m4514", "m1211", "m3958", "m14",…
## $ name         <chr> "Theresa May", "Sir Geoffrey Cox", "Boris Johnson", "Keir…
## $ gender       <chr> "F", "M", "M", "M", "M", "F", "M", "M", "F", "M", "F", "M…
## $ constituency <chr> "Maidenhead", "Torridge and West Devon", "Uxbridge and So…
## $ party_id     <chr> "p4", "p4", "p4", "p15", "p4", "p4", "p4", "p4", "p4", "p…
## $ short_name   <chr> "Mrs May", "Sir Geoffrey", "Mr Johnson", "Mr Starmer", "M…
## $ status       <chr> "active", "active", "active", "active", "active", "active…
```

```r
glimpse(payments_db)
```

```
## Rows: ??
## Columns: 13
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ category             <chr> "4. Visits outside the UK", "2. (b) Any other sup…
## $ category_name        <chr> "Gifts and other benefits", "Cash donations", "Gi…
## $ charity              <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
## $ date                 <chr> "Registered in November 2021", "Registered in Jan…
## $ date_visited         <chr> "Dates of visit: 5-12 November 2021", "", "Dates …
## $ description          <chr> "International flights £805.07; accommodation £1,…
## $ destination_of_visit <chr> "Accra, Ghana", "", "Kingston, Jamaica", "", "", …
## $ entity               <chr> "GUBA Foundation", "Mahir Kilic", "People's Natio…
## $ hours                <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
## $ id                   <chr> "44a5c7f837d9df230b8c1e7f72eea188", "b9f40bd69ac2…
## $ member_id            <chr> "m172", "m172", "m172", "m172", "m172", "m44", "m…
## $ purpose_of_visit     <chr> "To participate in the GUBA Foundation Yaa Asante…
## $ value                <dbl> 2631.51, 2000.00, 2574.57, 2000.00, 500.00, 1800.…
```

```r
members_df <- members_db %>% 
  select(name, id) %>% 
  collect()

payments_df <- payments_db %>% 
  select(member_id, entity, value) %>%
  collect()

distinct_entities <- payments_df %>% 
  distinct(entity) %>%
  nrow()

distinct_entities
```

```
## [1] 2213
```

```r
#There are 2213 distinct entities who paid money to MPS.

members_payments_df <- members_df %>% 
  left_join(payments_df, by = c("id" = "member_id")) %>% 
  group_by(entity) %>% 
  filter(!is.na(entity)) %>%
  summarise(count = n()) %>% 
  filter(count == 1) %>% 
  nrow()

#calculate percentage
percentage <- members_payments_df/distinct_entities*100

#print results
members_payments_df
```

```
## [1] 1361
```

```r
percentage
```

```
## [1] 61.50023
```

```r
#There are 1361 donation, or 61.50023% of total donation, to MPs belonging to a single party only.
```

## Which party has raised the greatest amount of money in each of the years 2020-2022?

I would like you to write code that generates the following table.


```
## Rows: ??
## Columns: 7
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ id           <chr> "m8", "m1508", "m1423", "m4514", "m1211", "m3958", "m14",…
## $ name         <chr> "Theresa May", "Sir Geoffrey Cox", "Boris Johnson", "Keir…
## $ gender       <chr> "F", "M", "M", "M", "M", "F", "M", "M", "F", "M", "F", "M…
## $ constituency <chr> "Maidenhead", "Torridge and West Devon", "Uxbridge and So…
## $ party_id     <chr> "p4", "p4", "p4", "p15", "p4", "p4", "p4", "p4", "p4", "p…
## $ short_name   <chr> "Mrs May", "Sir Geoffrey", "Mr Johnson", "Mr Starmer", "M…
## $ status       <chr> "active", "active", "active", "active", "active", "active…
```

```
## Rows: ??
## Columns: 13
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ category             <chr> "4. Visits outside the UK", "2. (b) Any other sup…
## $ category_name        <chr> "Gifts and other benefits", "Cash donations", "Gi…
## $ charity              <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
## $ date                 <chr> "Registered in November 2021", "Registered in Jan…
## $ date_visited         <chr> "Dates of visit: 5-12 November 2021", "", "Dates …
## $ description          <chr> "International flights £805.07; accommodation £1,…
## $ destination_of_visit <chr> "Accra, Ghana", "", "Kingston, Jamaica", "", "", …
## $ entity               <chr> "GUBA Foundation", "Mahir Kilic", "People's Natio…
## $ hours                <chr> "", "", "", "", "", "", "", "", "", "", "", "", "…
## $ id                   <chr> "44a5c7f837d9df230b8c1e7f72eea188", "b9f40bd69ac2…
## $ member_id            <chr> "m172", "m172", "m172", "m172", "m172", "m44", "m…
## $ purpose_of_visit     <chr> "To participate in the GUBA Foundation Yaa Asante…
## $ value                <dbl> 2631.51, 2000.00, 2574.57, 2000.00, 500.00, 1800.…
```

```
## Rows: ??
## Columns: 5
## Database: sqlite 3.41.2 [/Users/thaisirisiripoorikan/Desktop/my_website/data/sky-westminster-files.db]
## $ abbrev     <chr> "Alba", "Alliance", "Con", "DUP", "Green", "Ind", "Lab", "L…
## $ background <chr> "0015ff", "C0C0C0", "0000ff", "80", "78b82a", "C0C0C0", "ff…
## $ foreground <chr> "", "FFFFFF", "ffffff", "FFFFFF", "FFFFFF", "FFFFFF", "ffff…
## $ id         <chr> "p1034", "p1", "p4", "p7", "p44", "p8", "p15", "p17", "p22"…
## $ name       <chr> "Alba Party", "Alliance", "Conservative", "Democratic Union…
```

```
## # A tibble: 13 × 2
##    party_name                       total_payment
##    <chr>                                    <dbl>
##  1 Conservative                         25039566.
##  2 Labour                                7076506.
##  3 Liberal Democrats                     1200036.
##  4 Independent                            568760.
##  5 Scottish National Party                348293.
##  6 Sinn Féin                              163170.
##  7 Plaid Cymru                             50964.
##  8 Democratic Unionist Party               35866.
##  9 Green Party                             14346.
## 10 Social Democratic & Labour Party         9850 
## 11 Alba Party                               9149.
## 12 Speaker                                  2899.
## 13 Alliance                                 1192
```

<img src="../../images/total_donations_table.png" width="80%" />

... and then, based on this data, plot the following graph.

<img src="/blogs/homework3_files/figure-html/unnamed-chunk-8-1.png" width="80%" /><img src="../../images/total_donations_graph.png" width="80%" />

This uses the default ggplot colour pallete, as I dont want you to worry about using the [official colours for each party](https://en.wikipedia.org/wiki/Wikipedia:Index_of_United_Kingdom_political_parties_meta_attributes). However, I would like you to ensure the parties are sorted according to total donations and not alphabetically. You may even want to remove some of the smaller parties that hardly register on the graph. Would facetting help you?

Finally, when you are done working with the database, make sure you close the connection, or disconnect from the database.


```r
dbDisconnect(sky_westminster)
```
