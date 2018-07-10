## Introduction

steemr is an open source R package for playing with Steem data in the R environment. It can download, post process, analyze,  and visualize Steem data on the basis of the powerful statistic power of R. Currently, the version 0.0.0 can:

- obtain the complete post list for a given account,
- obtain the complete account information from steemdb.com,
- organized the 'follower' and 'following' information for a given account,
- obtain the following history of a given account,
- download the vote records of a given post, and
- download the complete data of the latest 100 posts of a given account.

## Quick start

### Installation

```
# From CRAN
install.packages('steemr')
# Or from github repo
install.packages('devtools')
devtools::install_github('pzhaonet/steemr')
library('steemr')
```

If the user would like to use the `method = 'appbase_api'`, then the package 'pzhaonet/steemr2', forked from 'kharoof/steemR', must be installed.

```
devtools::install_github('pzhaonet/steemr2')
```

### Examples 

#### obtain the complete account information

```
gid(id = 'dapeng', method = 'steemdb.com')
gid(id = 'dapeng', method = 'appbase_api')
```

#### Get a name list of an ID's followers and following

```{r}
gfollow('dapeng')
```

#### Get an id's following history

```
gfollowing(id = 'dapeng')
```

#### Get an ID's complete post list with hyperlinks

```{r}
gidposts(id = 'dapeng', method = 'steemdb.com', post_number = 3)
gidposts(id = 'dapeng', method = 'appbase_api')

```

#### Get the complete info of a single given post on steemdb.com

```{r}
mylinks <- c("cn/@dapeng/xuer-sale", "utopian-io/@dapeng/steemg-four-more")
tgpost <- gpost(postlink = mylinks[1], method = 'steemdb.com')
```

#### Get the detailed information of given posts from steemdb.com

```{r}
gposts(postlinks = mylinks, method = 'steemdb.com')
```

#### Get an ID's posts with complete information

```{r}
gidposts(id = 'dapeng', method = 'appbase_api')
gblog(id = 'dapeng')

```

#### Plot an active hour rose diagram from the time stamps of an ID's posts

```{r}
posts <- post_id(id = 'dapeng', method = 'appbase_api')
phour(my_df = posts, col_time = 'datetime')
```

#### A post's vote report

```{r}
gvotep(mylinks[1])
```

#### Find which followers have not voted a post yet

```{r}
avotenot(mylinks[2])
```
#### Get the vote information of given IDs from SteemSQL

```{r}
mysql <- ssql(uid = your_steemsql_id, pwd = your_steemsql_password)
gvoter(voters = c('dapeng', 'pzhao'),
       from = '2018-03-01', to = '2018-05-31',
       if_plot = TRUE, sql_con = mysql)
```
#### Summary of the voters of a series of posts

```{r}
tavotep <- avotep(posts)
```

#### Get the accounts list and analysis within a period

```{r}
gaccounts(sql_con = mysql, if_plot= T)
```

#### Get the comment list and analysis within a period

```{r}
gcomments(id = 'dapeng', sql_con = mysql, if_plot= T)
```

#### Save an ID's posts as markdown files in the local path

```{r}
bmd(post_df = posts)
```
#### Build a Hugo blog site from an ID's posts

```{r}
bblog(author = 'dapeng', post_df = posts, initial = TRUE)
```
More functions are coming soon. Have fun!

## Updates

- 2018-07-10. New functions: `sfollow()`.
- 2018-07-09. New functions: `gspmv()`, `gdelegation()`, `gcner()`, `acnsub()`.
- 2018-07-04. New functions: `bmd()`, `bblog()`.
- 2018-07-03. v0.0.8. Functions renamed. Codes re-organized. New functions for account info and comments info.
- 2018-06-27. v0.0.7. Codes improvement. New functions for vote reports.
- 2018-06-20. v0.0.6. Support AppBase API connection. `hourrose()` added.
- 2018-06-19. On [CRAN](https://CRAN.R-project.org/package=steemr).
- 2018-06-18. v0.0.5. Support SteemSQL and SteemData query. Documentation improved.
- 2018-06-12. v0.0.1. A bug of the hyperlinks was fixed.
- 2018-06-11. v0.0.0. A preliminary version.

## License

Copyright 2018 Peng Zhao.

Released under the MIT license.
