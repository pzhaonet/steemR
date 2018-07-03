#' Find which followers have not voted a post yet. avotenot means 'analysis who has not voted yet'.
#'
#' @param postlink A character string of the link to a target post.
#'
#' @return A character vector of the name list of the followers who have not voted the target post yet.
#' @export
#'
#' @examples
#' avotenot('cn/@dapeng/steemit-markdown')
avotenot <- function(postlink = NA){
  if(is.na(postlink)) {
    return(message('Please give a valid id.'))
  }

  vr_votes <- gvotep(post = postlink)$Voter
  postlink <- paste0('https://steemdb.com/', postlink)
  vr_id <- substr(strsplit(postlink, '/')[[1]][5],
                  2,
                  nchar(strsplit(postlink, '/')[[1]][5]))
  vr_follower <- unique(gfollower(id = vr_id)$followers)
  vr_follower <- vr_follower[!(vr_follower %in% vr_votes)]
  if (is.null(vr_follower)) {
    return(NULL)
  } else {
    return(vr_follower[order(vr_follower)])
  }
}


#' Summary of the voters of a series of posts. avotep means 'analysis of the votes on the given posts retrieved with the appbase_api method.'
#'
#' @param mypost A data frame of a series of posts retrieved with the appbase_api method.
#' @param if_plot A logic value of whether plot the pie diagram.
#' @param top A numeric value of the Top voters plotted in the pie diagram.
#'
#' @return A data frame of the voters
#' @export
#'
#' @examples avotep()
avotep <- function(mypost = NA,
                   if_plot = TRUE,
                   top = 10){
  # pre calculation
  voter <- data.frame(
    voter = charsplit(dataframe = mypost$voter),
    rshares = as.numeric(charsplit(dataframe = mypost$voter.rshares)),
    percent = as.numeric(charsplit(dataframe = mypost$voter.percent)) / 100,
    row.names = NULL
  )

  # summry of voters
  voter_sum <- beginr::tapplydf(data = voter,
                                select = 'rshares',
                                myfactor = 'voter',
                                sum)
  voter_percent <- beginr::tapplydf(data = voter,
                                    select = 'percent',
                                    myfactor = 'voter',
                                    mean)
  voter_sum <- merge(voter_sum,
                     voter_percent,
                     by = 'voter')
  voter_sum$percentage <- voter_sum$rshares / sum(voter_sum$rshares)
  voter_sum <- voter_sum[rev(order(voter_sum$rshares)), ]
  row.names(voter_sum) <- 1:nrow(voter_sum)

  # plot the pie diagram
  if (if_plot) {
    pie_df <- voter_sum[voter_sum$percentage >= 0,
                        c('voter', 'percentage')]
    if (nrow(pie_df) > top) {
      pie_df <- pie_df[1:top, ]
    }
    pie = pie(pie_df$percentage, labels = pie_df$voter)
  }

  return(voter_sum)
}


#' Calculate the the daily frequency.
#' adailyf means 'analysis of the daily frequency'.
#'
#' @param mydata A data frame with a date column.
#' @param datecol A chracter string of the date column name. The date column could either be Date or Character (in '\%Y-\%m-\%d' format).
#' @param col A color for plotting
#' @param ylab A character string of the y label
#' @param if_plot A logic value of whether plot the time series
#'
#' @return A dataframe of the daily frequency
#' @export
adailyf <- function(mydata,
                    datecol,
                    if_plot = FALSE,
                    col = 'steelblue3',
                    ylab = 'Daily posts') {
  daily <- data.frame(table(mydata[, datecol]))
  names(daily)[1] <- 'date'
  daily$date <- as.Date(daily$date)
  if(if_plot) {
    pdate(x = daily$date, y = daily$Freq, myxlim = range(daily$date),
         myylim = c(0, max(daily$Freq, na.rm = TRUE) + 1),
         myylab = ylab,
         mycol = col)
  }
  return(daily)
}
