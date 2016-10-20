#' Get a user's timeline
#' @param user - User id.
#' @param n - Maximum number of tweets. Max of n in userTimeline is 3200, so the default is that number.
#' @param includeRts - Whether retweets should be included in the result.
#' @param includeReplies - Whether replies should be included in the result.
#' @param tokenFileId - File id for auth token.
#' @export
getTwitterTimeline <- function(user, n=3200, includeRts = FALSE,
                               includeReplies = TRUE, tokenFileId){
  if(!requireNamespace("twitteR")){stop("package twitteR must be installed.")}

  twitter_token = getTwitterToken(tokenFileId)
  twitteR::use_oauth_token(twitter_token)

  # use includeReplies and reverse it for excludeReplies argument for argument consistency
  excludeReplies <- !includeReplies

  ret <- twitteR::userTimeline(user, n = n, includeRts = includeRts, excludeReplies = excludeReplies)

  if(length(ret)>0){
    twitteR::twListToDF(ret)
  } else {
    stop('No Tweets found.')
  }
}
