#' Ensurer Contract for Remark Validity
#'
#' @noRd
is_valid_remark <- ensures(
  is.data.frame ~
      "The remarks should be in a data.frame!"
 ,identical(colnames(.), c("Key", "Timestamp", "Level", "Remark", "Origin")) ~
      "The remarks data.frame has wrong column names!"
 ,identical(lapply(., class), lapply(no_remarks(), class)) ~
      "The remarks data.frame has wrong column classes!"
)

#' Create An Empty Remarks Set
#'
#' @importFrom dplyr tbl_df
#' @return \code{data.frame (Timestamp, Level, Remark, Origin)}
#' @noRd
no_remarks <- function()
{
  tbl_df(data.frame(Key       = character()
                   ,Timestamp = Sys.time()[integer(0)]
                   ,Level     = character()
                   ,Remark    = character()
                   ,Origin    = character()
                   ,stringsAsFactors = FALSE))
}

#' R6 Remarks Database Class
#'
#' @noRd
RemarksDB <- R6Class("Remarks",
  private = list(
    database = no_remarks()
  ),
  public  = list(
    add   = function(remark)
    {
      private$database <-
        rbind_list(private$database, ensure(remark, +is_valid_remark))
    },
    get   = function(...)
    {
      keys <- as.character(unlist(list(...)))
      if (length(keys) > 0)
        filter_(private$database, ~ Key %in% keys)
      else
        private$database
    },
    clear = function(...)
    {
      keys <- as.character(unlist(list(...)))
      if (length(keys) > 0)
        private$database <- filter_(private$database, ~ !Key %in% keys)
      else
        private$database <- no_remarks()
    }
  )
)

#' The Remarks Database Instance
#'
#' @noRd
.remarks <- RemarksDB$new()


#' Retrieve Remarks Associated With Key(s)
#'
#' This will retrieve remarks associated with one or more keys. If no remarks
#' exist an empty data.frame is returned.
#'
#' @details Each row in a remarks \code{data.frame} represent a "remark".
#' A remark has its "Key", a "Timestamp" (\code{POSIXct}), A severity "Level",
#' for categorizing types of messages e.g. differentiating
#' their importance, the "Remark" itself (\code{character}), and
#' an "Origin" (\code{character} identifying where the remark originated,
#' e.g. a function or application).
#'
#' @param ... character: The keys for which to ger remarks.
#' @param filter An optional (one-sided) formula for filtering remarks further,
#'   see \code{\link[dplyr]{filter_}}.
#'
#' @return A \code{data.frame} with columns \code{Key, Timestamp, Level, Remark,
#' Origin}.
#'
#' @importFrom dplyr filter_
#' @export
remarks <- function(..., filter = NULL)
{
  r <- .remarks$get(...)
  if (!is.null(filter) && inherits(filter, "formula"))
    filter_(r, filter)
  else
    r
}


#' Clear Remarks For Key(s)
#'
#' @param ... keys for which to clear remarks
#'
#' @export
clear_remarks <- function(...)
{
  .remarks$clear(...)
}

#' Create or Add a Remark
#'
#' There are two versions of this function. The "dot" version is pipe-friendly
#' and will take as a first argument a value which is then also returned.
#'
#' @param key character: The remark key.
#' @param level character: The severity level of your remark.
#' @param remark character: The remark itself. This can be a string interpolation
#'   formula.
#' @param origin character: The origin of your remark.
#'
#' @return A remark \code{data.frame}.
#' @export
remark <- function(key, level, remark, origin = NA_character_)
{
  if (inherits(remark, "formula"))
    remark <- str_interp(remark[[2L]], parent.frame())

  if (inherits(origin, "formula"))
    origin <- str_interp(origin[[2L]], parent.frame())

  r <- data.frame(Key       = as.character(key)
                 ,Timestamp = Sys.time()
                 ,Level     = level
                 ,Remark    = remark
                 ,Origin    = origin
                 ,stringsAsFactors = FALSE)

  .remarks$add(r)
}


#' @rdname remark
#'
#' @param . A value which is available for string interpolation, and is returned.
#'
#' @export
remark. <- function(., key, level, remark, origin = NA_character_)
{
  interp_remark <- inherits(remark, "formula")
  interp_origin <- inherits(origin, "formula")

  if (interp_remark ||interp_origin) {
    e <- new.env(parent = parent.frame())
    e[["."]] <- .
    if (interp_remark)
      remark <- str_interp(remark[[2L]], e)
    if (interp_origin)
      origin <- str_interp(origin[[2L]], e)
  }

  r <- data.frame(Key       = as.character(key)
                 ,Timestamp = Sys.time()
                 ,Level     = level
                 ,Remark    = remark
                 ,Origin    = origin
                 ,stringsAsFactors = FALSE)

  .remarks$add(r)
  .
}


