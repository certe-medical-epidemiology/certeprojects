% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pins.R
\name{pins}
\alias{pins}
\alias{export_pin}
\alias{import_pin}
\alias{remove_pin}
\alias{pins_board}
\title{Work with Pins}
\usage{
export_pin(
  x,
  name = NULL,
  title = NULL,
  type = NULL,
  description = NULL,
  board = pins_board()
)

import_pin(name, version = NULL, hash = NULL, board = pins_board())

remove_pin(name, version = NULL, board = pins_board())

pins_board(
  projects_channel_id = read_secret("teams.projects.channel_id"),
  account = connect_teams(),
  delete_by_item = TRUE
)
}
\arguments{
\item{x}{An object (typically a data frame) to pin.}

\item{name}{Pin name.}

\item{title}{A title for the pin; most important for shared boards so that
others can understand what the pin contains. If omitted, a brief
description of the contents will be automatically generated.}

\item{type}{File type used to save \code{x} to disk. Must be one of
"csv", "json", "rds", "parquet", "arrow", or "qs". If not supplied, will
use JSON for bare lists and RDS for everything else. Be aware that CSV and
JSON are plain text formats, while RDS, Parquet, Arrow, and
\href{https://CRAN.R-project.org/package=qs}{qs} are binary formats.}

\item{description}{A detailed description of the pin contents.}

\item{board}{A pin board, created by \code{\link[pins:board_folder]{board_folder()}}, \code{\link[pins:board_connect]{board_connect()}},
\code{\link[pins:board_url]{board_url()}} or another \code{board_} function.}

\item{version}{Retrieve a specific version of a pin. Use \code{\link[pins:pin_versions]{pin_versions()}} to
find out which versions are available and when they were created.}

\item{hash}{Specify a hash to verify that you get exactly the dataset that
you expect. You can find the hash of an existing pin by looking for
\code{pin_hash} in \code{\link[pins:pin_meta]{pin_meta()}}.}

\item{projects_channel_id}{Teams channel ID of the projects}

\item{account}{a Microsoft 365 account to use for looking up properties. This has to be an object as returned by \code{\link[=connect_teams]{connect_teams()}} or \code{\link[Microsoft365R:client]{Microsoft365R::get_team()}}.}

\item{delete_by_item}{Whether to handle folder deletions on an item-by-item
basis, rather than deleting the entire folder at once. You may need to set
this to \code{TRUE} for a board in SharePoint Online or OneDrive for Business,
due to document protection policies that prohibit deleting non-empty
folders.}
}
\description{
These functions can be used to work with \href{https://pins.rstudio.com}{\code{pins}}, developed by RStudio.
}
\details{
These functions from the \href{https://pins.rstudio.com}{\code{pins}} package are integrated into the team's Microsoft 365 account, using the \code{"pins"} folder in the given MS Teams channel.

For Pins functions of the \code{pins} package, use \code{\link[=pins_board]{pins_board()}} as input, e.g.:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{  pin_list(pins_board())
}\if{html}{\out{</div>}}

The following \code{pins} functions are re-exported by this package: \code{\link[=pin_list]{pin_list()}}, \code{\link[=pin_meta]{pin_meta()}}, and \code{\link[=pin_versions]{pin_versions()}}.

The \code{\link[=pins_board]{pins_board()}} function returns a \link[pins:board_ms365]{pins::board_ms365} object based on the \code{"pins"} folder in the Teams channel \emph{Projects}, which is retrieved with \code{\link[=teams_projects_channel]{teams_projects_channel()}}.
}
