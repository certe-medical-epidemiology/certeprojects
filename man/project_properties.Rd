% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_properties.R
\name{project_properties}
\alias{project_properties}
\alias{project_get_current_id}
\alias{project_identifier}
\alias{project_get_folder}
\alias{project_get_folder_full}
\alias{project_get_title}
\alias{project_get_file}
\alias{project_set_file}
\alias{project_set_folder}
\alias{project_open_analysis_file}
\alias{project_open_folder}
\alias{project_add_qmd_skeleton}
\title{Project Properties}
\usage{
project_get_current_id(ask = NULL, account = connect_planner())

project_identifier(project_number = project_get_current_id())

project_get_folder(
  project_number = project_get_current_id(),
  account = connect_planner()
)

project_get_folder_full(
  project_number = project_get_current_id(),
  projects_path = read_secret("projects.path"),
  account = connect_planner()
)

project_get_title(
  project_number = project_get_current_id(),
  account = connect_planner()
)

project_get_file(
  filename = ".*",
  project_number = project_get_current_id(),
  fixed = FALSE,
  account = connect_planner()
)

project_set_file(
  filename,
  project_number = project_get_current_id(),
  account = connect_planner()
)

project_set_folder(
  foldername,
  project_number = project_get_current_id(),
  account = connect_planner()
)

project_open_analysis_file(
  project_number = project_get_current_id(ask = TRUE),
  account = connect_planner()
)

project_open_folder(
  project_number = project_get_current_id(ask = TRUE),
  account = connect_planner()
)

project_add_qmd_skeleton(
  filename = NULL,
  project_number = project_get_current_id(),
  account = connect_planner()
)
}
\arguments{
\item{ask}{logical to indicate whether the project number should always be asked. The default, \code{NULL}, will show a popup in \link[=interactive]{interactive} \R sessions, allowing to search for projects. In non-interactive sessions, such as in Quarto and R Markdown, it will use the current \link[=getwd]{working directory} to determine the project number.}

\item{account}{a Microsoft 365 account to use for looking up properties. This has to be an object as returned by \code{\link[=connect_planner]{connect_planner()}} or via \code{\link[AzureGraph:graph_login]{AzureGraph::create_graph_login()}}\verb{$get_group(name)$get_plan(plan_title)}.}

\item{project_number}{Planner project number}

\item{projects_path}{location of the folder that contains all department projects}

\item{filename}{name for the new Quarto file}

\item{fixed}{\link{logical} to turn off regular expressions}

\item{foldername}{foldername to set}
}
\description{
Retrieve project properties, such as the title, folder location and project number.
}
\details{
\code{\link[=project_get_current_id]{project_get_current_id()}} uses \code{\link[=planner_task_search]{planner_task_search()}} to find a specific project based on any search string.

\code{\link[=project_identifier]{project_identifier()}} generates the project identifier for print on reports and in mails: a combination of the currently logged in user (in your case: '\Sexpr{Sys.info()["user"]}'), the current date/time (format: YYMMDDHHMM), and the project number. If the project number is not available, it will only return the current user and date/time (format: YYMMDDHHMM).

\code{\link[=project_set_folder]{project_set_folder()}} will create the folder if it does not exist.

\code{\link[=project_add_qmd_skeleton]{project_add_qmd_skeleton()}} initializes a new Quarto skeleton for a project.
}
\examples{
project_identifier(123)
}
