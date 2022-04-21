# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

project_dir <- paste0(tempdir(), "/projects/")
my_secrets_file <- tempfile(fileext = ".yaml")
Sys.setenv(secrets_file = my_secrets_file)
writeLines(c(paste0("projects.path: '", project_dir, "'"),
             "teams.webhooks.file: ''"),
           my_secrets_file)

test_that("project properties work", {
  
  read_secret("projects.path")

  dir.create(path = project_dir)
  dir.create(path = paste0(project_dir, "Project Something - p123/"))
  
  expect_true(file.create(paste0(project_dir, "Project Something - p123/Analyse.R")))
  
  expect_identical(project_get_folder_full(123),
                   gsub("\\", "/", 
                        paste0(normalizePath(paste0(project_dir, "Project Something - p123")), "/"),
                        fixed = TRUE))
  expect_identical(project_get_folder(123), "Project Something - p123")
  expect_warning(project_get_folder(456))
  expect_identical(suppressWarnings(project_get_folder(456)), NA_character_)
  expect_identical(project_get_file("Analyse.R", 123),
                   gsub("\\", "/", 
                        normalizePath(paste0(project_dir, "Project Something - p123/Analyse.R")),
                        fixed = TRUE))
  expect_identical(project_get_file(".*lys.*", 123),
                   gsub("\\", "/", 
                        normalizePath(paste0(project_dir, "Project Something - p123/Analyse.R")),
                        fixed = TRUE))
  expect_warning(project_get_file("does not exist", 123))
  expect_identical(suppressWarnings(project_get_file("does not exist", 123)), NA_character_)
  expect_identical(project_set_file("test.csv", 123),
                   gsub("\\", "/",
                        paste0(normalizePath(paste0(project_dir, "Project Something - p123")), "/test.csv"),
                        fixed = TRUE))
  
  expect_identical(project_set_folder("testdir", 123),
                   gsub("\\", "/",
                        paste0(normalizePath(paste0(project_dir, "Project Something - p123/testdir")), "/"),
                        fixed = TRUE))
})

# test_that("teams works", {
#   expect_warning(teams("test"))
#   expect_identical(suppressWarnings(teams("test")), "test")
#   expect_warning(teams("test", title = "Title", subtitle = "Subtitle"))
#   expect_identical(class(suppressWarnings(teams("test", title = "Title", subtitle = "Subtitle"))), "list")
#   expect_error(suppressWarnings(teams("test", title = "Title", subtitle = "Subtitle", images = "hello")))
#   expect_identical(class(question_mark_image()), "character")
#   expect_warning(teams("test", title = "Title", subtitle = "Subtitle",
#                        items = c(test = "abc",
#                                  "bcd"),
#                        buttons = c("Send mail" = "mailto:test@test.com",
#                                    "Open link" = "https://duckduckgo.com")))
#   expect_error(teams_webhooks())
# })
