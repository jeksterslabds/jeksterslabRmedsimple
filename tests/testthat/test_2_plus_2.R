#' ---
#' title: "Test: test_2_plus_2"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: test_2_plus_2}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ include=FALSE, cache=FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
#'
#+ setup
library(testthat)
context("Test test_2_plus_2.")
#'
#+ testthat, echo=TRUE
test_that("2 + 2 = 4", {
  expect_equivalent(
    2 + 2,
    4
  )
})
