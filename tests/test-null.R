require(shiny)
library(debrowser)
library(testthat)

test_that("passing no data returns NULL", {
    expect_null(getSampleNames(NULL))
    expect_null(compareClust() )
    expect_null(getGOPlots(NULL, NULL))
    null_deseq <- runDESeq(NULL)
    expect_null(null_deseq)
    expect_null(mainScatter(NULL))
    expect_null(plot_pca(NULL))
})
