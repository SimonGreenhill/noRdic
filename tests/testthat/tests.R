library(covr)
library(testthat)

# AREA,GENUSE,SPUSE
# NORN,IC,IC_cosmetics
# SWE,IC,IC_dyes
# DEN,IC,IC_tanning
# FOR,M,M_blood
# NORN,M,M_blood
# NORS,M,M_blood
# DEN,M,M_blood
# FOR,M,M_blood
# NORN,M,M_blood
testdata <- read.nordic('testdata.csv')


test_that("Test read.nordic", {
    expect_equal(colnames(testdata), c("AREA", "TYPE", "USE"))

    expect_equal(
        sort(unique(testdata[testdata$USE == 'M', 'AREA'])),
        c("DEN", "FOR", "NORN", "NORS")
    )

    expect_equal(
        sort(unique(testdata[testdata$USE == 'IC', 'AREA'])),
        c("DEN", "NORN", "SWE")
    )

    expect_equal(
        sort(unique(testdata[testdata$USE == 'M_blood', 'AREA'])),
        c("DEN", "FOR", "NORN", "NORS")
    )

    expect_error(read.nordic('baddata.csv'), "File baddata.csv does not")
})


test_that("Test to.rayDISC", {
    d <- to.rayDISC(testdata[testdata$USE == 'IC_dyes', ], c("DEN", "FOR"))
    expect_equal(nrow(d), 3)
    expect_equal(d[d$AREA == 'SWE', 'STATE'], '1')
    expect_equal(d[d$AREA == 'DEN', 'STATE'], '0')
    expect_equal(d[d$AREA == 'FOR', 'STATE'], '0')

    d <- to.rayDISC(testdata[testdata$USE == 'M', ], c("ELF"))
    # note that the duplicate FOR and NORN rows are removed
    expect_equal(nrow(d), 5)
    expect_equal(d[d$AREA == 'DEN', 'STATE'], '1')
    expect_equal(d[d$AREA == 'FOR', 'STATE'], '1')
    expect_equal(d[d$AREA == 'NORN', 'STATE'], '1')
    expect_equal(d[d$AREA == 'NORS', 'STATE'], '1')
    expect_equal(d[d$AREA == 'ELF', 'STATE'], '0')

    # multiple changes
    d <- to.rayDISC(testdata[testdata$USE == 'IC_dyes', ], c("DEN"))
    d <- to.rayDISC(d, c("ELF"), '2')
    d <- to.rayDISC(d, c("NORN"), '3')
    expect_equal(nrow(d), 4)
    expect_equal(d[d$AREA == 'SWE', 'STATE'], '1')
    expect_equal(d[d$AREA == 'DEN', 'STATE'], '0')
    expect_equal(d[d$AREA == 'ELF', 'STATE'], '2')
    expect_equal(d[d$AREA == 'NORN', 'STATE'], '3')
})
