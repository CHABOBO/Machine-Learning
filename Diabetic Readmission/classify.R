classify <- function(x) {
        value = -1
        if (startsWith(x, "E"))
        {
                value = 19
        }
        else if (startsWith(x, "V"))
        {
                value = 20
        }
        else
        {
                x = as.numeric(x)
                if (x < 140) {
                        value = 1
                }
                else if (x >= 140 && x < 240) {
                        value = 2
                }
                else if (x >= 240 && x < 280) {
                        value = 3
                }
                else if (x >= 280 && x < 290) {
                        value = 4
                }
                else if (x >= 290 && x < 320) {
                        value = 5
                }
                else if (x >= 320 && x < 360) {
                        value = 6
                }
                else if (x >= 360 && x < 390) {
                        value = 7
                }
                else if (x >= 390 && x < 460) {
                        value = 8
                }
                else if (x >= 460 && x < 520) {
                        value = 9
                }
                else if (x >= 520 && x < 580) {
                        value = 10
                }
                else if (x >= 580 && x < 630) {
                        value = 11
                }
                else if (x >= 630 && x < 680) {
                        value = 12
                }
                else if (x >= 680 && x < 710) {
                        value = 13
                }
                else if (x >= 710 && x < 740) {
                        value = 14
                }
                else if (x >= 740 && x < 760) {
                        value = 15
                }
                else if (x >= 760 && x < 780) {
                        value = 16
                }
                else if (x >= 780 && x < 800) {
                        value = 17
                }
                else if (x >= 800 && x < 1000) {
                        value = 18
                }
        }
        value
}

classify1 <- function(x) {
        value = ""
        if (is.na(x))
        {
                value = x
        }
        else if (grepl(x = x, pattern = "discharge+", ignore.case = T))
        {
                #print(x)
                value = 1
        }
        else if (grepl(x = x, pattern = "expired+", ignore.case = T))
        {
                value = 2
        }
        else if (grepl(x = x, pattern = "not mapped+", ignore.case = T))
        {
                value = 3
        }
        else if (grepl(x = x, pattern = "left ama+", ignore.case = T))
        {
                value = 4
        }
        else if (grepl(x = x, pattern = "hospice+", ignore.case = T))
        {
                value = 5
        }
        else if (grepl(x = x, pattern = "admitted+", ignore.case = T))
        {
                value = 6
        }
        value
}

classify2 <- function(x) {
        value = ""
        if (is.na(x))
        {
                value = x
        }
        else if (grepl(x = x, pattern = "referral+", ignore.case = T))
        {
                value = 1
        }
        else if (grepl(x = x, pattern = "transfer+", ignore.case = T))
        {
                value = 2
        }
        else if (grepl(x = x, pattern = "emergency+", ignore.case = T))
        {
                value = 3
        }
        else if (grepl(x = x, pattern = "not mapped+", ignore.case = T))
        {
                value = 4
        }
        else if (grepl(x = x, pattern = "enforcement+", ignore.case = T))
        {
                value = 5
        }
        value
}

classify3 <- function(x) {
        value = ""
        if (is.na(x))
        {
                value = x
        }
        else if (grepl(x = x, pattern = "caucasian+", ignore.case = T))
        {
                value = 1
        }
        else if (grepl(x = x, pattern = "africanamerican+", ignore.case = T))
        {
                value = 2
        }
        else if (grepl(x = x, pattern = "hispanic+", ignore.case = T))
        {
                value = 3
        }
        else if (grepl(x = x, pattern = "other+", ignore.case = T))
        {
                value = 4
        }
        else if (grepl(x = x, pattern = "asian+", ignore.case = T))
        {
                value = 5
        }
        value
}