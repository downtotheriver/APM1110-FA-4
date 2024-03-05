APM1110 - FA 4 - Dacanay
================
Jordan Dacanay

### 5) A geospatial analysis system has four sensors supplying images. The percentage of images supplied by each sensor and the percentage of images relevant to a query are shown in the following table.

``` r
geospatial_data <- read.csv("Geospatial Analysis System.csv")
attach(geospatial_data)
print(geospatial_data)
```

    ##   Sensor Percentage.of.Images.Supplied Percentage.of.Relevant.Images
    ## 1      1                            15                            50
    ## 2      2                            20                            60
    ## 3      3                            25                            80
    ## 4      4                            40                            85

### What is the overall percentage of relevant images?

#### Let us first assign the values of each sensor’s percentage of image supplied and percentage of relevant images to their respective variables.

``` r
# Percentage of Image Supplied
pis_1 <- Percentage.of.Images.Supplied[1]/100
pis_2 <- Percentage.of.Images.Supplied[2]/100
pis_3 <- Percentage.of.Images.Supplied[3]/100
pis_4 <- Percentage.of.Images.Supplied[4]/100

# Percentage of Relevant Images
pri_1 <- Percentage.of.Relevant.Images[1]/100
pri_2 <- Percentage.of.Relevant.Images[2]/100
pri_3 <- Percentage.of.Relevant.Images[3]/100
pri_4 <- Percentage.of.Relevant.Images[4]/100

# Calculating the overall percentage of relevant images
opri <- ((pis_1 * pri_1) + (pis_2 * pri_2) + (pis_3 * pri_3) + (pis_4 * pri_4)) * 100
```

#### ANSWER

``` r
print(paste0("The overall percentage of relevant images is ", opri, "%."))
```

    ## [1] "The overall percentage of relevant images is 73.5%."

### 6) A fair coin is tossed twice.

### Let E1 be the event that both tosses have the same outcome, that is, E1 = (HH, TT).

### Let E2 be the event that the first toss is a head, that is, E2 = (HH, HT).

### Let E3 be the event that the second toss is a head, that is, E3 = (TH, HH).

### Show that E1, E2, and E3 are pairwise independent but not mutually independent

#### WTS: E1, E2, and E3 are pairwise independent

``` r
# Probability of each event
p_E1 <- 2/4
p_E2 <- 2/4
p_E3 <- 2/4
v_p_e <- c(p_E1, p_E2, p_E3)

# Probability of each pair of event
p_E1_and_E2 <- 1/4
p_E2_and_E3 <- 1/4
p_E1_and_E3 <- 1/4
v_p_p <- c(p_E1_and_E2, p_E1_and_E3, p_E2_and_E3)

# Check whether the product of the individual probabilities equals the probability of their intersections.
true_counter = 0
pair_counter = 1
for (i in seq(1, 2)) {
  for (j in seq(1, 2)) {
    if (v_p_e[i] * v_p_e[i+j] == v_p_p[pair_counter]) {
      true_counter <- true_counter + 1
    }
    if (i == 2 & j == 1) {
      break
    }
    pair_counter <- pair_counter + 1
  }
}

if (true_counter == 3) {
  print("Events E1, E2, and E3 are pairwise independent since every pair of events taken from the set is independent.")
  pairwise_checker = TRUE
} else {
  print("Events E1, E2, and E3 are not pairwise independent since one or more pair of events taken from the set is/are not independent.")
  pairwise_checker = FALSE
}
```

    ## [1] "Events E1, E2, and E3 are pairwise independent since every pair of events taken from the set is independent."

#### WTS: E1, E2, and E3 are not mutually independent

``` r
# Probability of all events
p_E1_and_E2_and_E3 <- 1/4

if (p_E1_and_E2_and_E3 == prod(v_p_e)) {
  print("Events E1, E2, and E3 are mutually independent since the probability of the intersection of all three events is equal to the product of the probabilities of the individual events.")
  mutual_checker = TRUE
} else {
  print("Events E1, E2, and E3 are not mutually independent since the probability of the intersection of all three events is not equal to the product of the probabilities of the individual events.")
  mutual_checker = FALSE
}
```

    ## [1] "Events E1, E2, and E3 are not mutually independent since the probability of the intersection of all three events is not equal to the product of the probabilities of the individual events."

#### ANSWER

``` r
if (pairwise_checker == TRUE & mutual_checker == TRUE) {
  print("Events E1, E2, and E3 are both pairwise independent and mutually independent.")
} else if (pairwise_checker == TRUE & mutual_checker == FALSE) {
  print("Events E1, E2, and E3 are pairwise independent but not mutually independent.")
}
```

    ## [1] "Events E1, E2, and E3 are pairwise independent but not mutually independent."
