################################################################################
## Simulating data for homework 1                                             ##
################################################################################

output <- data.frame("id" = sample(x = seq(from =  1, to = 8 * 25),
                                   size =  8 * 25,
                                   replace = FALSE),
                     "memory" = rep(x = NA, times = 8 * 25),
                     "condition" = rep(x = c("recognition", "free"), 
                                       each = 4 * 25),
                     "stimulus" = rep(x = c("low", "high"), times = 4 * 25),
                     "sex" = c("female", "male")[
                       rbinom(n = 4 * 25, size = 1, prob = 0.4) + 1]) |> 
  dplyr::arrange(id)

set.seed(284620273)
# large
interaction_cs <- as.numeric(output$condition == "recognition" &
                    output$stimulus == "low")

output$memory <- rep(x = 2, times = dim(output)[1]) -
  0.2 * c(output$sex == "male") -
  0.6 * c(output$condition == "recognition") - 
  0.9 * c(output$stimulus == "low") + 
  2.4 * c(interaction_cs) + 
  rnorm(n = dim(output)[1], mean = 0, sd = 0.9)

output$memory <- round(x = output$memory, digits = 2)

readr::write_csv(x = output, file = "hoeworks/memory_frequency.csv")
