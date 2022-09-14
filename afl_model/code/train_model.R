#### Train model

train_model <- function(filepath, input) {
  stan(
    file = filepath,
    cores = parallel::detectCores(),
    data = input
  )
}
