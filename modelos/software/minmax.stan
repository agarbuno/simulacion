data {
  real xmin;
  real xmax;
  int N;
}
parameters {
  real mu;
  real log_sigma; 
}
transformed parameters {
  real sigma = exp(log_sigma);
}
model {
  target += normal_lpdf(xmin | mu, sigma); 
  target += normal_lpdf(xmax | mu, sigma);
  target += (N-2) * log(normal_cdf(xmax | mu, sigma) - normal_cdf(xmin | mu, sigma));
}
