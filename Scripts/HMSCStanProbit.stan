//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data{
int<lower=1> ny;
int<lower=1> ns;
int<lower=1> nc;
int<lower=1> nt;
int<lower=1> nf;
int<lower=0,upper=1> Y[ny,ns];
matrix[ny,nc] X;
matrix[ns,nt] T;
corr_matrix[ns] C;
real<lower=nc> f0;
cov_matrix[nc] V0;
cov_matrix[nc*nt] U0;
vector<lower=0>[ns] aS;
vector<lower=0>[ns] bS;
real<lower=0> nu;
real<lower=0> a1;
real<lower=0> a2;
real<lower=0> b1;
real<lower=0> b2;
}

transformed data{
matrix[nc*nt,nc*nt] LU0 = cholesky_decompose(U0);
}

parameters{
matrix[nc,ns] Beta;
matrix[nc,nt] Gamma;
cov_matrix[nc] V;
matrix[ny,nf] Eta;
matrix[nf,ns] Lambda0;
vector<lower=0>[nf] delta;
real<lower=0,upper=1> rho;
}

transformed parameters{
vector[nf] tau = exp(cumulative_sum(log(delta)));
matrix[nf,ns] Lambda = Lambda0 ./ rep_matrix(sqrt(tau),ns);
}

model{
matrix[ny,ns] L = X*Beta + Eta*Lambda;
matrix[ns,ns] Q = rho*C + diag_matrix(rep_vector(1-rho,ns));
matrix[nc,ns] Mu = Gamma * T';

// observation model
for(j in 1:ns){
Y[,j] ~ bernoulli(Phi(L[,j]));
}

// priors
// to_vector(Beta) ~ multi_normal(to_vector(Mu), kronecker(Q,V));
{
real logPriorBeta;
matrix[nc,ns] A = Beta - Mu;
matrix[ns,ns] tmp = (Q\A') * (V\A);
logPriorBeta = -0.5*trace(tmp) - 0.5*nc*log_determinant(Q) - 0.5*ns*log_determinant(V);
target += logPriorBeta;
}

to_vector(Gamma) ~ multi_normal_cholesky(rep_vector(0,nc*nt), LU0);
V ~ inv_wishart(f0, V0);
for(h in 1:nf){
Eta[,h] ~ std_normal();
Lambda0[h,] ~ student_t(nu,0,1);
}
delta[1] ~ gamma(a1,b1);
delta[2:nf] ~ gamma(a2,b2);
}




