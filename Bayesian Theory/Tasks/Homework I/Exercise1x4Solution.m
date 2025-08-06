% ============================== #
% Advanced Statistical Modelling #
% Class 3 - Exercise 1.4         #
% Filip Chmielowski              #
% ============================== #

% Clear workspace and figures
clear;
clc;
close all;

% Define data for Anna's prior
mu_anna = 0.2; % Anna's prior mean
sigma_anna = 0.08; % Anna's prior standard deviation

% Calculate alpha and beta for Anna's Beta prior distribution
alpha_anna = mu_anna * ((mu_anna * (1 - mu_anna)) / (sigma_anna^2) - 1);
beta_anna = (1 - mu_anna) * ((mu_anna * (1 - mu_anna)) / (sigma_anna^2) - 1);

% Sam's prior is uniform Beta(1, 1)
alpha_sam = 1;
beta_sam = 1;

% Observed data
n1 = 100;
y1 = 26;

% Posterior for Anna
alpha_anna_post1 = alpha_anna + y1;
beta_anna_post1 = beta_anna + n1 - y1;

% Posterior for Sam
alpha_sam_post1 = alpha_sam + y1;
beta_sam_post1 = beta_sam + n1 - y1;

% Define a range of theta (probability) values
theta = linspace(0, 1, 1000);

% Prior and Posterior distributions for both Anna and Sam
prior_anna = betapdf(theta, alpha_anna, beta_anna);
prior_sam = betapdf(theta, alpha_sam, beta_sam);
posterior_anna1 = betapdf(theta, alpha_anna_post1, beta_anna_post1);
posterior_sam1 = betapdf(theta, alpha_sam_post1, beta_sam_post1);

% Plot the prior distributions
figure;
plot(theta, prior_anna, 'b', 'LineWidth', 2); hold on;
plot(theta, prior_sam, 'g', 'LineWidth', 2);
title('Prior Distributions');
xlabel('\theta (Proportion supporting the casino)');
ylabel('Density');
legend('Anna''s Prior (Beta)', 'Sam''s Prior (Uniform)');
grid on;

% Plot the posterior distributions after n=100, y=26
figure;
plot(theta, posterior_anna1, 'b', 'LineWidth', 2); hold on;
plot(theta, posterior_sam1, 'g', 'LineWidth', 2);
title('Posterior Distributions after observing n=100, y=26');
xlabel('\theta (Proportion supporting the casino)');
ylabel('Density');
legend('Anna''s Posterior', 'Sam''s Posterior');
grid on;

% Now repeat for n=1000, y=260
n2 = 1000;
y2 = 260;

% Posterior for Anna with new data
alpha_anna_post2 = alpha_anna + y2;
beta_anna_post2 = beta_anna + n2 - y2;

% Posterior for Sam with new data
alpha_sam_post2 = alpha_sam + y2;
beta_sam_post2 = beta_sam + n2 - y2;

% Posterior distributions for both Anna and Sam
posterior_anna2 = betapdf(theta, alpha_anna_post2, beta_anna_post2);
posterior_sam2 = betapdf(theta, alpha_sam_post2, beta_sam_post2);

% Plot the posterior distributions for n=1000, y=260
figure;
plot(theta, posterior_anna2, 'b', 'LineWidth', 2); hold on;
plot(theta, posterior_sam2, 'g', 'LineWidth', 2);
title('Posterior Distributions after observing n=1000, y=260');
xlabel('\theta (Proportion supporting the casino)');
ylabel('Density');
legend('Anna''s Posterior (n=1000, y=260)', 'Sam''s Posterior (n=1000, y=260)');
grid on;
