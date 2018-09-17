clear();

clients_n = 10;

%CQI = [3, 6, 11, 15, 20, 25, 39, 50, 63, 72, 80, 93];
maxcqi_cdf = @(x) (x/15)^10;

% TRYING WITH A NON UNIFORM RBSIZES DISTRIBUTION, WITH UNIFORM CQIS
CQI = 1:15;
RBSIZES = [3,3,6,11,15,20,25,36,39,50,63,72,80,93,93];

%calcolo della cdf rbsize
rbsize_cdf = zeros(93, 1);
for i=1:93
    a = 0;
    for j=1:size(RBSIZES,2)
        if RBSIZES(j) <= i
            a = a + 1;
        end
    end   
    a = a/size(RBSIZES,2);
    rbsize_cdf(i,1) = a;
end
%plot(rbsize_cdf)


rbsize_pmf = zeros(93,1);
for i = 1:size(RBSIZES,2)
    j = RBSIZES(i);
    rbsize_pmf(j,1) = rbsize_cdf(j,1) -  rbsize_cdf(j-1,1);
end

rbsize_mean = sum(rbsize_pmf .* (1:93)');

% see proof
% we want the product of n-1 distributions
rbsizemax_cdf = rbsize_cdf.^(clients_n-1);
%plot(rbsizemax_cdf)

rbsizemax_pmf = zeros(93,1);
for i = 1:size(RBSIZES,2)
    j = RBSIZES(i);
    rbsizemax_pmf(j,1) = rbsizemax_cdf(j,1) -  rbsizemax_cdf(j-1,1);
end

rbsizemax_mean = sum(rbsizemax_pmf .* (1:93)');

% we are trying to estimate bestcqi traffic using E[nfill] which we
% extracted from the simulation results
nfill = 1.59;

% product and divisions are necessary to convert the result from B/s to
% Mbit/s
uniform_fair_framesize_mean = ((25*rbsize_mean)*1000*8)/1000000;
uniform_bestcqi_framesize_mean = ((((25 - nfill)*rbsize_mean + nfill*rbsizemax_mean))*1000*8)/1000000;
disp(['Uniform Fair Mean Th at Saturation = ', num2str(uniform_fair_framesize_mean), 'Mbit/s']);
disp('   > this is correct for NoFramingTest, because we assumed that all frames are totally filled by current served user');
disp(['Uniform BestCQI Mean Th at Saturation = ', num2str(uniform_bestcqi_framesize_mean), 'Mbit/s']);
disp('   > this is 200k far from the real result, frame filling is not best modelled in this model')
%%%% BINOMIAL BESTCQI %%%%
nfill = 1.264; % took from simulation results (binomial bestcqi)

cqi_count = 14;
clients_binomial_p = [ 0.13, 0.22, 0.31, 0.40, 0.49, 0.58, 0.67, 0.76, 0.85, 0.94 ];
clients_binom_pmf = zeros(clients_n, cqi_count+1);
clients_composite_pmf = zeros(clients_n, max(RBSIZES));
clients_composite_mean = zeros(clients_n,1);

% compute binomial pmf for each user
for i = 1:clients_n
    clients_binom_pmf(i,:) = binopdf(0:cqi_count, cqi_count, clients_binomial_p(i));
    for j = 0:cqi_count
        %dominio = unique(RBSIZES)
        clients_composite_pmf(i,RBSIZES(j+1)) = clients_composite_pmf(i,RBSIZES(j+1)) + clients_binom_pmf(i,j+1);
    end
    % compute mean rbsize
    clients_composite_mean(i) = sum(clients_composite_pmf(i,:).*(1:93));
end

% compute max(user1cqi, user2cqi..., user10cqi) binomials mean
binomial_maxcqi_cdf = ones(1, max(RBSIZES));

% compute cdf from pdf
clients_composite_cdf = zeros(clients_n, max(RBSIZES));
for k = 1:clients_n
    for i=1:max(RBSIZES)
        a = 0;
        for j=1:max(RBSIZES)
            if j <= i
                a = a + clients_composite_pmf(k,j);
            end
        end
        clients_composite_cdf(k,i) = a;
    end

    % we want the product of n-1 distributions
    if(k ~= clients_n)
        binomial_maxcqi_cdf(1,:) = binomial_maxcqi_cdf(1,:) .*  clients_composite_cdf(k,:);
    end
end

% computed clients_composite_cdf pmf
binomial_maxcqi_pmf = zeros(1,max(RBSIZES));
binomial_maxcqi_pmf(1,1) = binomial_maxcqi_cdf(1,1);
for i = 2:max(RBSIZES)
    binomial_maxcqi_pmf(1,i) = binomial_maxcqi_cdf(1,i) - binomial_maxcqi_cdf(1,i-1);
end

% compute mean of binomial maxcqi
binomial_maxcqi_mean = sum(binomial_maxcqi_pmf(1,:).*(1:93));

% compute binomial BestCQI mean traffic
binomial_bestcqi_throughput_mean = zeros(clients_n,1);
for i=1:clients_n
   binomial_bestcqi_throughput_mean(i) = ((((25 - nfill)*clients_composite_mean(i) + nfill*binomial_maxcqi_mean))*1000*8)/1000000/clients_n; 
end

% compute FairCQI
binomial_fair_throughput_mean = ((((25 - nfill)*mean(clients_composite_mean) + nfill*mean(clients_composite_mean)))*1000*8)/1000000;

disp(['Binomial Fair Mean Th at Saturation = ', num2str(sum(binomial_fair_throughput_mean)), 'Mbit/s']);
disp(['Binomial BestCQI Mean Th at Saturation = ', num2str(sum(binomial_bestcqi_throughput_mean)), 'Mbit/s']);
disp("   > Binomial estimates are 200kb far from the real values, caused by bad modelling of residual frame filling");