clc
% DHRDEMO  Captain Toolbox demonstration
 
% This script analyses the air passenger time series using
% the functions for Dynamic Harmonic Regression (DHR).
 
%MAKES SURE TO LOAD IN SALES DATA
% Load in sales data as 48x1 double 
%Select inport data. Select sales.dat then change output type to numeric matrix 
  
y=fcast(sales, [36 48]);
clf; plot([sales y])
%% 

% We first call DHROPT to optimise the Noise Variance
% Ratio (NVR) hyper-parameters.
 
% We will use a trend component and 5 harmonics. %change to 4??
 
P=[0 12./(1:5)]
 
% All of the parameters will be modelled with
% an Integrated Random Walk (IRW).
 
TVP=1;
 
% The order of the AR spectrum is specified below.
 
nar=16;

% ESTIMATING HYPER-PARAMETERS : PLEASE WAIT
 
nvr=dhropt(y, P, TVP, nar)
%% 

% The plot compares the estimated and fitted spectra.

% The DHR function utilises the optimsed NVR values.
 
[fit, fitse, trend, trendse, comp]=dhr(y, P, TVP, nvr);
 
% The trend identifies the business cycle.
 
clf
plot([trend(:, 1) sales])
set(gca, 'xlim', [0 length(sales)])
title('Data and trend')

%% 

% The seasonal component increases over time.
 
clf
plot(sum(comp'))
set(gca, 'xlim', [0 length(sales)])
title('Total seasonal component')

% The functions successfully interpolate over the 10 months
% of missing data starting at sample 85. Similarly, they predict
% the output for the final year, i.e. sample 132 until the end
% of the series. Note that since missing data were introduced at
% the start of the analysis, the latter is a 'true' forecast.
%% 
 
clf
plot(fit, 'b')
hold on
plot(sales, 'r')
plot([fit-sales zeros(48, 1)], 'b')
plot([36, 36], [-50000 50000], 'r')  % forecasting horizon
set(gca, 'xlim', [0 length(sales)])
title('Data, fit and residuals')

%% 

% A zoomed in view of the final two years are shown, with
% the standard errors and forecasting horizon also indicated.
 
clf
plot(fit, 'b')
hold on
plot(sales, 'ro')
plot([fit+2*fitse fit-2*fitse], ':b')
plot([130, 130], [200 700], 'r')  % forecasting horizon
axis([119 length(fit) 250 700])
title('Data (o), fit and standard errors')
 
 

