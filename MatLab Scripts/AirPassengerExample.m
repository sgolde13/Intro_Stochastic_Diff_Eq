clc
% DHRDEMO  Captain Toolbox demonstration
 
% This script analyses the air passenger time series using
% the functions for Dynamic Harmonic Regression (DHR).
 
load air.dat;  % thousands of passengers per month (1949-1960)
 
% Missing values are added in order to test the ability of the
% algorithms to automatically handle interpolation and
% forecasting. Here, fcast is employed to replace samples
% 84-94 (interpolation) and 131-144 (forecasting) with
% Not-a-Number variables.
 
y=fcast(air, [84 94; 131 144]);
 
clf; plot([air y])

% We first call DHROPT to optimise the Noise Variance
% Ratio (NVR) hyper-parameters.
 
% We will use a trend component and 5 harmonics.
 
P=[0 12./(1:5)]
 
% All of the parameters will be modelled with
% an Integrated Random Walk (IRW).
 
TVP=1;
 
% The order of the AR spectrum is specified below.
 
nar=16;

% ESTIMATING HYPER-PARAMETERS : PLEASE WAIT
 
nvr=dhropt(y, P, TVP, nar)

% The plot compares the estimated and fitted spectra.

% The DHR function utilises the optimsed NVR values.
 
[fit, fitse, trend, trendse, comp]=dhr(y, P, TVP, nvr);
 
% The trend identifies the business cycle.
 
clf
plot([trend(:, 1) air])
set(gca, 'xlim', [0 length(air)])
title('Data and trend')

% The seasonal component increases over time.
 
clf
plot(sum(comp'))
set(gca, 'xlim', [0 length(air)])
title('Total seasonal component')

% The functions successfully interpolate over the 10 months
% of missing data starting at sample 85. Similarly, they predict
% the output for the final year, i.e. sample 132 until the end
% of the series. Note that since missing data were introduced at
% the start of the analysis, the latter is a 'true' forecast.
 
clf
plot(fit, 'b')
hold on
plot(air, 'r')
plot([fit-air zeros(144, 1)], 'b')
plot([84, 84], [-50 50], 'r')  % start of interpolation
plot([94, 94], [-50 50], 'r')  % end of interpolation
plot([130, 130], [-50 50], 'r')  % forecasting horizon
set(gca, 'xlim', [0 length(air)])
title('Data, fit and residuals')

% A zoomed in view of the final two years are shown, with
% the standard errors and forecasting horizon also indicated.
 
clf
plot(fit, 'b')
hold on
plot(air, 'ro')
plot([fit+2*fitse fit-2*fitse], ':b')
plot([130, 130], [200 700], 'r')  % forecasting horizon
axis([119 length(fit) 250 700])
title('Data (o), fit and standard errors')
 
 

