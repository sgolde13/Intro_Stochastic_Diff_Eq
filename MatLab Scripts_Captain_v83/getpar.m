% GETPAR  Returns transfer function polynomials from theta matrix
%
% [a,b,c,P,d]=getpar(th)
%
% th: theta matrix (*)
%
% a: Denominator polynomial
% b: Numerator polynomial
% c: Noise polynomial
% P: Covariance matix
% d: Delays (continuous time only)
%
% Example: th=riv([y u], [2 1 3 0]); [a, b]=getpar(th)
%   estimate a transfer function model using the RIV function
%   then extract the denominator and numerator polynomials
%
% See also RIV, RIVID, RIVC, RIVCID, MAR, PREPZ

% Copyright 2017 by Lancaster University, United Kingdom
% Diego Pedregal, James Taylor, Wlodek Tych, Peter Young

% Returns the denominator a, numerator b and noise c 
% polynomials, together with the noise covariance P matrix and (in 
% the continuous time case) number of delays d. These variables 
% are all extracted from a previously estimated theta-matrix (see help 
% theta). Each row of b represents the numerator polynomial for 
% each input variable.