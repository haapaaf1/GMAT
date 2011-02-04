function [ci,ce,Ji,Je,iGfun,jGvar] = CON_TP220(x,varargin)

%   Copyright 2002-2005, United States Government as represented by the 
%   Administrator of The National Aeronautics and Space Administration.
%   All Rights Reserved.
%   This software is licensed under the NASA Open Source Agreement.
%
%   Modification History
%   -----------------------------------------------------------------------
%   Sept-08-08  S. Hughes:  Created the initial version.

ci = [];
Ji = [];
 
ce = (x(1) - 1)^3 - x(2);
Je = [3*(x(1) - 1)^2 -1]';

iGfun = [1 1]';
jGvar = [1 2]';