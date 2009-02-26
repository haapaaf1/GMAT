%$Header: /GMAT/doc/cvs/GMAT_RegSetup/matlab/gmat_keyword/Request.m,v 1.2 2007/11/27 22:18:55 edove Exp $
function data = Request(channel, var)

%-----------------------------------------------------------
% if computer is 'PCWIN' call dde function to request data
%-----------------------------------------------------------
if strcmp(computer,'PCWIN')
   %data = ddereq(gmatChannel, var); % receive as numeric
   data = ddereq(channel, var, [1 1]); % receive as string, default timeout of 3 sec
elseif isunix 
   data = SendGMAT('Request',var);
else
   disp('Request(): unknown platform is not supported at this time');
end
