%$Header: /GMAT/doc/cvs/GMAT_RegSetup/matlab/gmat_keyword/Advise.m,v 1.2 2007/11/27 22:18:55 edove Exp $
function data = Advise(channel, item, var)

%-----------------------------------------------------------
% if computer is 'PCWIN' call dde function to request data
%-----------------------------------------------------------
if (computer == 'PCWIN')
   disp('calling ddeadv');
   ddeadv(channel, item, 'disp(var)', var, [1,1]); % receive as string
   var
else
   disp('Request(): Only PC windows is supported at this time');
end
