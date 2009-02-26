%$Header: /GMAT/doc/cvs/GMAT_RegSetup/matlab/gmat_keyword/Poke.m,v 1.3 2008/07/18 16:22:27 edove Exp $
function Poke(channel, dataType, dataStr)

%-----------------------------------------------------------
% if computer is 'PCWIN' call dde function to send string data
%-----------------------------------------------------------
%if (computer == 'PCWIN')
if ispc
   ddepoke(channel, dataType, dataStr);
else
   disp('Poke(): Only PC windows is supported at this time');
end
