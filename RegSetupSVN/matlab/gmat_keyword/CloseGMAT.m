%$Header: /GMAT/doc/cvs/GMAT_RegSetup/matlab/gmat_keyword/CloseGMAT.m,v 1.3 2007/12/14 22:38:08 edove Exp $
function CloseGMAT()

global gmatChannel;
%-----------------------------------------------------------
% if computer is 'PCWIN' call dde function to terminate
% conversation with GMAT
%-----------------------------------------------------------
if strcmp(computer, 'PCWIN')
   ddeterm(gmatChannel);
elseif isunix
   CallGMAT('Close','');
else 
   disp('CloseGMAT(): Unknown platform is not supported at this time');
end

gmatChannel = 0;
%disp('Close;')
