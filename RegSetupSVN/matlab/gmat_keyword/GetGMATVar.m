%$Header: /GMAT/doc/cvs/GMAT_RegSetup/matlab/gmat_keyword/GetGMATVar.m,v 1.5 2008/07/18 16:22:27 edove Exp $
function data = GetGMATVar(var)

%--------------------------------------
% call dde function to request data
%--------------------------------------
global gmatChannel;
disp(['Get GMAT::' var]);

if (gmatChannel == 0) 
   disp('channel is not valid');
else
   tempdata = Request(gmatChannel, var);
   try % Extract numeric data
       data = eval(tempdata); 
   catch % Extract string data
       tempdata2 = tempdata(2:size(tempdata,2)-1); % Strip brackets from tempdata
       data = tempdata2;
   end
end
