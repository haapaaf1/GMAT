%$Header: /GMAT/doc/cvs/GMAT_RegSetup/matlab/gmat_keyword/GetGMATObject.m,v 1.2 2007/11/27 22:18:55 edove Exp $
function objct = GetGMATObject(var)

global gmatChannel;
%disp(['Get GMAT::' var]);

if (gmatChannel == 0) 
   disp('channel is not valid');
else
   var1 = [var  '.'];
   data = Request(gmatChannel, var1);
   eval(data);
   objct = eval(var);
end
