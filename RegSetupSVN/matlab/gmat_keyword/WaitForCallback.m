%$Header: /GMAT/doc/cvs/GMAT_RegSetup/matlab/gmat_keyword/WaitForCallback.m,v 1.1 2007/11/27 22:18:55 edove Exp $
%------------------------------------------------------------------------------
%                               WaitForCallback.m
%------------------------------------------------------------------------------
% GMAT: Goddard Mission Analysis Tool
%
% ** Legal **
%
% Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
% number S-67573-G
%
% Author: Wendy Shoan
% Created: 2006.08.24
%
% Matlab function to wait for a callback to GMAT to complete.
%
%------------------------------------------------------------------------------
function WaitForCallback()

global gmatChannel;
global gmatCallbackStatus;
if (~isunix && gmatChannel == 0) 
   disp('channel is not valid');
else
    var = 'CallbackStatus';
%   if isunix
%      pause(5);
%   else
%      pause(1);
%   end

   continueWait = true;
   
   while(continueWait)
      
      gmatCallbackStatus = Request(gmatChannel, var)
      
      if (isfloat(gmatCallbackStatus))
         %% Skip Close/Open to stay with the same channel
         %%CloseGMAT;
         %%OpenGMAT;
         continueWait = true;
      elseif(ischar(gmatCallbackStatus))
         if (strcmp(gmatCallbackStatus,'Completed'))
            continueWait = false;
         else
            continueWait = true;
         end
      end
      
%      if isunix
%         pause(5);
%      else
%         pause(1);
%      end 

   end
end
