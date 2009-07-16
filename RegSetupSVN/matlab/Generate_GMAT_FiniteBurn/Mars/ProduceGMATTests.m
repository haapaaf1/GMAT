% This script will produce the specified GMAT and FreeFlyer scripts for
% finite burn testing.  Change the numbers in the for loops to correspond to
% what tanks, thrusters and spacecrafts (different spacecrafts only 
% available for Earth) are to be used in the scripts.  The numbers must 
% match the number of elements avalable in the spacecraft, thruster and tank 
% functions.  Also make sure that the correct file path for saving the 
% scripts and specifing the outputs is set in the GMATTestScript and 
% FFTestScript functions.

% Created by Jason Tichy


tic,
for Sc = 1:1;                 %ranges from 1 to 1
    for Thrst = 1:7;          %ranges from 1 to 7
        for CSsys = 0:3;      %ranges from 0 to 3
            for Tnk = 1:1;    %ranges from 1 to 22
                GMATTestScript(['Sc',char(64+Sc)],['Thruster',char(64+Thrst)],CSsys,['Tank',char(64+Tnk)]);
                FFTestScript(['Sc',char(64+Sc)],['Thruster',char(64+Thrst)],CSsys,['Tank',char(64+Tnk)]);
            end
        end
    end
end
toc