function [MJDTimes] = GMAT_EphemGenGreg2MJD(GregTimes)
% This function uses GMAT to convert Gregorian times into Modified Julian
% times. 
% *** WARNING: You will see a performance hit using this approach since
% a new mission in GMAT gets called several times (The size of the
% GregTimes array). ***

format long g
for loop = 1:max(size(GregTimes))
    ClearGMAT

    Create Spacecraft Sat;
    GMAT Sat.DateFormat = 'UTCGregorian';
    eval(['GMAT Sat.Epoch = ',GregTimes{loop},';']);

    Create Variable temp;

    % Force Start of Mission Sequence
    BeginScript;
    EndScript;

    GMAT temp = Sat.UTCModJulian; % This line places the Sat.UTCModJulian as part of the workspace so the GetGMATVar command can get the latest value

    BuildRunGMAT
    MJDTimes{loop} = GetGMATVar('Sat.UTCModJulian');
end