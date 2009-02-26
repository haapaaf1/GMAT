function [TesterInitials, MoveFiles, Move_AllOneBuild, Move_AllBuild, ...
    Move_SystemTestBuild, Move_AcceptTestBuild, Move_ValidationBuild,...
    BBorBG, Build1, Build2, What2Compare, Scripts2Compare,...
    Test2Compare] = RegSys_useSetup(compareDir)
% $Id: RegSys_useSetup.m,v 1.4 2007/08/03 20:52:09 edove Exp $

disp(' ');
disp('Do you want to use the input file (regressInput.txt) for the setup of this program? (Y or N)');

% User inputs value. Program warns the user of any incorrectly entered value
error = 1;
useSetup = input('Choice: ','s');
disp(' ');
while error == 1
    if strcmp(lower(useSetup),'y')
        useSetupFlag = 1;
        error = 0;
    elseif strcmp(lower(useSetup),'n')
        useSetupFlag = 0;
        error = 0;
    else
        disp('You have entered an incorrect value. Please try again.');
        useSetup = input('Choice: ','s');
        disp(' ');
    end
end

if strcmp(lower(useSetup),'y');
    % Read in the setup file and store as a nx1 string cell array
    fid = fopen([compareDir,'/regressInput.txt']);
    SetupData = textscan(fid,'%s','delimiter', '=;','commentStyle','%');
    fclose(fid);
    
    % Check to see if values in setup files are numbers or strings
    % then intialize variable accordingly

    for loop = 1:2:(size(SetupData{1,1},1)-1);
        if mean(isstrprop(char(SetupData{1,1}(loop+1)), 'digit')) == 1;
            eval([char(SetupData{1,1}(loop)),'= ',char(SetupData{1,1}(loop+1)),';']);
        else;
            eval([char(SetupData{1,1}(loop)),'= ''',char(SetupData{1,1}(loop+1)),''';']);
        end;
    end;
else;
    % GMAT Regression Testin setup from command window NOT FUNCTIONAL YET
    disp(' ');
    disp('GMAT Regression setup from command window NOT FUNCTIONAL YET');
    
    TesterInitials = {};
    MoveFiles = {};
    Move_AllOneBuild = {};
    Move_AllBuild = {};
    Move_SystemTestBuild = {};
    Move_AcceptTestBuild = {};
    Move_ValidationBuild = {};
    BBorBG = {};
    Build1 = {};
    Build2 = {};
    What2Compare = {};
    Scripts2Compare = {};
    Test2Compare = {};
    return
end;