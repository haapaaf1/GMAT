% RegressionSystem Compares the report files of various GMAT test using file differencing.
%
% The GMAT regression testing system is designed to automatically test the 
% Acceptance, System, and Validation tests using a file difference 
% comparison method between builds or a build and the good data. 
%
% This comparison tool is a small part of several system. The folder 
% structure that is CVS controlled in the GMAT_RegSetup repository. The
% entire repository must be downloaded in order to ensure no problems when
% running this script. Output reports do not need to be CVS controlled due
% to the possibilty of computer setups changing report files slightly.
%
% FILE FOLDERS USED
%  [Main GMAT_RegSetup Directory]/GMAT_scripts
%  [Main GMAT_RegSetup Directory]/input/
%  [Main GMAT_RegSetup Directory]/output/
%  [Main GMAT_RegSetup Directory]/matlab/comparisons
%
% INPUT
% - The user will have have the choice of using a setup file or using the 
%   matlab command window for intial setup
% - The user will have the option of building and running the scripts in 
%   the input folder themselves or having the program do it for them.
% - The user will get to approve files as good after completing the 
%   regression test (Only one file can be approved at a time). Approvals 
%   will be logged into the selected regression test summary.
%
% OUTPUT
% - Comparison Summary
% - Report files in a Build related folder
%
% IMPORTANT NOTES
% DO NOT CVS control any files in your local 
% [Main GMAT_RegSetup Directory]/output/ directory and subdirectories,
% excluding the Good_reports folders
%
% REVISION HISTORY
% $Id: RegressionSystem.m,v 1.11 2008/04/02 21:00:40 edove Exp $
%
%   Modification History
%   ---------------------------------------------------------------------------------
%   ??/??/???? - E.Dove:  Created the first version.
%   04/21/2009 - E.Dove:  Enabled build to build comparison functionality and moved comparison
%       code to seperate function.


clc
clear all
close all

% Save main and matlab script folder to variables for later us
% (It does't matter if the user changes the directory to this script's folder)

tempDir = mfilename('fullpath');
if ispc;
    temp = findstr(tempDir,'\');
    compareDir = tempDir(1:temp(size(temp,2))-1);
else
    temp = findstr(tempDir,'/');
    compareDir = tempDir(1:temp(size(temp,2))-1);
end;
cd(compareDir);
cd('..');
cd('..');
mainDir = cd;

addpath(genpath([mainDir,'/matlab']));
cd(compareDir);

disp('Welcome to the GMAT Regression Testing program. ');
disp('   - If testing the latest build of GMAT, make sure to have all the relevant ');
disp('   System, Acceptance, and Range tests built and run using the GMAT Regression Testing');
disp('   system setup before continuing further.');
disp(['   - Clear reports from the ',mainDir,'/output/[insert Test folder]/ folders,  ']);
disp('   if you do not know what build they are related to.');
disp(' ');
disp('Press ENTER to continue.');
pause;

[TesterInitials, MoveFiles, Move_AllOneBuild, Move_AllBuild, ...
    Move_SystemTestBuild, Move_AcceptTestBuild, Move_ValidationBuild,...
    BBorBG, Build1, Build2, What2Compare, Scripts2Compare,...
    Test2Compare] = RegSys_useSetup(compareDir); % *** Function Call ***

if isempty(Build1);
    return;
end;

RegSys_parseValidation(Move_AllBuild,mainDir); % *** Function Call ***
RegSys_parseValidation(Build1,mainDir); % *** Function Call ***

if strcmpi(MoveFiles,'y');
    RegSys_moveFiles(Build1, Build2, mainDir, Move_AllOneBuild, Move_AllBuild,...
        Move_SystemTestBuild, Move_AcceptTestBuild, Move_ValidationBuild); % *** Function Call ***
end;

if strcmpi(BBorBG,'bg');
    Folder1 = 'Good_reports';
    Folder2 = [num2str(Build1),'GMAT_reports'];
    Build2  = [];
    [goodFilesAll,build1FilesAll] = RegSys_bgStoreFiles(Build2, Build1, Folder1, Folder2, mainDir); % *** Function Call ***
else
    Folder1 = [num2str(Build1),'GMAT_reports'];
    Folder2 = [num2str(Build2),'GMAT_reports'];
    [build1FilesAll,build2FilesAll] = RegSys_bgStoreFiles(Build1, Build2, Folder1, Folder2, mainDir); % *** Function Call ***
end;

% Initialize variables for summary file saving
currDate = datevec(date);
if size(num2str(currDate(2)),2) == 1;
    MM = ['0',num2str(currDate(2))];
else
    MM = num2str(currDate(2));
end;
if size(num2str(currDate(3)),2) == 1;
    DD = ['0',num2str(currDate(3))];
else
    DD = num2str(currDate(3));
end;
check4file = exist([mainDir,'/output/Comparisons/RegTestSummary_',num2str(currDate(1)),MM,DD,'_1_',TesterInitials,'.regrpt']);
regSummaryPath = [mainDir,'/output/Comparisons/'];
if check4file == 0;
    regSummaryFile = ['RegTestSummary_',num2str(currDate(1)),MM,DD,'_1_',TesterInitials,'.regrpt'];
else
    numRegRpts = size(dir([mainDir,'/output/Comparisons/RegTestSummary_',num2str(currDate(1)),MM,DD,'*.regrpt']),1);
    regSummaryFile = ['RegTestSummary_',num2str(currDate(1)),MM,DD,'_',num2str(numRegRpts+1),'_',TesterInitials,'.regrpt'];
end;

% Save header data to filename
fid = fopen([regSummaryPath,regSummaryFile], 'w');
fprintf(fid,'GMAT Regression System Output File\n\n');
fprintf(fid,['Regression Test Performed by: ',TesterInitials,'\n']);
fprintf(fid,['Regression Test Performed on: ',num2str(currDate(1)),'-',MM,'-',DD,'\n']);
if strcmpi(BBorBG,'bg');
    fprintf(fid,'Comparison Type: Build Date vs Good data\n');
else
    fprintf(fid,'Comparison Type: Build Date vs Build Date data\n');
end;
fprintf(fid,['Folder1: ',Folder1,'\n']);
fprintf(fid,['Folder2: ',Folder2,'\n\n']);
fclose(fid);

% Display header for comparison results
disp(' ');
disp('====================================================');
disp('Generating Summary of comparison results for display');
disp('Details of the comparison can be found in');
disp([regSummaryPath,regSummaryFile]);
if strcmpi(BBorBG,'bg');
    disp('Comparison Type: Build Date vs Good data');
else
    disp('Comparison Type: Build Date vs Build Date data');
end;
disp('====================================================');
disp(' ');

if strcmpi(What2Compare,'files');
    % NOT FUNCTIONAL YET
    disp(' ');
    disp('Individual file comparisons NOT FUNCTIONAL YET');
    return;
elseif strcmpi(What2Compare,'test');
    % NOT FUNCTIONAL YET
    disp(' ');
    disp('Comparing only test specific files option is NOT FUNCTIONAL YET');
    return;
elseif strcmpi(What2Compare,'all');
    if strcmp(lower(BBorBG),'bg');
        RegSys_compareFiles(Build2,Build1,Folder1,Folder2,mainDir,What2Compare,BBorBG,goodFilesAll,build1FilesAll,regSummaryPath,regSummaryFile);
    else
        RegSys_compareFiles(Build1,Build2,Folder1,Folder2,mainDir,What2Compare,BBorBG,build1FilesAll,build2FilesAll,regSummaryPath,regSummaryFile);
    end;
end;