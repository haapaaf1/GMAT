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
    [goodFilesAll,build1FilesAll] = RegSys_bgStoreFiles(Build1,mainDir); % *** Function Call ***
else
%     RegSys_bbStoreFiles(Build1,mainDir)    
    % NOT FUNCTIONAL YET
    Folder1 = [num2str(Build1),'GMAT_reports'];
    Folder2 = [num2str(Build2),'GMAT_reports'];
    disp(' ');
    disp('Build to Build Comparisons NOT FUNCTIONAL YET');
    return;
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
%     if strcmp(lower(BBorBG),'bg');
%         RegSys_compareFiles(Build1,mainDir,What2Compare,BBorBG,goodFilesAll,build1FilesAll);
%     else;
%         RegSys_compareFiles(Build1,mainDir,What2Compare,BBorBG,build1FilesAll,build2FilesAll);
%     end;
end;

% \/ EXPORT to seperate function once finished coding
sharedTestLoc = [];

folder1Tests = fieldnames(goodFilesAll);
size(folder1Tests,1);

folder2Tests = fieldnames(build1FilesAll);
size(folder2Tests,1);

% Find the tests that both report folders share together
for loop = 1:size(folder1Tests,1);
    findSharedTestLoc = strcmp(folder2Tests, folder1Tests{loop,1});
    if isempty(find(findSharedTestLoc))
        sharedTestLoc(loop,1) = 0;
    else
        sharedTestLoc(loop,1) = find(findSharedTestLoc);
    end;
end;

% Compare both folders using file differencing
for testLoop = find(sharedTestLoc)'; % Loop of tests in common
    trackDNE = 0; 
    trackLDM = 0;
    trackMatch = 0;
    numFolder1ScriptFiles = size(eval(['goodFilesAll.',folder1Tests{testLoop,1}]),1);
    numFolder2ScriptFiles = size(eval(['build1FilesAll.',folder1Tests{testLoop,1}]),1);
    
    % Output header for each test type saved to regression summary file
    fid = fopen([regSummaryPath,regSummaryFile], 'a');
    fprintf(fid,['\n',folder1Tests{testLoop,1},' Results\n']);
    fprintf(fid,['Test Folder Report Location: /output/',folder1Tests{testLoop,1},'\n']);
    fprintf(fid,'---------------------------\n');
    
    for scriptLoop = 1:numFolder1ScriptFiles; % Loop of Folder 1 scripts
        currFileName = ['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',1}'];
        findExtLoc = strfind(eval(currFileName),'.good');
        findReportLoc = strcmp(eval(['build1FilesAll.',folder1Tests{testLoop,1}]), [eval([currFileName,'(1:',num2str(findExtLoc-1),')']),'.report']); % Find report in Folder2. Always .report.
        
        if isempty(find(findReportLoc))
%             disp(' ');
%             disp('File not found in ...');
            % INSERT better error message here and what to do.
            % Track the files that aren't in common
            
            % Check to see if file exists in both folders and send details to regression summary
            trackDNE = trackDNE + 1;
            eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2} = [''DNE''];']); % File Does Not Exist (DNE)
            fprintf(fid,[eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',1}(1:',num2str(findExtLoc-1),');']),' - Does not exist in both folders\n']);
            continue;
        else
            reportLoc = find(findReportLoc); % Track the files in the second folder that are in common

            fid2 = fopen([mainDir,'/output/',folder1Tests{testLoop,1},'/Good_reports/',eval([currFileName,'(1:',num2str(findExtLoc-1),')']),'.good']);
            folder1Data = textscan(fid2,'%s','delimiter','\n');
            fclose(fid2);

            fid2 = fopen([mainDir,'/output/',folder1Tests{testLoop,1},'/',num2str(Build1),'GMAT_reports/',eval([currFileName,'(1:',num2str(findExtLoc-1),')']),'.report']);
            folder2Data = textscan(fid2,'%s','delimiter','\n');
            fclose(fid2);

            % Check to see if lines match and send details to regression summary
            script1TotalLines = size(folder1Data{1,1},1);
            script2TotalLines = size(folder2Data{1,1},1);
            if size(folder1Data{1,1},1) == size(folder2Data{1,1},1);
                for scriptLineLoop = 1:script1TotalLines;
                    try
                        isnumeric(eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2}']));
                    catch
                        eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2} = [];']);
                    end;
                    if isempty(strmatch(folder1Data{1,1}{scriptLineLoop,1},folder2Data{1,1}{scriptLineLoop,1},'exact'));
                        % Store lines that don't match in the second column of [folder]FilesAll.[Test]{1,2}
                        eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2} = [goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2};', num2str(scriptLineLoop),'];']);
                    else
                        
                    end;
                end;
                
                % Append file results to regression summary file
                if size(eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2}']),1) > 5;
                    fprintf(fid,[eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',1}(1:',num2str(findExtLoc-1),');']),' - More than 5 lines do not match\n']);
                elseif (size(eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2}']),1) > 0) & (size(eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2}']),1) <= 5);
                    fprintf(fid,[eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',1}(1:',num2str(findExtLoc-1),');']),' - Line(s) ',num2str(eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2}'''])),' do(es) not match\n']);
                end;
                
            else
%                 disp(' ');
%                 disp('The amount of report lines in the file do not match one another.');
                % Track the files that don't match by size  
                
                % Check to see if both files have the same amount of lines and send details to regression summary
                trackLDM = trackLDM + 1;
                eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2} = [''LDM''];']); % File Lines Don't Match (LDM)
                fprintf(fid,[eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',1}(1:',num2str(findExtLoc-1),');']),' - File does not have the same amount of lines in both folders\n']);
            end;
        end;
        
        % Generate a summary of the mismatched Validation lines
        S1LineMatchesInS2 = [];    % Stores the script lines/errors in script1 that match the lines/errors in script2
        S2LineMatchesInS1 = [];    % Stores the script lines/errors in script2 that match the lines/errors in script1
        S1LineMismatchesInS2 = []; % Stores the script lines/errors in script1 that doesn't match the lines/errors in script2
        S2LineMismatchesInS1 = []; % Stores the script lines/errors in script2 that doesn't match the lines/errors in script1
        if strcmpi(folder1Tests{testLoop,1},'validation')
            % Find the lines of the validations report that match
            % in script1 and script2, excluding the error number
            % First line of an error looks like --> #: **** ERROR

            if isempty(strfind(eval(currFileName),'test_xyploy')) == 0
%                 keyboard;
                disp('hello');
            end

            for scriptLineLoop = 1:script1TotalLines
                findColons = strfind(folder1Data{1,1}{scriptLineLoop,1},':');
                if isempty(findColons) % No colons found in current script1 line
                    S2matchesRowLoc   = find(strcmp(folder1Data{1,1}{scriptLineLoop,1},folder2Data{1,1}));
                    S2LineMatchesInS1 = [S2LineMatchesInS1; S2matchesRowLoc];
                    if isempty(S2matchesRowLoc) % No match found in current script1 error line to any script2 line
                        S1LineMismatchesInS2 = [S1LineMismatchesInS2; scriptLineLoop];
                    else % Match found in current script1 line to one or more script2 lines
                        S1LineMatchesInS2 = [S1LineMatchesInS2; scriptLineLoop];
                    end
                else % One or more colons found in script1 line
                    locFirstColon = findColons(1,1);
                    if locFirstColon <= 3 % Current line contains the first line of an error
                        line1colSize = size(folder1Data{1,1}{scriptLineLoop,1},2);
                        txt4search = folder1Data{1,1}{scriptLineLoop,1}(locFirstColon:line1colSize); % Removes the error number for the script
                        findSameError = strfind(folder2Data{1,1},txt4search);  % Searches script2 for the same error message
                        strfindRowLoc = find(cellfun('size', findSameError, 1)); % Convert strFind results to be in terms of row locations of script2
                        if isempty(strfindRowLoc) % No match found in current script1 error line to any script2 line
                            S1LineMismatchesInS2 = [S1LineMismatchesInS2; scriptLineLoop];
                        else % Current script1 error occurs in script2
                            S1LineMatchesInS2 = [S1LineMatchesInS2; scriptLineLoop];
                            S2LineMatchesInS1 = [S2LineMatchesInS1; strfindRowLoc];
                        end
                    else % Current script1 line does not contain the first line of an error
                        S2matchesRowLoc   = find(strcmp(folder1Data{1,1}{scriptLineLoop,1},folder2Data{1,1}));
                        S2LineMatchesInS1 = [S2LineMatchesInS1; S2matchesRowLoc];
                        if isempty(S2matchesRowLoc) % No match found in current script1 error line to any script2 line
                            S1LineMismatchesInS2 = [S1LineMismatchesInS2; scriptLineLoop];
                        else % Match found in current script1 line to one or more script2 lines
                            S1LineMatchesInS2 = [S1LineMatchesInS2; scriptLineLoop];
                        end
                    end
                end
            end
            S1LineMatchesInS2 = unique(sort(S1LineMatchesInS2));
            S2LineMatchesInS1 = unique(sort(S2LineMatchesInS1));
            lineRngOfS2       = 1:script2TotalLines;
            temp = num2str([lineRngOfS2';S2LineMatchesInS1]);% Make sure character arrays are same width. Needed for string searching.
            lineRngOfS2str       = cellstr(temp(1:script2TotalLines,:));
            S2LineMatchesInS1str = cellstr(temp(script2TotalLines+1:size(temp,1),:));
            for loop = 1:script2TotalLines
                findMismatch = strmatch(lineRngOfS2str{loop},S2LineMatchesInS1str);
                if isempty(findMismatch)
                    S2LineMismatchesInS1 = [S2LineMismatchesInS1;loop];
                end
            end

            % Store all lines that don't match into one string array
            validMismatchData = [{'Linked to the following file:'};...
                {regSummaryFile};{' '};...
                {['Mismatched lines from ', Folder1]};...
                {'======================'};folder1Data{1,1}(S1LineMismatchesInS2,1);...
                {' '};{['Mismatched lines from ', Folder2]};...
                {'======================'}; folder2Data{1,1}(S2LineMismatchesInS1,1)];

            if (isempty(S2LineMismatchesInS1) == 0) || (isempty(S1LineMismatchesInS2) == 0)
                % Send mismatch lines to a file located in the report directory
                fid3 = fopen([mainDir,'/output/',folder1Tests{testLoop,1},'/',num2str(Build1),'GMAT_reports/',eval([currFileName,'(1:',num2str(findExtLoc-1),')']),'.regrpt'], 'w');
                for loop = 1:size(validMismatchData,1)
                    fprintf(fid3,[validMismatchData{loop,1},'\n']);
                end
                fclose(fid3);
            end
        end
        
        % Report lines that matched
        if isempty(eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2}']));
            trackMatch = trackMatch + 1;
            eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',2} = [''Match''];']);% Flag as a match
            fprintf(fid,[eval(['goodFilesAll.',folder1Tests{testLoop,1},'{',num2str(scriptLoop),',1}(1:',num2str(findExtLoc-1),');']),' - Exact match\n']);
        end;
    end;
    trackTotalFiles = numFolder1ScriptFiles + abs(numFolder2ScriptFiles - (numFolder1ScriptFiles - trackDNE));
    trackFailed = numFolder1ScriptFiles - (trackMatch + trackLDM + trackDNE);
    
    % Report Summary results
    disp([folder1Tests{testLoop,1},' Results'])
    disp('----------------------------------------------------');
    disp([num2str(trackDNE + abs(numFolder2ScriptFiles - (numFolder1ScriptFiles - trackDNE))),' out of ',num2str(trackTotalFiles),' unique test reports did not exist in both folders']);
    disp([num2str(trackLDM),' out of ',num2str(trackTotalFiles),' unique test reports existed in both folders but have a different number of lines']);
    disp([num2str(trackFailed),' out of ',num2str(trackTotalFiles),' unique test reports existed in both folders but some lines did not match']);
    disp(' ');
    
    fprintf(fid,['\nThe remainder of the files, if any, that do not exist are located in ',Folder2,'\n']);
    fprintf(fid,'\nSummary\n');
    fprintf(fid,[num2str(trackDNE + abs(numFolder2ScriptFiles - (numFolder1ScriptFiles - trackDNE))),' out of ',num2str(trackTotalFiles),' unique test reports did not exist in both folders\n']);
    fprintf(fid,[num2str(trackLDM),' out of ',num2str(trackTotalFiles),' unique test reports existed in both folders but have a different number of lines\n']);
    fprintf(fid,[num2str(trackFailed),' out of ',num2str(trackTotalFiles),' unique test reports existed in both folders but some lines did not match\n']);
    fprintf(fid,'---------------------------\n\n');
    fclose(fid);
end;

% /\ EXPORT to seperate function once finished coding