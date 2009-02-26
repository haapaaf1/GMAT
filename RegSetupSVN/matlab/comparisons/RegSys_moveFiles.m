function [] = RegSys_moveFiles(Build1, Build2, mainDir, Move_AllOneBuild, ...
    Move_AllBuild, Move_SystemTestBuild, Move_AcceptTestBuild, ...
    Move_ValidationBuild)
% $Id: RegSys_moveFiles.m,v 1.4 2007/09/11 13:30:31 edove Exp $

% Initialize variables
folderCount = 0;
reportFolders = [];
reportFoldersFiles = [];
reportFilesAll = {};
goodFolders = [];

% Search for valid test folders.
temp = dir([mainDir,'/output',]);
loopFolder = size(temp,1);

for loop = 1:loopFolder;
    if (temp(loop).isdir == 1) & (strcmp(temp(loop).name, '.') ~= 1) & (strcmp(temp(loop).name, '..') ~= 1);
        if (strcmp(lower(temp(loop).name), 'comparisons') ~= 1) & (strcmp(lower(temp(loop).name), 'cvs') ~= 1);
            folderCount = folderCount + 1;
            validFolder{folderCount,1} = temp(loop).name;
        end;
    end;
end;

% Delete existing report and/or good files located in the Good_reports folder
% The good_report folder for the valid test types (AcceptTest, SystemTest, etc) must exist
% Check to see if good directory exists in all valid test folders
for loop = 1:folderCount
    if exist([mainDir,'/output/',validFolder{loop,1},'/Good_reports'],'dir') == 7;
        goodFolders = [goodFolders, loop];
    else;
        disp(' ')
        disp('The following folder for Good reports does not exist:');
        disp([mainDir,'/output/',validFolder{loop,1},'/Good_reports']);
    end;
end;

% Check to see if there are report files in the good folder directory
% Store all good files in one structure
for loop = goodFolders;
    cd([mainDir,'/output/',validFolder{loop,1},'/Good_reports']);
    for report_goodLoop = 1:2
        if report_goodLoop == 1;
            goodFoldersFiles = [];
            goodFilesAll = {};
            goodFilesCheck = dir('*.good');
        elseif report_goodLoop == 2;
            goodFoldersFiles = [];
            goodFilesAll = {};
            goodFilesCheck = dir('*.report');
        end
        if size(goodFilesCheck,1) ~= 0;
            try
                % Check to see if the goodFilesAll structure exists
                if isa(goodFilesAll,'struct')

                end
            catch
                % Create goodFilesAll structure is it doesn't exist
                eval(['goodFilesAll.',validFolder{loop,1},' = {};']);
            end

            % Check to see if the test has been created as a field in the
            % goodFilesAll structure

            if isfield(goodFilesAll,validFolder{loop,1});

            else;
                eval(['goodFilesAll.',validFolder{loop,1},' = {};']);
            end;

            goodFoldersFiles = [goodFoldersFiles, loop];
            for innerLoop = 1:size(goodFilesCheck,1);
                delete([mainDir,'/output/',validFolder{loop,1},'/Good_reports/',goodFilesCheck(innerLoop,1).name])
            end;
        end;
    end;
end;

% Check to see if directory containing report files exists
for loop = 1:folderCount
    if exist([mainDir,'/output/',validFolder{loop,1},'/',num2str(Move_AllBuild),'GMAT_reports'],'dir') == 7;
        reportFolders = [reportFolders, loop];
    else;
        disp(' ')
        disp('The following folder for Build reports does not exist:');
        disp([mainDir,'/output/',validFolder{loop,1},'/',num2str(Move_AllBuild),'GMAT_reports']);
    end;
end;

% Check to see if there are report files in the report folder directory
% Store all report files in one structure

disp(' ');
disp(['Please wait copying ',num2str(Move_AllBuild),' build report files to good folder(s)']);

for loop = reportFolders;
    cd([mainDir,'/output/',validFolder{loop,1},'/',num2str(Move_AllBuild),'GMAT_reports']);
    reportFilesCheck = dir('*.report');
    if size(reportFilesCheck,1) ~= 0;
        try
            % Check to see if the reportFilesAll structure exists
            if isa(reportFilesAll,'struct')

            end
        catch
            % Create reportFilesAll structure is it doesn't exist
            eval(['reportFilesAll.',validFolder{loop,1},' = {};']);
        end
        
        % Check to see if the test has been created as a field in the
        % reportFilesAll structure
        
        if isfield(reportFilesAll,validFolder{loop,1});
            
        else;
           eval(['reportFilesAll.',validFolder{loop,1},' = {};']); 
        end;
        
        reportFoldersFiles = [reportFoldersFiles, loop];
        for innerLoop = 1:size(reportFilesCheck,1);
            dotLoc = strfind(reportFilesCheck(innerLoop,1).name,'.report') - 1;
            copyfile([mainDir,'/output/',validFolder{loop,1},'/',num2str(Move_AllBuild),'GMAT_reports/',reportFilesCheck(innerLoop,1).name],[mainDir,'/output/',validFolder{loop,1},'/Good_reports/',reportFilesCheck(innerLoop,1).name(1:dotLoc),'.good']);
        end;

    else;
        disp(' ')
        disp('The following folder does not contain any *.report files:');
        disp([mainDir,'/output/',validFolder{loop,1},'/',num2str(Move_AllBuild),'GMAT_reports']);
    end;
end;

return