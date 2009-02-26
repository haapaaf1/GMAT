function [goodFilesAll,build1FilesAll] = RegSys_bgStoreFiles(Build1,mainDir)
% $Id: RegSys_bgStoreFiles.m,v 1.2 2007/02/13 21:04:38 edove Exp $

% Initialize variables
folderCount = 0;
goodFolders = [];
goodFoldersFiles = [];
goodFilesAll = {};
build1Folders = [];
build1FoldersFiles = [];
build1FilesAll = {};


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

% Check to see if good directory exists in all valid test folders
for loop = 1:folderCount
    if exist([mainDir,'/output/',validFolder{loop,1},'/Good_reports'],'dir') == 7;
        goodFolders = [goodFolders, loop];
    else;
        disp(' ')
        disp('The following folder for good reports does not exist:');
        disp([mainDir,'/output/',validFolder{loop,1},'/Good_reports']);
    end;
end;

% Check to see if there are good files in the good folder directory
% Store all good files in one structure

for loop = goodFolders;
    cd([mainDir,'/output/',validFolder{loop,1},'/Good_reports']);
    goodFilesCheck = dir('*.good');
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
            eval(['goodFilesAll.',validFolder{loop,1},' = [goodFilesAll.',validFolder{loop,1},';{goodFilesCheck(innerLoop,1).name}];']);
        end;
    else;
        disp(' ')
        disp('The following folder does not contain any *.good files:');
        disp([mainDir,'/output/',validFolder{loop,1},'/Good_reports']);
    end;
end;

% Check to see if Build1 directory exists in all valid test folders
for loop = 1:folderCount
    if exist([mainDir,'/output/',validFolder{loop,1},'/',num2str(Build1),'GMAT_reports'],'dir') == 7;
        build1Folders = [build1Folders, loop];
    else;
        disp(' ')
        disp('The following folder for Build reports does not exist:');
        disp([mainDir,'/output/',validFolder{loop,1},'/',num2str(Build1),'GMAT_reports']);
    end;
end;

% Check to see if there are report files in the Build1 folder directory
% Store all Build1 files in one structure

for loop = build1Folders;    
    cd([mainDir,'/output/',validFolder{loop,1},'/',num2str(Build1),'GMAT_reports']);
    build1FilesCheck = dir('*.report');
    if size(build1FilesCheck,1) ~= 0;
        try
            % Check to see if the build1FilesAll structure exists
            if isa(build1FilesAll,'struct')
                
            end
        catch
            % Create build1FilesAll structure is it doesn't exist
           eval(['build1FilesAll.',validFolder{loop,1},' = {};']); 
        end
        
        % Check to see if the test has been created as a field in the
        % build1FilesAll structure
        
        if isfield(build1FilesAll,validFolder{loop,1});
            
        else;
           eval(['build1FilesAll.',validFolder{loop,1},' = {};']); 
        end;
        
        build1FoldersFiles = [build1FoldersFiles, loop];
        for innerLoop = 1:size(build1FilesCheck,1);
            eval(['build1FilesAll.',validFolder{loop,1},' = [build1FilesAll.',validFolder{loop,1},';{build1FilesCheck(innerLoop,1).name}];']);
        end;
    else;
        disp(' ')
        disp('The following folder does not contain any *.report files:');
        disp([mainDir,'/output/',validFolder{loop,1},'/',num2str(Build1),'GMAT_reports']);
    end;
end;