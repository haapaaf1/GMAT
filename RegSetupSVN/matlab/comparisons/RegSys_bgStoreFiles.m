function [build1FilesAll,build2FilesAll] = RegSys_bgStoreFiles(Build1, Build2, Folder1, Folder2, mainDir)
% $Id: RegSys_bgStoreFiles.m,v 1.2 2007/02/13 21:04:38 edove Exp $

%   Modification History
%   ---------------------------------------------------------------------------------
%   ??/??/???? - E.Dove:  Created the first version.
%   04/21/2009 - E.Dove:  Removed the saving of the *.report and *.good as part of filename. 
%      Enabled build to build comparison functionality.


% Initialize variables
folderCount = 0;
build1Folders = [];
build1FoldersFiles = [];
build1FilesAll = {};
build2Folders = [];
build2FoldersFiles = [];
build2FilesAll = {};
if isempty(Build1)
    build1FileExt  = '.good';
else
    build1FileExt  = '.report';
end
build2FileExt = '.report';

% Determine the file extension linked to 

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

% Check to see if Build1 directory exists in all valid test folders
for loop = 1:folderCount
    if exist([mainDir,'/output/',validFolder{loop,1},'/',Folder1],'dir') == 7;
        build1Folders = [build1Folders, loop];
    else
        disp(' ')
        disp('The following folder for Build1 reports does not exist:');
        disp([mainDir,'/output/',validFolder{loop,1},'/',Folder1]);
    end;
end;

% Check to see if there are Build1 files in the Build1 folder directory
% Store all Build1 files in one structure

for loop = build1Folders;
    cd([mainDir,'/output/',validFolder{loop,1},'/',Folder1]);
    build1FilesCheck = dir(['*',build1FileExt]);
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
            
        else
           eval(['build1FilesAll.',validFolder{loop,1},' = {};']); 
        end;
        
        build1FoldersFiles = [build1FoldersFiles, loop];
        for innerLoop = 1:size(build1FilesCheck,1);
            eval(['build1FilesAll.',validFolder{loop,1},' = [build1FilesAll.',validFolder{loop,1},';{build1FilesCheck(innerLoop,1).name(1:size(build1FilesCheck(innerLoop,1).name,2)-size(build1FileExt,2))}];']);
        end;
    else
        disp(' ')
        disp(['The following folder does not contain any *',build1FileExt,' files:']);
        disp([mainDir,'/output/',validFolder{loop,1},'/',Folder1]);
    end;
end;

% Check to see if Build2 directory exists in all valid test folders
for loop = 1:folderCount
    if exist([mainDir,'/output/',validFolder{loop,1},'/',Folder2],'dir') == 7;
        build2Folders = [build2Folders, loop];
    else
        disp(' ')
        disp('The following folder for Build reports does not exist:');
        disp([mainDir,'/output/',validFolder{loop,1},'/',Folder2]);
    end;
end;

% Check to see if there are report files in the Build2 folder directory
% Store all Build2 files in one structure

for loop = build2Folders;    
    cd([mainDir,'/output/',validFolder{loop,1},'/',Folder2]);
    build2FilesCheck = dir(['*',build2FileExt]);
    if size(build2FilesCheck,1) ~= 0;
        try
            % Check to see if the build2FilesAll structure exists
            if isa(build2FilesAll,'struct')
                
            end
        catch
            % Create build2FilesAll structure is it doesn't exist
           eval(['build2FilesAll.',validFolder{loop,1},' = {};']); 
        end
        
        % Check to see if the test has been created as a field in the
        % build2FilesAll structure
        
        if isfield(build2FilesAll,validFolder{loop,1});
            
        else
           eval(['build2FilesAll.',validFolder{loop,1},' = {};']); 
        end;
        
        build2FoldersFiles = [build2FoldersFiles, loop];
        for innerLoop = 1:size(build2FilesCheck,1);
            eval(['build2FilesAll.',validFolder{loop,1},' = [build2FilesAll.',validFolder{loop,1},';{build2FilesCheck(innerLoop,1).name(1:size(build2FilesCheck(innerLoop,1).name,2)-size(build2FileExt,2))}];']);
        end;
    else
        disp(' ')
        disp(['The following folder does not contain any *',build2FileExt,' files:']);
        disp([mainDir,'/output/',validFolder{loop,1},'/',Folder2]);
    end;
end;