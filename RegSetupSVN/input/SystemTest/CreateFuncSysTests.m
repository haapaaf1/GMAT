function CreateFuncSysTests

%   CreateFuncSysTests generates GMAT Function and nested GMAT Function counterparts of all SystemTest scripts
%
%   Modification History
%   ---------------------------------------------------------------------------------
%   05/22/2009 - E.Dove:  Created the first version.
%   

%% Initialization
clc
clear all
close all

% Save the current file's folder to a variable for later us
tempDir = mfilename('fullpath');
if ispc;
    temp = findstr(tempDir,'\');
    SystemTestFolder = tempDir(1:temp(size(temp,2))-1);
else
    temp = findstr(tempDir,'/');
    SystemTestFolder = tempDir(1:temp(size(temp,2))-1);
end;
cd(SystemTestFolder);

% Initialize Variables
findRowExclude = [];
allFileRows = [];
funcFolder = [SystemTestFolder,'/Functions/'];

% Find all relevant file extensions in the current folder
storeScripts1 = dir('*.script');
storeScripts2 = dir('*.m');

storeScriptNames = {storeScripts1.name,storeScripts2.name}';


% Store the contents of each file into a cell array
for storeLoop = 1:size(storeScriptNames,1)

    % Open script file and save text into a cell array
    fid = fopen([SystemTestFolder,'/',storeScriptNames{storeLoop,1}]);
    loadScript = textscan(fid,'%s','delimiter','\n');
    fclose(fid);
    storedScripts{storeLoop,1} = loadScript{1,1};

    
    if isempty(strmatch('function',storedScripts{storeLoop,1})) == 0 || isempty(strmatch('CreateFuncSysTests.script',storeScriptNames{storeLoop,1})) == 0
        findRowExclude = [findRowExclude;storeLoop];
    end
end

% Find the row location of storeScriptNames associated with GMAT scripts (non-Matlab function files)
gmatScriptRows = setdiff([1:size(storeScriptNames,1)],findRowExclude);

for createFuncLoop = gmatScriptRows
    %% Create the GMAT Function file (gmf)
    findFileMat  = [];
    fileDataNestedFunc = {};
    fileDataScript = {};
    createStringCell = {};
    currReportFile = [];
    currRptName    = [];
    rptFileVarInpt = [];
    rptFileVarInptNest = [];
    
    createStringCell{1,1} = ' ';
    funcFile     = ['Func_',storeScriptNames{createFuncLoop,1}];
    funcFile     = strrep(funcFile,'.script','');
    funcFile     = strrep(funcFile,'.m','');
    funcNestFile = [funcFile,'_nested'];
    
    % Find row location of report file creation lines.
    createRptRows = find(strncmp('Create ReportFile',storedScripts{createFuncLoop,1},17));

    % Find row location of report filename used, if any.
    for rowLoop = 1:size(createRptRows,1)
        RptLine = storedScripts{createFuncLoop,1}{createRptRows(rowLoop)};
        currRptName = RptLine(19:size(RptLine,2));
        currRptName = strrep(currRptName,';','');
        currRptName = strrep(currRptName,' ','');
        rptFileVar{rowLoop} = ['reportLoc',num2str(rowLoop)];
        
        if rowLoop == 1 && rowLoop ~= size(createRptRows,1)
            rptFileVarInptNest = ['(',rptFileVar{rowLoop},'_nested,'];
        elseif rowLoop == 1 && rowLoop == size(createRptRows,1)
            rptFileVarInptNest = ['(',rptFileVar{rowLoop},'_nested)'];
        elseif rowLoop ~= 1 && rowLoop == size(createRptRows,1)
            rptFileVarInptNest = [rptFileVarInptNest,rptFileVar{rowLoop},'_nested)'];
        else
            rptFileVarInptNest = [rptFileVarInptNest,rptFileVar{rowLoop},'_nested,'];
        end
        rptFileVarInpt   = strrep(rptFileVarInptNest,'_nested','');
        
        findFileCell = strfind(storedScripts{createFuncLoop,1},[currRptName,'.Filename ']);
        for cellLoop = 1:size(findFileCell,1)
            if isempty(findFileCell{cellLoop,1}) 
                findFileMat(cellLoop,1) = 0;
            else
                findFileMat(cellLoop,1) = 1;
            end
        end
        findFileRow = find(findFileMat);
        allFileRows = [allFileRows;findFileRow];
        
        % Save the name of report file for later use
        colLocB4File = [];
        if isempty(findFileRow)
            % No filename specified so using default report name based on reportFile name
            reportFiles{rowLoop,1} = [currRptName,'.txt'];
            
            % Add line to specify report file name
            storedScripts{createFuncLoop,1} = [storedScripts{createFuncLoop,1}(1:createRptRows(rowLoop)); ...
                ['GMAT ',currRptName,'.Filename = ',rptFileVar{rowLoop},';']; ...
                storedScripts{createFuncLoop,1}(createRptRows(rowLoop)+1:size(storedScripts{createFuncLoop,1},1))];
        else
            % Filename specified
            findFileNmBackSlash = findstr(storedScripts{createFuncLoop,1}{findFileRow},'\');
            findFileNmFwdSlash  = findstr(storedScripts{createFuncLoop,1}{findFileRow},'/');
%             findFileNmQuote     = findstr(storedScripts{createFuncLoop,1}{findFileRow},'''');
            findFileNmEqual     = findstr(storedScripts{createFuncLoop,1}{findFileRow},'=');
%             findFileNmColon     = findstr(storedScripts{createFuncLoop,1}{findFileRow},';');
            findFileNmPerc      = findstr(storedScripts{createFuncLoop,1}{findFileRow},'%');
            
            % Find column location of reportFile filename line just before start of the file name
            if isempty(findFileNmBackSlash) && isempty(findFileNmFwdSlash)
                % No path specified for filename
                colLocB4File = findFileNmEqual(1) + 1;
            else
                % Path specified for filename
                colLocB4File = max([findFileNmFwdSlash,findFileNmBackSlash])+1;
            end
            
            % Find column location of reportFile filename line after the end of the file name
            if isempty(findFileNmPerc)
                colLocAftFile = size(storedScripts{createFuncLoop,1}{findFileRow},2);
            else
                colLocAftFile = findFileNmPerc(1)-1;
            end
            
            % Store string of report file name
            currReportFile = storedScripts{createFuncLoop,1}{findFileRow}(colLocB4File:colLocAftFile);        
            currReportFile = strrep(currReportFile,'''','');
            currReportFile = strrep(currReportFile,';','');
            currReportFile = strrep(currReportFile,' ','');
            fileExtLoc     = max(findstr(currReportFile,'.'));
            currReportFile = currReportFile(1:fileExtLoc-1);
            
            reportFiles{rowLoop,1} = currReportFile; 
            storedScripts{createFuncLoop,1}{findFileRow} = [storedScripts{createFuncLoop,1}{findFileRow}(1:findFileNmEqual),' ',rptFileVar{rowLoop},';'];
        end

        createStringCell = [createStringCell; ...
            ['Create String ',rptFileVar{rowLoop},';']; ...
            ['GMAT ',rptFileVar{rowLoop},' = ''./output/SystemTest/Func_',currReportFile,'.report'';']; ...
            ' '; ...
            ['Create String ',rptFileVar{rowLoop},'_nested;']; ...
            ['GMAT ',rptFileVar{rowLoop},'_nested',' = ''./output/SystemTest/Func_',currReportFile,'_nested.report'';']; ...
            ' '];
    end

    % Add function line to top of file
    storedScripts{createFuncLoop,1} = [['function ',funcFile,rptFileVarInpt] ; ...
        storedScripts{createFuncLoop,1}(1:size(storedScripts{createFuncLoop,1},1))];
    
    % Save the GMAT Function file
    fid = fopen([funcFolder,funcFile,'.gmf'],'w');
    for loop = 1:size(storedScripts{createFuncLoop,1},1);
%         storedScripts{createFuncLoop,1}{loop} = strrep(storedScripts{createFuncLoop,1}{loop},'''','''''');
        storedScripts{createFuncLoop,1}{loop} = strrep(storedScripts{createFuncLoop,1}{loop},'%','%%');
        warning off;
        fprintf(fid, [storedScripts{createFuncLoop,1}{loop},'\n']);
        warning on;
    end;
    fclose(fid);
    
    %% Create the script file 
    fileDataScript{1,1} = ['%% Created from the following script: ',storeScriptNames{createFuncLoop,1}];
    fileDataScript{numel(fileDataScript)+1,1} = '%%';
    fileDataScript{numel(fileDataScript)+1,1} = ['%% Functions used: ',funcFile,'.gmf, ',funcNestFile,'.gmf'];
    fileDataScript{numel(fileDataScript)+1,1} = '%% Function path: ./output/SystemTest/';
    fileDataScript{numel(fileDataScript)+1,1} = '%% Outputs to: ./output/SystemTest/';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = '%%----------------------------------------';
    fileDataScript{numel(fileDataScript)+1,1} = '%%---------- Spacecrafts';
    fileDataScript{numel(fileDataScript)+1,1} = '%%----------------------------------------';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = 'Create Spacecraft DefaultSC;';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = '%%----------------------------------------';
    fileDataScript{numel(fileDataScript)+1,1} = '%%---------- Propagators';
    fileDataScript{numel(fileDataScript)+1,1} = '%%----------------------------------------';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = 'Create ForceModel DefaultProp_ForceModel;';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = 'Create Propagator DefaultProp;';
    fileDataScript{numel(fileDataScript)+1,1} = 'GMAT DefaultProp.FM = DefaultProp_ForceModel;';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = '%%----------------------------------------';
    fileDataScript{numel(fileDataScript)+1,1} = '%%---------- Variables, Arrays, Strings';
    fileDataScript{numel(fileDataScript)+1,1} = '%%----------------------------------------';
    fileDataScript = [fileDataScript;createStringCell];   
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = '%%----------------------------------------';
    fileDataScript{numel(fileDataScript)+1,1} = '%%---------- Mission Sequence';
    fileDataScript{numel(fileDataScript)+1,1} = '%%----------------------------------------';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = '%% Begin/EndScript used to force GMAT to go into command mode. Might';
    fileDataScript{numel(fileDataScript)+1,1} = '%% not be necessary but I''ve included it just in case.';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = 'BeginScript;';
    fileDataScript{numel(fileDataScript)+1,1} = 'EndScript;';
    fileDataScript{numel(fileDataScript)+1,1} = ' ';
    fileDataScript{numel(fileDataScript)+1,1} = '%% Call function';
    fileDataScript{numel(fileDataScript)+1,1} = [funcFile,rptFileVarInpt];
    fileDataScript{numel(fileDataScript)+1,1} = [funcNestFile,rptFileVarInptNest];

    % Save the script file
    fid = fopen([funcFolder,funcFile,'.script'],'w');
    for loop = 1:size(fileDataScript,1);
        warning off;
        fprintf(fid, [fileDataScript{loop,1},'\n']);
        warning on;
    end;
    fclose(fid);    

    %% Create nested function file
    fileDataNestedFunc{1,1} = ['function ',funcNestFile,rptFileVarInpt];
    fileDataNestedFunc{numel(fileDataNestedFunc)+1,1} = ' ';
    fileDataNestedFunc{numel(fileDataNestedFunc)+1,1} = '%% Call function';
    fileDataNestedFunc{numel(fileDataNestedFunc)+1,1} = [funcFile,rptFileVarInpt];
    
        % Save the script file
    fid = fopen([funcFolder,funcNestFile,'.gmf'],'w');
    for loop = 1:size(fileDataNestedFunc,1);
        warning off;
        fprintf(fid, [fileDataNestedFunc{loop,1},'\n']);
        warning on;
    end;
    fclose(fid);    
end

