function [] = RegSys_parseValidation(Build1,mainDir)
% $Id: RegSys_parseValidation.m,v 1.6 2008/03/17 20:08:50 edove Exp $
%
%   Modification History
%   ---------------------------------------------------------------------------------
%   ??/??/???? - E.Dove:  Created the first version.
%   05/19/2009 - E.Dove:  Throw an exception when scripts in log file aren't Validation scripts,
%       Delete existing *.regrpt and *.report files

validFolder = [mainDir,'/output/Validation/',num2str(Build1),'GMAT_reports/'];

if exist(validFolder,'dir') == 7;
    % Delete existing Validation reports
    delete([validFolder,'*.report']);
    delete([validFolder,'*.regrpt']);
    
    if exist([validFolder,'GMATlog.txt'],'file') == 2;
        % Open log file and store in Matlab
        fid1 = fopen([validFolder,'GMATlog.txt']);
        ValidData = textscan(fid1,'%s','delimiter','\n');
        fclose(fid1);
        
        % Find starting and ending points for each script log
        findScript1    = strmatch('Starting script',ValidData{1,1});
        storedScripts  = [ValidData{1,1}(findScript1)];
        findValidation = strfind(storedScripts,'Validation');
        findScript     = findScript1(find(cell2mat(findValidation)));
        
        if isempty(findScript)
            disp(' ')
            disp('The scripts contained in the batch run were not located in the Validation folder.')
            disp('Check to make sure the scripts contained in the log file are only Validation scripts.')
            disp('The log file in question is in the following folder:')
            disp(validFolder)
            return
        end
        
        lastScriptLog = strmatch('===> Grand Total',ValidData{1,1}(findScript(size(findScript,1))+1 : size(ValidData{1,1},1))) - 2; % Amount of lines after last row of findScript
        
        % If there is no script file summary for all files with errors 
        if isempty(lastScriptLog);
            lastScriptLog = size(ValidData{1,1},1) - findScript(size(findScript,1));
        end;
        
        % Cycle through each script log and save as a *.report file
        % Removing Total Time calculations when present
        for logScriptLoop = 1:size(findScript,1);
            if logScriptLoop < size(findScript,1);
                save2Report = ValidData{1,1}(findScript(logScriptLoop):(findScript(logScriptLoop+1)));
                findEndLog = strmatch('Errors were found',save2Report)-1;
                findFilename = max([cell2mat(strfind(ValidData{1,1}(findScript(logScriptLoop)), '\')), cell2mat(strfind(ValidData{1,1}(findScript(logScriptLoop)), '/'))]);
                findExt = max(cell2mat(strfind(ValidData{1,1}(findScript(logScriptLoop)), '.')));
                validFile = [ValidData{1,1}{findScript(logScriptLoop)}(findFilename+1:findExt-1),'.report'];
                
                fid = fopen([validFolder,validFile],'w');
                for loop = 1:findEndLog;
                    if isempty(strmatch('===> Total Run Time',save2Report{loop})) == 0;
                        save2Report{loop} = ' ';
                    end;
                    if isempty(strmatch('Starting script',save2Report{loop})) == 0;
                        save2Report{loop} = ' ';
                    end;
                    if isempty(strmatch('***** file',save2Report{loop})) == 0;
                        save2Report{loop} = ' ';
                    end;
                    
                    warning off;
                    fprintf(fid, [save2Report{loop},'\n']);
                    warning on;
                end;
                fclose(fid);
            else
                save2Report = ValidData{1,1}(findScript(logScriptLoop):(findScript(logScriptLoop)+lastScriptLog));
                findEndLog = strmatch('Errors were found',save2Report)-1;
                findFilename = max([cell2mat(strfind(ValidData{1,1}(findScript(logScriptLoop)), '\')), cell2mat(strfind(ValidData{1,1}(findScript(logScriptLoop)), '/'))]);
                findExt = max(cell2mat(strfind(ValidData{1,1}(findScript(logScriptLoop)), '.')));                
                validFile = [ValidData{1,1}{findScript(logScriptLoop)}(findFilename+1:findExt-1),'.report'];
                
                fid = fopen([validFolder,validFile],'w');
                for loop = 1:findEndLog;
                    if isempty(strmatch('===> Total Run Time',save2Report{loop})) == 0;
                        save2Report{loop} = ' ';
                    end;
                    if isempty(strmatch('Starting script',save2Report{loop})) == 0;
                        save2Report{loop} = ' ';
                    end;
                    if isempty(strmatch('***** file',save2Report{loop})) == 0;
                        save2Report{loop} = ' ';
                    end;                    
                    
                    warning off;
                    fprintf(fid, [save2Report{loop},'\n']);
                    warning on;
                end;
                fclose(fid);
            end;
        end;
    else
        disp(' ')
        disp('The Validation folder does not contain a GMATlog.txt file.');
        disp('If you want to run validation comparisons please rerun the');
        disp('Regression Testing script.');
    end;
end;