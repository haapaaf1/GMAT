function [] = RegSys_compareFiles(Build1,Build2,Folder1,Folder2,mainDir,What2Compare,BBorBG,Folder1FilesAll,Folder2FilesAll,regSummaryPath,regSummaryFile)
% $Id: RegSys_compareFiles.m,v 1.2 2007/02/13 21:04:38 edove Exp $

%   Modification History
%   ---------------------------------------------------------------------------------
%   ??/??/???? - E.Dove:  Created the first version.
%   04/21/2009 - E.Dove:  Enabled build to build comparison functionality and moved comparison
%       code to seperate function.
%   04/22/2009 - E.Dove:  Corrected bug that was applying AcceptTest data to all other test-types.
%   05/06/2009 - E.Dove:  Added error checking for when a folder selected does not exist.

% Initialize Variables
if isempty(Build1)
    build1FileExt  = '.good';
else
    build1FileExt  = '.report';
end
build2FileExt = '.report';
sharedTestLoc = [];

% Check to see if both folders contain data
if isempty(Folder1FilesAll) || isempty(Folder2FilesAll)
    if isempty(Folder1FilesAll) && isempty(Folder2FilesAll)
        disp(['The ', Folder1, ' and ', Folder2,' output folders located in the specified testSuite folder(s)'])
        disp('either don''t exist or there are no report files in them.')
        disp(' ')
        disp('Please correct this issue before rerunning the regression system.')
        return        
    elseif isempty(Folder1FilesAll)
        disp(['The ', Folder1, ' output folder located in the specified testSuite folder(s) either'])
        disp('doesn''t exist or there are no report files in it.')
        disp(' ')
        disp('Please correct this issue before rerunning the regression system.')
        return
    elseif isempty(Folder2FilesAll)
        disp(['The ', Folder2, ' output folder located in the specified testSuite folder(s) either'])
        disp('doesn''t exist or there are no report files in it.')
        disp(' ')
        disp('Please correct this issue before rerunning the regression system.')        
        return
    end
end

folder1Tests = fieldnames(Folder1FilesAll);
size(folder1Tests,1);

folder2Tests = fieldnames(Folder2FilesAll);
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
    
    % Output header for each test type saved to regression summary file
    fid = fopen([regSummaryPath,regSummaryFile], 'a');
    fprintf(fid,['\n',folder1Tests{testLoop,1},' Results\n']);
    fprintf(fid,['Test Folder Report Location: /output/',folder1Tests{testLoop,1},'\n']);
    fprintf(fid,'---------------------------\n');
    
    [diffFiles, diffIA, diffIB] = eval(['setxor(Folder1FilesAll.',folder1Tests{testLoop,1},',Folder2FilesAll.',folder1Tests{testLoop,1},')']);
    trackDNE = numel(diffIA) + numel(diffIB);
    [sameFiles, sameIA, sameIB] = eval(['intersect(Folder1FilesAll.',folder1Tests{testLoop,1},',Folder2FilesAll.',folder1Tests{testLoop,1},')']);
    
    % Report files that don't exist in both folders and send details to regression summary
    for scriptDiffLoopA = 1:numel(diffIA)
        eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(diffIA(scriptDiffLoopA)),',2} = [''DNE''];']); % File Does Not Exist (DNE)
        fprintf(fid,[eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(diffIA(scriptDiffLoopA)),',1};']),' - Does not exist in both folders\n']);
    end
    for scriptDiffLoopB = 1:numel(diffIB)
        eval(['Folder2FilesAll.',folder1Tests{testLoop,1},'{',num2str(diffIB(scriptDiffLoopB)),',2} = [''DNE''];']); % File Does Not Exist (DNE)
        fprintf(fid,[eval(['Folder2FilesAll.',folder1Tests{testLoop,1},'{',num2str(diffIB(scriptDiffLoopB)),',1};']),' - Does not exist in both folders\n']);
    end    
    
    % Determine if the files exist in both folders and their contents match
    for scriptLoop = 1:numel(sameIA); % Loop of same scripts
        currFileName = ['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',1}'];
        findReportLoc = strcmp(eval(['Folder2FilesAll.',folder1Tests{testLoop,1}]), eval(currFileName)); % Find report in Folder2.
        
        reportLoc = find(findReportLoc); % Track the files in the second folder that are in common

        fid2 = fopen([mainDir,'/output/',folder1Tests{testLoop,1},'/',Folder1,'/',eval(currFileName),build1FileExt]);
        folder1Data = textscan(fid2,'%s','delimiter','\n');
        fclose(fid2);

        fid2 = fopen([mainDir,'/output/',folder1Tests{testLoop,1},'/',Folder2,'/',eval(currFileName),build2FileExt]);
        folder2Data = textscan(fid2,'%s','delimiter','\n');
        fclose(fid2);

        % Check to see if lines match and send details to regression summary
        script1TotalLines = size(folder1Data{1,1},1);
        script2TotalLines = size(folder2Data{1,1},1);
        if size(folder1Data{1,1},1) == size(folder2Data{1,1},1);
            for scriptLineLoop = 1:script1TotalLines;
                try
                    isnumeric(eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2}']));
                catch
                    eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2} = [];']);
                end;
                if isempty(strmatch(folder1Data{1,1}{scriptLineLoop,1},folder2Data{1,1}{scriptLineLoop,1},'exact'));
                    % Store lines that don't match in the second column of [folder]FilesAll.[Test]{1,2}
                    eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2} = [Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2};', num2str(scriptLineLoop),'];']);
                else

                end;
            end;

            % Append file results to regression summary file
            if size(eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2}']),1) > 5;
                fprintf(fid,[eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',1};']),' - More than 5 lines do not match\n']);
            elseif (size(eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2}']),1) > 0) & (size(eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2}']),1) <= 5);
                fprintf(fid,[eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',1};']),' - Line(s) ',num2str(eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2}'''])),' do(es) not match\n']);
            end;

        else
            %                 disp(' ');
            %                 disp('The amount of report lines in the file do not match one another.');
            % Track the files that don't match by size

            % Check to see if both files have the same amount of lines and send details to regression summary
            trackLDM = trackLDM + 1;
            eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2} = [''LDM''];']); % File Lines Don't Match (LDM)
            fprintf(fid,[eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',1};']),' - File does not have the same amount of lines in both folders\n']);
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
                fid3 = fopen([mainDir,'/output/',folder1Tests{testLoop,1},'/',Folder2,'/',eval(currFileName),'.regrpt'], 'w');
                for loop = 1:size(validMismatchData,1)
                    fprintf(fid3,[validMismatchData{loop,1},'\n']);
                end
                fclose(fid3);
            end
        end
        
        % Report lines that matched
        if isempty(eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2}']));
            trackMatch = trackMatch + 1;
            eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',2} = [''Match''];']);% Flag as a match
            fprintf(fid,[eval(['Folder1FilesAll.',folder1Tests{testLoop,1},'{',num2str(sameIA(scriptLoop)),',1};']),' - Exact match\n']);
        end;
    end;
    trackTotalFiles = numel(sameIA) + trackDNE;
    trackFailed = trackTotalFiles - (trackMatch + trackLDM + trackDNE);
    
    % Report Summary results
    disp([folder1Tests{testLoop,1},' Results'])
    disp('----------------------------------------------------');
    disp([num2str(trackDNE),' out of ',num2str(trackTotalFiles),' unique test reports did not exist in both folders']);
    disp([num2str(trackLDM),' out of ',num2str(trackTotalFiles),' unique test reports existed in both folders but have a different number of lines']);
    disp([num2str(trackFailed),' out of ',num2str(trackTotalFiles),' unique test reports existed in both folders but some lines did not match']);
    disp(' ');
    
    fprintf(fid,'\nSummary\n');
    fprintf(fid,[num2str(trackDNE),' out of ',num2str(trackTotalFiles),' unique test reports did not exist in both folders\n']);
    fprintf(fid,[num2str(trackLDM),' out of ',num2str(trackTotalFiles),' unique test reports existed in both folders but have a different number of lines\n']);
    fprintf(fid,[num2str(trackFailed),' out of ',num2str(trackTotalFiles),' unique test reports existed in both folders but some lines did not match\n']);
    fprintf(fid,'---------------------------\n\n');
    fclose(fid);
end;