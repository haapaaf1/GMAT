%   REVISION HISTORY
%   $Id: LoopTestSummary.m,v 1.3 2007/05/04 20:51:43 edove Exp $
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      07/26/2006      Original
%   E.Dove      05/04/2007      Last Modified
%   E.Dove      05/06/2009      Changed location folder to save Latex files

clc
error = 1;

rerunScript = 1;
while size(rerunScript,1) ~= 0
    clc
    clear all
    close all
    rerunScript = 1;
    format long
    tempDir = mfilename('fullpath');
    if ispc
        temp = findstr(tempDir,'\');
        compareDir = tempDir(1:temp(size(temp,2))-1);
    else
        temp = findstr(tempDir,'/');
        compareDir = tempDir(1:temp(size(temp,2))-1);
    end
    cd(compareDir);
    cd('..');
    cd('..');
    mainDir = cd;
    cd('..');
    cvsRootDir = cd;
    AcceptTestDir = [cvsRootDir,'/docs/AcceptTest'];
    cd(compareDir);
    
    % ===================  Initialize variables ========================
    nameFile        = 'Loop';
    char1           = ' ';
    char2           = ' ';
    testType        = 0; % 0 = no LHS/RHS 1 = one LHS/RHS | 2 = two LHS/RHS
    reportRows      = 0;
    ranOK_col       = 0;
    fileExtLoc      = 0;
    LHSRHS_options  = {'number', 'variable', 'array', 'SC non-time parameter', 'SC time parameter'};
    ansFlag_msgs    = {'Incorrect loop execution', 'Loop execution failed', 'Correct loop execution'};
    LoopSummary     = {'TestName', 'Pass/Fail', 'Failed/TotalTests'};
    Tool1Dir        = [mainDir,'/output/AcceptTest/GMAT_reports'];
    countReport     = 0;
    disp(' ');
    disp('Select a directory to save the Acceptance Test Latex files to.');
    disp('(If you have access to the CVS controlled GMATDocuments repository,');
    disp('select the AcceptTest folder inside of that repository.)');
 
    if exist(AcceptTestDir,'dir')==7;
        saveDir = uigetdir(AcceptTestDir,'Select Directory to save Latex files to');
    else;
        saveDir = uigetdir(cd,'Select Directory to save Latex files to');
    end;
    
    while saveDir == 0; % Makes sure the user selects a directory
        if exist(AcceptTestDir,'dir')==7;
            saveDir = uigetdir(AcceptTestDir,'Select Directory to save Latex files to');
        else;
            saveDir = uigetdir(cd,'Select Directory to save Latex files to');
        end;
    end;
%     DataDir         = [mainDir,'/output/AcceptTest/CompareResults/LoopTests'];    
%     warning off
%     mkdir(DataDir); % Creates a directory to save excel and matlab comparison data
%     warning on
    % ==================================================================
    
    % ====== Check for Loop report files in the correct directory ======
    cd(Tool1Dir);
    txtFilesTool1 = dir('*.report');
    numTxtsTool1 = size(txtFilesTool1,1);
    
    if numTxtsTool1 ~= 0
        for loop=1:numTxtsTool1
            if findstr(txtFilesTool1(loop).name,nameFile) == 1
                countReport = countReport + 1;
                reportFilesTool1{countReport,1} = txtFilesTool1(loop).name;
            elseif (loop == numTxtsTool1)  & (countReport == 0)
                disp(' ');
                disp('There are no relevant report files in the current directory. Check for report files');
                disp(['in the following folder: ',Tool1Dir]);
                disp('Then try re-running this script');
                reportFilesTool1 = {};
                menuSize = 0;
                pause;
                return
            end
        end
    else
        disp(' ');
        disp('There are no relevant report files in the current directory. Check for report files');
        disp(['in the following folder: ',Tool1Dir]);
        disp('Then try re-running this script');
        reportFilesTool1 = {};
        menuSize = 0;
        pause;
        return
    end
    
    reportNum = size(reportFilesTool1,1); % Total number of reports
    
    fprintf('\nLoop Test Error Report Summary\n')
    fprintf('==================================\n')
    fprintf('[Some loop tests contain a left & right hand side(LHS&RHS)\n')
    fprintf(' of the condition. For example, If LHS > RHS.\n')
    fprintf(' Two loop types in the name mean the second loop is nested inside\n')
    fprintf(' the first loop. For example, ForIf is an if inside a For]\n')
    
    for allLoopChoice = 1:reportNum
        headerCol       = 1;
        headerRow       = 1;
        headerFlag      = 0; % 0 represents space in report header line
        errorFlag       = 0; % 0 = no errors for loop test | 1 = errors in loop test
        prevType        = -10000;
        currType        = 0;
        errorCount      = 0;
        disp(' ');

        reportChoiceTool1 = allLoopChoice;
        fileExtLoc = findstr(reportFilesTool1{reportChoiceTool1},'.report');
        
        if isempty(str2num(reportFilesTool1{reportChoiceTool1}(fileExtLoc-1))) % if report name contains no ##'s at the end
            testType = 0;
            testName = reportFilesTool1{reportChoiceTool1}(11:fileExtLoc-1);
            disp(['--------- ',testName,' Loop Tests ---------']);
            disp(['Filename: ',reportFilesTool1{reportChoiceTool1}(1:fileExtLoc-1)])
            disp(['LHS/RHS: N/A'])
            disp(' ');
        elseif isempty(str2num(reportFilesTool1{reportChoiceTool1}(fileExtLoc-4))) % if report name contains ## at the end
            testType = 1;
            testName = reportFilesTool1{reportChoiceTool1}(11:fileExtLoc-3);
            disp(['--------- ',testName,' Loop Tests ---------']);
            disp(['Filename: ',reportFilesTool1{reportChoiceTool1}(1:fileExtLoc-1)])
            disp(['LHS: ',char(LHSRHS_options(str2num(reportFilesTool1{reportChoiceTool1}(fileExtLoc-2))))]);
            disp(['RHS: ',char(LHSRHS_options(str2num(reportFilesTool1{reportChoiceTool1}(fileExtLoc-1))))]);
            disp(' ');
        else % report name contains ##_## at the end
            testType = 2;
            testName = reportFilesTool1{reportChoiceTool1}(11:fileExtLoc-6);
            disp(['--------- ',testName,' Loop Tests ---------']);
            disp(['Filename: ',reportFilesTool1{reportChoiceTool1}(1:fileExtLoc-1)])
            disp(['LHS: ',char(LHSRHS_options(str2num(reportFilesTool1{reportChoiceTool1}(fileExtLoc-2))))]);
            disp(['RHS: ',char(LHSRHS_options(str2num(reportFilesTool1{reportChoiceTool1}(fileExtLoc-1))))]);
            disp(' ');
        end
            
        % ======= Read numerical data from ASCII Loop Report files =======
        file_Tool1      = [Tool1Dir,'/',reportFilesTool1{reportChoiceTool1}];
        try;
            warning off;
            mat_Tool1       = dlmread(file_Tool1,'\s',1,0);
            warning on;
        
            % Go to next loop if no data in file
            if size(mat_Tool1,1) < 2;
                disp('No data or invalid header found in this file.');
            else;
                fid = fopen(file_Tool1);
                mat_Tool1headerFull = fgetl(fid);
                fclose(fid);
                mat_Tool1headerFull = strcat(mat_Tool1headerFull);

                headerLoopEnd   = size(mat_Tool1headerFull,2);

                % Convert header string to cell string array of rows
                for headerLoop = 2:headerLoopEnd
                    char1 = mat_Tool1headerFull(headerLoop-1);
                    char2 = mat_Tool1headerFull(headerLoop);

                    if strcmp(' ',char1) & strcmp(' ',char2)
                        if headerFlag == 0
                            headerFlag = 1;

                            mat_Tool1header{headerRow} = mat_Tool1headerFull(headerCol:headerLoop-2);
                            headerRow = headerRow + 1;
                        end
                    elseif strcmp(' ',char1) & (isspace(char2) == 0)
                        headerCol = headerLoop;
                    elseif (headerFlag == 0) & (headerLoopEnd == headerLoop)
                        mat_Tool1header{headerRow} = mat_Tool1headerFull(headerCol:headerLoopEnd);
                    else
                        headerFlag = 0;
                    end
                end

                % Scan report numerical data for loop summary creation
                find_ranOK = strfind(mat_Tool1header,'ranOK');
                find_ansFlag = strfind(mat_Tool1header,'ansFlag');
            end;

            reportRows = size(mat_Tool1,1);
            if (reportRows < 500) & (reportRows > 0)
                if isempty(cell2mat(find_ranOK))
                    for loop = 1:size(mat_Tool1header,2)
                        if isempty(find_ansFlag{loop}) == 0
                            ansFlag_col = loop;
                        end
                    end

                    for loop = 1:reportRows
                        currType = mat_Tool1(loop,1);
                        if mat_Tool1(loop,ansFlag_col) <= 0
                            errorFlag = 1;
                            if mat_Tool1(loop,ansFlag_col) == -99
                                disp([ansFlag_msgs{1},' in type ',num2str(mat_Tool1(loop,1)),' section']);
                            elseif mat_Tool1(loop,ansFlag_col) == 0
                                disp([ansFlag_msgs{2},' in type ',num2str(mat_Tool1(loop,1)),' section']);
                            end
                            if prevType ~= currType
                                errorCount = errorCount + 1;
                                PassFail = 'Fail';
                                prevType = currType;
                            end
                        elseif (loop == reportRows) & (errorFlag == 0)
                            disp('No errors found in this test');
                            if prevType ~= currType
                                PassFail = 'Pass';
                                prevType = currType;
                            end
                        end
                    end
                    LoopSummary = [LoopSummary; {reportFilesTool1{reportChoiceTool1}(1:fileExtLoc-1),PassFail,[num2str(errorCount),'/',num2str(currType)]}];
                else
                    for loop = 1:size(mat_Tool1header,2)
                        if isempty(find_ranOK{loop}) == 0
                            ranOK_col = loop;
                        elseif isempty(find_ansFlag{loop}) == 0
                            ansFlag_col = loop;
                        end
                    end

                    for loop = 1:reportRows
                        currType = mat_Tool1(loop,1);
                        if mat_Tool1(loop,ranOK_col) <= 0
                            errorFlag = 1;
                            if mat_Tool1(loop,ansFlag_col) == -99
                                disp([ansFlag_msgs{1},' in type ',num2str(mat_Tool1(loop,1)),' section of script']);
                            elseif mat_Tool1(loop,ansFlag_col) == 0
                                disp([ansFlag_msgs{2},' in type ',num2str(mat_Tool1(loop,1)),' section of script']);
                            elseif mat_Tool1(loop,ansFlag_col) == 1
                                disp([ansFlag_msgs{3},' in type ',num2str(mat_Tool1(loop,1)),' section of script']);
                            end
                            if prevType ~= currType
                                errorCount = errorCount + 1;
                                PassFail = 'Fail';
                                prevType = currType;
                            end
                        elseif (loop == reportRows) & (errorFlag == 0)
                            disp('No errors found in this test');
                            if prevType ~= currType
                                PassFail = 'Pass';
                                prevType = currType;
                            end
                        end
                    end
                    LoopSummary = [LoopSummary; {reportFilesTool1{reportChoiceTool1}(1:fileExtLoc-1),PassFail,[num2str(errorCount),'/',num2str(currType)]}];
                end
            elseif (reportRows > 500);
                disp(['Possible infinite loop bug in type ',num2str(mat_Tool1(reportRows,1)),' section']);
            else;
                PassFail = 'Fail';
                LoopSummary = [LoopSummary; {reportFilesTool1{reportChoiceTool1}(1:fileExtLoc-1),PassFail,[num2str(errorCount),'/',num2str(currType)]}];
            end;
        catch;
            disp('Unknown error found in while reading data');
            disp('Check to make sure data is valid');
            PassFail = 'Fail';
            LoopSummary = [LoopSummary; {reportFilesTool1{reportChoiceTool1}(1:fileExtLoc-1),PassFail,[num2str(errorCount),'/',num2str(currType)]}];
        end;
    end;
    disp(['---------------------------------------']);
    LoopSummary
    fprintf('==================================\n')
    
    % ====== Initialize Latex related variables ======
    ColumnLabels = LoopSummary(1,:).';
    perfRows     = strmatch('Loop',LoopSummary(:,1));
    Caption      = ['Loop Test Case Results'];
    Label        = ['Table: LoopResults'];
    digits       = 10;
    Data         = LoopSummary(perfRows,2:size(LoopSummary,2));
    saveFile     = 'LoopResults.tex';
    counter      = 1;
    num4Tex      = 45;
    
    for loop = perfRows(1):perfRows(size(perfRows,1))
        % Convert underscores to dashes for the current case name, since Latex can't hande underscores
        prevCase = char(LoopSummary(loop,1));
        underLoc  = findstr(prevCase,'_');

        if size(underLoc,2) == 1
            prevCaseD = [prevCase(1:(underLoc(1)-1)),'-',prevCase((underLoc(1)+1):size(prevCase,2))];
        else
            prevCaseD = [prevCase(1:(underLoc(1)-1)),'-'];
            for innerLoop = 1:size(underLoc,2)
                if innerLoop == size(underLoc,2)
                    prevCaseD = [prevCaseD,prevCase((underLoc(size(underLoc,2))+1):size(prevCase,2))];
                else
                    prevCaseD = [prevCaseD,prevCase((underLoc(innerLoop)+1):(underLoc(innerLoop+1)-1)),'-'];
                end
            end
        end
   
        RowLabels{counter,1} = [prevCaseD(size('Loop_GMAT_',2)+1:size(prevCaseD,2))];
        counter = counter + 1;
    end
    
    % Create LaTex document of loop test summary
    if counter > num4Tex
        for loop = 1:ceil(counter/num4Tex)
            if loop == ceil(counter/num4Tex)
                RowLabels2 = RowLabels((loop-1)*num4Tex+1:size(perfRows,1));
                Label2        = ['Table: Performance',num2str(loop)];
                saveFile     = ['LoopResults-',num2str(loop),'.tex'];
                Data2         = Data((loop-1)*num4Tex+1:size(perfRows,1),1:size(Data,2));
            else
                RowLabels2 = RowLabels((loop-1)*num4Tex+1:(loop)*num4Tex);
                Label2        = ['Table: Performance',num2str(loop)];
                saveFile     = ['LoopResults-',num2str(loop),'.tex'];
                Data2         = Data((loop-1)*num4Tex+1:(loop)*num4Tex,1:size(Data,2));
            end
            BasicLatexTable(Data2, ColumnLabels, RowLabels2, Caption, Label2, digits, saveFile, saveDir)
        end
    else
        BasicLatexTable(Data, ColumnLabels, RowLabels, Caption, Label, digits, saveFile, saveDir)
    end

    cd(compareDir);
    disp(' ');
    disp(['Press any alpha-numeric key and then ENTER to rerun this comparison script or']);
    disp('Press ENTER to exit.');
    rerunScript = input('');
end