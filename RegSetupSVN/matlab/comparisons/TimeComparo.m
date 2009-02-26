% TimeComparo Compares the performance time of select test cases by non-GMAT programs to GMAT
%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      10/04/2005      Original
%   E.Dove      10/03/2007      Last Modified
%   $Id: TimeComparo.m,v 1.4 2007/10/03 21:58:58 edove Exp $
%
%   See also BUILDRUN_SCRIPT_GMAT, STK_REPROPAGATE, STKAVGPERFORMANCE

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
    addpath(compareDir);

    disp('Welcome to the TimeComparo GMAT Performance Time Comparison Program.');
    disp('Please make sure Excel is closed before running this program');
    disp('Press ENTER to continue.');
    pause

    % ===================  Initialize variables ========================
    cd(compareDir);
    templateFile = [mainDir,'/output/AcceptTest/CompareResults/NonGMATrunTimes.xls'];
    DataDir = [mainDir,'/output/AcceptTest/CompareResults/Performance'];
    warning off
    mkdir(DataDir); % Creates a directory to save excel and matlab comparison data
    warning on

    % Open Excel Automation server
    e = actxserver('Excel.Application');
    Workbooks = e.Workbooks;

    e.Visible=1; % Make Excel visible

    % Open Excel file
    Workbook=Workbooks.Open(templateFile);

    % Specify sheet number, data, and range to write to
    sheetnum=1;
    range = 'A:E';

    % Make the first sheet active
    Sheets = e.ActiveWorkBook.Sheets;
    sheet1 = get(Sheets, 'Item', sheetnum);
    invoke(sheet1, 'Activate');
    Activesheet = e.Activesheet;

    % Reading the data from the sheet
    Range = get(Activesheet,'Range',range);
    out = Range.value;

    for loop = 1:size(out,1)
        if strcmp(num2str(cell2mat(out(loop,1))),num2str(NaN))
            lastDatarow = loop - 1;
            templateTimes = out(1:lastDatarow,1:size(out,2));
            break
        end
    end
    GMATtimes = templateTimes(:,1);
    GMATtimes(1,2) = {'GMAT Time to run (seconds)'};
    GMATtimes(1,3) = {'GMAT Integrator Step Sizes'};
    GMATtimes(:,4:(size(out,2)+2)) = templateTimes(:,2:size(out,2));
    GMATtimes(:,1) = cellstr(strvcat(GMATtimes(:,1))); % Eliminate extra spaces after test case filename

    % ########## Display the menu for times of test cases run ############
    disp(' ');
    disp('The following menu list dates where the BuildRun_Script_GMAT.m script was run using');
    disp('the all option. To create an Excel document with non-GMAT tools listed side-by-side');
    disp('with the GMAT Build used for the folowing dates choose the date of interest.');
    disp('====================================================================================');
    availTimes = dir([DataDir,'/*Time2RunAll_Performance.mat']);
    timeMenu = size(availTimes,1); % Total number of options user can choose from
    for timeLoop = 1:timeMenu
        chars(timeLoop) = findstr(availTimes(timeLoop).name,'_Time2RunAll_Performance') - 1;
        disp([num2str(timeLoop), '. ', availTimes(timeLoop).name(1:chars(timeLoop))]);
    end

    % User inputs value. Program warns the user of any incorrectly entered value
    error = 1;
    disp('====================================================================================');
    timeChoice = input('Choice: ');
    disp(' ');
    while error == 1
        error = 2;
        while error == 2
            if size(timeChoice,1) ~= 0
                break
            else
                error = 2;
                disp('You have entered an incorrect value. Please try again.');
                timeChoice = input('Choice: ');
                disp(' ');
            end
        end
        if timeChoice ~= [1:timeMenu]
            error = 1;
            disp('You have entered an incorrect value. Please try again.');
            timeChoice = input('Choice: ');
            disp(' ');
        else
            break
        end
    end
    Time1 = availTimes(timeChoice).name(1:chars(timeChoice));

    %Find an existing excel document: Open if it exists or create if it doesn't
    findExcel = dir([DataDir,'/','TimeComparo_',Time1,'.xls']);
    if size(findExcel,1) == 1
        eWorkbooks = e.Workbooks.Open([DataDir,'/',findExcel.name]); %Opens an existing workbook
        sheetExists = 1;
    elseif size(findExcel,1) == 0
        sheetExists = 0;
        eWorkbooks = e.Workbooks.Add; %Sets the interface with the workbook
    else
        disp('An error has occurred when searching for an existing Excel file');
        disp('Comparison data might not be saved in excel');
        sheetExists = 2;
    end

    % Open selected mat file and save data to an Excel spreadsheet
    open([DataDir,'/',availTimes(timeChoice).name]);
    timeList{1} = ans.timeList;
    
    findMat = findstr('.mat',availTimes(timeChoice).name);
    if exist([DataDir,'/',availTimes(timeChoice).name(1:findMat-1),'_5.mat'],'file') == 2 % Check to see if multiple performance runs were made
        for avgLoop = 2:5
            open([DataDir,'/',availTimes(timeChoice).name(1:findMat-1),'_',num2str(avgLoop),'.mat']);
            timeList{avgLoop} = ans.timeList;
        end
        
        for outerLoop = 2:size(GMATtimes,1)
            for innerLoop = 1:size(timeList{1},1)
                if strmatch(timeList{1}(innerLoop,1),GMATtimes(outerLoop,1))
                    GMATtimes(outerLoop,2) = {mean(cell2mat([timeList{1}(innerLoop,2),timeList{2}(innerLoop,2),timeList{3}(innerLoop,2),timeList{4}(innerLoop,2),timeList{5}(innerLoop,2)]))*60}; % Save GMAT time of run (in seconds)
                    GMATtimes(outerLoop,3) = {mean(cell2mat([timeList{2}(innerLoop,3),timeList{3}(innerLoop,3),timeList{4}(innerLoop,3),timeList{5}(innerLoop,3)]))}; % Save GMAT integrator step sizes
                end
            end
        end
    else % Only one or less than the selected amount of performance runs were made
        for outerLoop = 2:size(GMATtimes,1)
            for innerLoop = 1:size(timeList{1},1)
                if strmatch(timeList{1}(innerLoop,1),GMATtimes(outerLoop,1))
                    GMATtimes(outerLoop,2) = {cell2mat(timeList{1}(innerLoop,2))*60};
                    GMATtimes(outerLoop,3) = {cell2mat(timeList{1}(innerLoop,3))};
                end
            end
        end
    end
    
    % Reformat arrangment of saved data
    gmatNoDataLoc  = strmatch('1',num2str(cellfun('isempty',GMATtimes(:,2))));
    GMATtimes(gmatNoDataLoc,2) = {NaN}; % Add NaN to empty cells
    GMATtimes(gmatNoDataLoc,3) = {NaN}; % Add NaN to empty cells    
    AllTimes = GMATtimes(:,1);
    AllTimes(:,2)  = GMATtimes(:,2); % Store all runtimes together (GMAT)
    AllTimes(:,3)  = GMATtimes(:,4); % Store all runtimes together (STK)
    AllTimes(:,4)  = GMATtimes(:,6); % Store all runtimes together (FF)
    AllTimes(:,5)  = GMATtimes(:,3); % Store all integrator steps together (GMAT)
    AllTimes(:,6)  = GMATtimes(:,5); % Store all integrator steps together (STK)
    AllTimes(:,7)  = GMATtimes(:,7); % Store all integrator steps together (FF)
    AllTimes{1,8}  = 'GMAT (Time per Step Size)x10000';
    AllTimes{1,9}  = 'STK (Time per Step Size)x10000';
    AllTimes{1,10} = 'FF (Time per Step Size)x10000';
    AllTimes(2:size(GMATtimes,1),8)  = num2cell(cell2mat(GMATtimes(2:size(GMATtimes,1),2))./cell2mat(GMATtimes(2:size(GMATtimes,1),3))*10000); % (GMAT time per step)
    AllTimes(2:size(GMATtimes,1),9)  = num2cell(cell2mat(GMATtimes(2:size(GMATtimes,1),4))./cell2mat(GMATtimes(2:size(GMATtimes,1),5))*10000); % (STK time per step)
    AllTimes(2:size(GMATtimes,1),10) = num2cell(cell2mat(GMATtimes(2:size(GMATtimes,1),6))./cell2mat(GMATtimes(2:size(GMATtimes,1),7))*10000); % (FF time per step)
    % sprintf('\n') = Add line return
    AllTimes{1,11} = ['% GMAT/STK',sprintf('\n'),'diff. time to run']; % Numbers under 100% mean GMAT is doing better
    AllTimes{1,12} = ['% GMAT/STK',sprintf('\n'),'diff. Int. steps']; 
    AllTimes{1,13} = ['% GMAT/STK',sprintf('\n'),'Time per step']; 
    AllTimes{1,14} = ['% GMAT/FF',sprintf('\n'),'diff. time to run']; % Numbers under 100% mean GMAT is doing better
    AllTimes{1,15} = ['% GMAT/FF',sprintf('\n'),'diff. Int. steps']; 
    AllTimes{1,16} = ['% GMAT/FF',sprintf('\n'),'Time per step'];     
    AllTimes(2:size(GMATtimes,1),11) = num2cell(cell2mat(GMATtimes(2:size(GMATtimes,1),2))./cell2mat(GMATtimes(2:size(GMATtimes,1),4))*100); % (GMAT/STK runtime)
    AllTimes(2:size(GMATtimes,1),14) = num2cell(cell2mat(GMATtimes(2:size(GMATtimes,1),2))./cell2mat(GMATtimes(2:size(GMATtimes,1),6))*100); % (GMAT/FF runtime)
    
    AllTimes(2:size(GMATtimes,1),12) = num2cell(cell2mat(GMATtimes(2:size(GMATtimes,1),3))./cell2mat(GMATtimes(2:size(GMATtimes,1),5))*100); % (GMAT/STK int steps)
    AllTimes(2:size(GMATtimes,1),15) = num2cell(cell2mat(GMATtimes(2:size(GMATtimes,1),3))./cell2mat(GMATtimes(2:size(GMATtimes,1),7))*100); % (GMAT/FF int steps)
    
    AllTimes(2:size(GMATtimes,1),13) = num2cell(cell2mat(AllTimes(2:size(GMATtimes,1),8))./cell2mat(AllTimes(2:size(GMATtimes,1),9))*100); % (GMAT/STK time per step)
    AllTimes(2:size(GMATtimes,1),16) = num2cell(cell2mat(AllTimes(2:size(GMATtimes,1),8))./cell2mat(AllTimes(2:size(GMATtimes,1),10))*100); % (GMAT/FF time per step)
    
    % Initialize Latex related variables
%     ColumnLabels = GMATtimes(1,:).';
    Columns4Latex{1} = 1:4;
    Columns4Latex{2} = [1,8:10];
    Columns4Latex{3} = [1,11:13];
    Columns4Latex{4} = [1,14:16];
    for latexLoop = 1:4;
        ColumnLabels = [AllTimes(1,Columns4Latex{latexLoop})]';
        % need to automate this portion so script can handle labels with two lines or %
        if latexLoop == 1
            ColumnLabels = {'Test Case' 'GMAT TimeToRun(sec)' 'STK TimeToRun(sec)' 'FF TimeToRun(sec)'}';
        elseif latexLoop == 2
            ColumnLabels = {'Test Case' 'GMAT 10000xTime/Step' 'STK 10000xTime/Step' 'FF 10000xTime/Step'}';
        elseif (latexLoop == 3) | (latexLoop == 4);
            ColumnLabels = {'Test Case' '\\%% Time to Run' '\\%% Int. Steps' '\\%% Time Per Step'}';
        end
        perfRows     = strmatch('Performance',GMATtimes(:,1));
        Caption      = ['Performance Test Case Comparisons'];
        if latexLoop == 3;
            Caption      = ['GMAT/STK Performance Test Case Comparisons'];
        elseif latexLoop == 4;
            Caption      = ['GMAT/FF Performance Test Case Comparisons'];
        end
        Label        = ['Table: Performance',num2str(latexLoop)];
        digits       = 10;
        saveDir      = [cvsRootDir,'/GMATDocuments/AcceptTest'];
        Data         = AllTimes(perfRows,Columns4Latex{latexLoop}(2:size(Columns4Latex{latexLoop},2)));
        saveFile     = ['PerformanceTime',num2str(latexLoop),'.tex'];
        counter      = 1;
        num4Tex      = 45;

        for loop = perfRows(1):perfRows(size(perfRows,1))
            % ==== Convert underscores to dashes for the current case name, since Latex can't hande underscores
            prevCase = char(AllTimes(loop,1));
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
            % ================================================================

            RowLabels{counter,1} = [prevCaseD(size('Performance_GMAT_',2)+1:size(prevCaseD,2))];
            counter = counter + 1;
        end

        if counter > num4Tex
            for loop = 1:ceil(counter/num4Tex)
                if loop == ceil(counter/num4Tex)
                    RowLabels2 = RowLabels((loop-1)*num4Tex+1:size(perfRows,1));
                    Label2        = ['Table: Performance',num2str(latexLoop),'-',num2str(loop)];
                    saveFile     = ['PerformanceTime',num2str(latexLoop),'-',num2str(loop),'.tex'];
                    Data2         = Data((loop-1)*num4Tex+1:size(perfRows,1),1:size(Data,2));
                else
                    RowLabels2 = RowLabels((loop-1)*num4Tex+1:(loop)*num4Tex);
                    Label2        = ['Table: Performance',num2str(latexLoop),'-',num2str(loop)];
                    saveFile     = ['PerformanceTime',num2str(latexLoop),'-',num2str(loop),'.tex'];
                    Data2         = Data((loop-1)*num4Tex+1:(loop)*num4Tex,1:size(Data,2));
                end
                BasicLatexTable(Data2, ColumnLabels, RowLabels2, Caption, Label2, digits, saveFile, saveDir)
            end
        else
            BasicLatexTable(Data, ColumnLabels, RowLabels, Caption, Label, digits, saveFile, saveDir)
        end
    end
    % ============= Send data to Excel ===============
    % eWorkbooks = e.Workbooks.Add; %Sets the interface with the workbook
    eSheets = e.ActiveWorkBook.Sheets; %Sets the interface with the sheets
    eSheet1 = eSheets.get('Item', 1); %Sets an interface with the first sheet in the workbook
    eSheet1.Activate; %Activates the sheet previously set to eSheet1

    [rows,cols]=size(AllTimes); %finds the number of rows and columns of C
    col1=floor((cols-1)/26); %finds if there are 2 letters in the column index name
    if col1<1
        col=char(cols+64); %gives the letter associated with the number of columns
    else
        col2=mod(cols,26);
        col=[char(col1+64) char(col2+64)];%gives the letters associated with the number of columns
    end
    row=int2str(rows);
    rng=['A1:' col row];

    eActivesheetRange = e.Activesheet.get('Range', rng);
    eActivesheetRange.Value = AllTimes; %Sends the data in the matrix on the active sheet in the workbook
    eActivesheetRange.Cells.EntireColumn.AutoFit; %Resizes the Columns to fit the data
    % Color code cells based on GMAT doing better than the other tool
    % Yellow indicates GMAT is performing better in one or all of the following categories:
    % diff. time to run, diff. Int. steps, or Time per step"

    GMATbetter{1} = find([AllTimes{2:size(GMATtimes,1),11}]<100)+1; % time to run
    GMATbetter{2} = find([AllTimes{2:size(GMATtimes,1),12}]<100)+1; % Int. steps
    GMATbetter{3} = find([AllTimes{2:size(GMATtimes,1),13}]<100)+1; % Time per step
    
    for loop = 1:3;
        for innerLoop = GMATbetter{loop};
            rng = ['A',num2str(innerLoop)];
            eActivesheetRange = e.Activesheet.get('Range', rng);
            eActivesheetRange.Interior.Color = 65535;
            rng = [char((loop+10)+64),num2str(innerLoop)];
            eActivesheetRange = e.Activesheet.get('Range', rng);
            eActivesheetRange.Interior.Color = 65535;            
        end
    end;
    
    
    try, % If the user chooses not to overwrite an excel file w/ the same name an error occurs.
        if sheetExists == 1
            invoke(eWorkbooks,'Save')
            disp('The following file was saved into the comparison results folder:');
            disp(['TimeComparo_',Time1,'.xls']);
        elseif sheetExists == 0
            SaveAs(eWorkbooks, [DataDir,'/','TimeComparo_',Time1,'.xls']); %saves the workbook in current directory
            disp('The following file was saved into the comparison results folder:');
            disp(['TimeComparo_',Time1,'.xls']);
        end
    catch,
        disp(' ');
        disp('You either opted to not overwrite the Excel spreadsheet with the comparison data');
        disp('or an error occured during the saving of the Excel spreadsheet.');
        disp('The script must be re-run if you wish to save the comparison data to Excel');
    end

    e.Quit; %quits from the current excel spreadsheet that is open
    e.delete; %deletes the active x connection with excel
    % ================================================

    cd(compareDir);
    disp(' ');
    disp(['Press any alpha-numeric key and then ENTER to create another performance time comparison ']);
    disp(['or Press ENTER to exit.']);
    rerunScript = input('');
end