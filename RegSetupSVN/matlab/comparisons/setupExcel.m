function [e,eSheets,sheetExists,eWorkbooks] = setupExcel(runExcelFlag,DataDir,Tool1Folder,Tool2Folder)

% ================ Setting up the Excel connection ==================
e = actxserver ('Excel.Application'); %Opens Excel and connects to it
findExcel = dir([DataDir,'/',Tool1Folder,'_',Tool2Folder,'_Results_',date,'.xls']);
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
e.Visible = 1; %Makes Excel "visible" instead of just running in the background
eSheets = e.ActiveWorkBook.Sheets; %Sets the interface with the sheets
% ===================================================================