function [] = saveExcel_3(runExcelFlag,e,eSheets,sheetExists,eWorkbooks,maxDiffs2XL,DataDir,Tool1,Tool2,Tool1Folder,Tool2Folder)

% ======================== Save data into Excel ========================
eSheet3 = eSheets.get('Item', 3); %Sets an interface with the third sheet in the workbook
eSheet3.Activate; %Activates the sheet previously set to eSheet3

[rows,cols]=size(maxDiffs2XL); %finds the number of rows and columns of C
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
e.Activesheet.UsedRange.Clear; %Clears the current sheet of any values
eActivesheetRange.Value = maxDiffs2XL; %Sends the data in the matix on the active sheet in the workbook
eActivesheetRange.Cells.EntireColumn.AutoFit; %Resizes the Columns to fit the data
eSheet3.set('name',[Tool1,'_',Tool2,'_Cb']) %Names the sheet
try, % If the user chooses not to overwrite an excel file w/ the same name an error occurs.
    if sheetExists == 1
        invoke(eWorkbooks,'Save')
    elseif sheetExists == 0
        SaveAs(eWorkbooks, [DataDir,'/',Tool1Folder,'_',Tool2Folder,'_Results_',date,'.xls']); %saves the workbook in current directory
    end
catch,
    disp(' ');
    disp('You either opted to not overwrite the Excel spreadsheet with the comparison data');
    disp('or an error occured during the saving of the Excel spreadsheet.');
    disp('The script must be re-run if you wish to save the comparison data to Excel');
end
e.Quit; %quits from the current excel spreadsheet that is open
e.delete; %deletes the active x connection with excel
% ======================================================================