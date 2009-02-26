function [runExcelFlag] = askExcel

% ############### Prompt user on running Excel #######################

disp(' ');
disp('Would you like to generate an Excel spreadsheet with comparison results? (Y or N)');
disp('[Excel must be installed on your machine to create a spreadsheet]');

% User inputs value. Program warns the user of any incorrectly entered value
error = 1;
runExcel = input('Choice: ','s');
disp(' ');
while error == 1
    if strcmp(lower(runExcel),'y')
        runExcelFlag = 1;
        error = 0;
    elseif strcmp(lower(runExcel),'n')
        runExcelFlag = 0;
        error = 0;
    else
        disp('You have entered an incorrect value. Please try again.');
        runExcel = input('Choice: ','s');
        disp(' ');
    end
end