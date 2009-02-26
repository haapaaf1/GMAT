function [reportChoiceTool1] = dispReportMenu1(Tool1Folder,Tool2Folder,reportFilesTool1)

% Display Tool1 menu of reports
disp(' ');
disp(['Choose the ',Tool1Folder,' report to compare to ',Tool2Folder]);
disp('(Enter the number for the desired report)');
disp('=========================================');
reportNum = size(reportFilesTool1,1) + 1; % Total number of options user can choose from
for reportLoop = 1:reportNum
    if reportLoop ~= reportNum
        disp([num2str(reportLoop), '. ', reportFilesTool1{reportLoop}]);
    else
        disp([num2str(reportLoop), '. ','Compare all report files.']);
    end
end

% User inputs value. Program warns the user of any incorrectly entered value
error = 1;
disp('-----------------------------------------');
reportChoiceTool1 = input('Choice: ');
disp(' ');
while error == 1
    error = 2;
    while error == 2
        if size(reportChoiceTool1,1) ~= 0
            break
        else
            error = 2;
            disp('You have entered an incorrect value. Please try again.');
            reportChoiceTool1 = input('Choice: ');
            disp(' ');
        end
    end
    if reportChoiceTool1 ~= [1:reportNum]
        error = 1;
        disp('You have entered an incorrect value. Please try again.');
        reportChoiceTool1 = input('Choice: ');
        disp(' ');
    else
        break
    end
end