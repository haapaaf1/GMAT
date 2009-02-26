function [menuSize,reportFilesTool1] = checkTool1files(Tool1Dir,Tool1,countReport,nameFile)

    % Check for Tool1 report files in the correct directory
    cd(Tool1Dir);
    txtFilesTool1 = dir('*.report');
    numTxtsTool1 = size(txtFilesTool1,1);
    
    if numTxtsTool1 ~= 0
        for loop=1:numTxtsTool1
            if findstr(txtFilesTool1(loop).name,nameFile) == 1
                countReport = countReport + 1;
                reportFilesTool1{countReport,1} = txtFilesTool1(loop).name;
            elseif (loop == numTxtsTool1) & (countReport == 0)
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
    menuSize = size(reportFilesTool1,1) + 1;