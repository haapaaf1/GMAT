function [reportLengthTool2,currTool1choice,currTool2choice,matchFound,charLoc1Tool2,reportFilesTool2,reportChoiceTool2] = checkTool2files_2(matchFound,Tool2Dir,Tool1,Tool2,Tool2Folder,reportLengthTool1,reportChoiceTool1,reportFilesTool1,nameFile)

% Check for relevant Tool2 report files in the correct directory
cd(Tool2Dir);
countReport = 0;
txtFilesTool2 = dir('*.report');
numTxtsTool2 = size(txtFilesTool2,1);

if numTxtsTool2 ~= 0
    for loop=1:numTxtsTool2
        if findstr(txtFilesTool2(loop).name,nameFile) == 1
            countReport = countReport + 1;
            reportFilesTool2{countReport,1} = txtFilesTool2(loop).name;
        end
    end
    if (loop == numTxtsTool2) & (countReport == 0)
        disp(' ');
        disp('There are no relevant report files in the current directory. Check for report files');
        disp(['in the following folder: ',Tool2Dir]);
        disp('Then try re-running this script');
        reportFilesTool2 = {};
        reportLengthTool2 = 0;
        currTool1choice = 0;
        currTool2choice = 0;
        charLoc1Tool2 = 0;
        reportChoiceTool2 = 0;
        pause;
        return
    end
else
    disp(' ');
    disp('There are no relevant report files in the current directory. Check for report files');
    disp(['in the following folder: ',Tool2Dir]);
    disp('Then try re-running this script');
    reportFilesTool2 = {};
    reportLengthTool2 = 0;
    currTool1choice = 0;
    currTool2choice = 0;
    charLoc1Tool2 = 0;
    reportChoiceTool2 = 0;
    pause;
    return
end

for loop = 1:countReport
    reportLengthTool2 = size(reportFilesTool2{loop},2);
    charLoc1Tool1 = size(Tool1,2)+size(nameFile,2)+3; % Tool1 character location in report file after tool name
    comparePos2 = reportLengthTool1 - charLoc1Tool1; % report size from num above to last character
    charLoc1Tool2 = size(Tool2,2)+size(nameFile,2)+3; % Tool2 character location in report file after tool name

    currTool1choice = reportFilesTool1{reportChoiceTool1}(charLoc1Tool1:reportLengthTool1);
    currTool2choice = reportFilesTool2{loop}(charLoc1Tool2:reportLengthTool2);

    if size(currTool1choice,2) == size(currTool2choice,2)
        if currTool1choice == currTool2choice
            matchFound = 1;
            reportChoiceTool2 = loop;
        elseif (matchFound == 0) & (loop == countReport)
            disp(' ')
            disp(['No match found in the ',Tool2Folder,' folder. Check to make sure the following case is present in the ',Tool2Folder,' folder:']);
            disp([nameFile,'_[ToolFolder]_',currTool1choice]);
            disp(' ');
            reportChoiceTool2 = 0;
            return
        end
    elseif (matchFound == 0) & (loop == countReport)
        disp(' ')
        disp(['No match found in the ',Tool2Folder,' folder. Check to make sure the following case is present in the ',Tool2Folder,' folder:']);
        disp([nameFile,'_[ToolFolder]_',currTool1choice]);
        disp(' ');
        reportChoiceTool2 = 0;
        return
    end
end