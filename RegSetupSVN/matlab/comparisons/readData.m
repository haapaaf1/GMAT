function [Tool21_rows,diffMat_Tool1_Tool2,mat_Tool21,mat_Tool11] = readData(reportChoiceTool1,columnSize,rowStartTool1,rowStartTool2,Tool1Dir,Tool2Dir,reportFilesTool1,reportFilesTool2,reportChoiceTool2,matchChoicesT1,matchChoicesT2,Tool2Folder,Tool1Folder)

% ======= Read numerical data from Tool2 and Tool1 ASCII Report Files =======
% Read Tool1 output file
file_Tool11      = [Tool1Dir,'/',reportFilesTool1{reportChoiceTool1}];
mat_Tool11       = zeros(10000,columnSize);
warning off;
mat_Tool11       = dlmread(file_Tool11,'\s',rowStartTool1,0);
warning on;
Tool11_rows      = size(mat_Tool11,1);

% Read Tool2 output file
file_Tool21     = [Tool2Dir,'/',reportFilesTool2{reportChoiceTool2}];
mat_Tool21      = zeros(10000,columnSize);
warning off;
mat_Tool21      = dlmread(file_Tool21,'\s',rowStartTool2,0);
warning on;
Tool21_rows     = size(mat_Tool21,1);
mat_Tool21(:,1)  = zeros(Tool21_rows,1);

% ============== Determine Diferences for Tool1/Tool2 Comparison =================
if Tool21_rows == Tool11_rows
    diffMat_Tool1_Tool2 = zeros(Tool21_rows,columnSize);
    if Tool11_rows ~= 1
        mat_Tool11(2:Tool11_rows,1) = round((mat_Tool11(2:Tool11_rows,1) - mat_Tool11(1,1))*86400); % Convert first column from Days to Seconds
    end 
    mat_Tool11(1,1) = 0;
    matchChoicesT1 = [matchChoicesT1, reportChoiceTool1];
    matchChoicesT2 = [matchChoicesT2, reportChoiceTool2];
else
    disp(' ');
    disp(['Report data between a ',Tool2Folder,' and ',Tool1Folder,' file do not match.']);
    disp('Solve this problem and then rerun this script');
    disp(['Check the following file (or the ',Tool1Folder,' equivalent): ']);
    disp(file_Tool21);
    diffMat_Tool1_Tool2 = {};
    pause;
    return
end
diffMat_Tool1_Tool2(:,1:columnSize) = mat_Tool11(:,1:columnSize) - mat_Tool21(:,1:columnSize);