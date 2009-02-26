function [normMat_Tool1_Tool2,maxNorm,storePosError,storeVelError,currCase,max5,max5loc] = normNmaxData(Tool21_rows,diffMat_Tool1_Tool2,columnSize,allLoopChoice,reportChoiceTool2,charLoc1Tool2,reportFilesTool2,currTool1choice)

% ============== Normalize Cases for Tool1/Tool2 Comparison =================
for normRow = 1:Tool21_rows
    normMat_Tool1_Tool2(normRow,1) = diffMat_Tool1_Tool2(normRow,1);
    normMat_Tool1_Tool2(normRow,2) = norm(diffMat_Tool1_Tool2(normRow,2:(columnSize-3)));
    normMat_Tool1_Tool2(normRow,3) = norm(diffMat_Tool1_Tool2(normRow,(columnSize-2):columnSize));
end

% ========= Determine Max. Position & Velocity Normalization ============
temp = normMat_Tool1_Tool2(:,2);
for loop = 1:5
    [max5(loop),max5loc(loop)] = max(temp);
    temp(max5loc(loop)) = 0;
end
max5loc(6) = size(diffMat_Tool1_Tool2,1);
if size(normMat_Tool1_Tool2,1) ~= 1
    maxNorm      = max(normMat_Tool1_Tool2);
else
    maxNorm = normMat_Tool1_Tool2;
end
maxNorm(1,1) = 0;
storePosError{allLoopChoice} = maxNorm(1,2) * 1000;
storeVelError{allLoopChoice} = maxNorm(1,3) * 1000;
reportLengthTool2 = size(reportFilesTool2{reportChoiceTool2}) - charLoc1Tool2;
currCase = currTool1choice(1:(reportLengthTool2(2)-6));