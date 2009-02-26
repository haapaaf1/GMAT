function [maxDiffs,diffMat_Tool1_Tool2] = find360nAbs(diffMat_Tool1_Tool2,Tool21_rows)

[R,C]=find(abs(round(diffMat_Tool1_Tool2))==360); % Find values in the difference matrix that equal 360 deg
if (isempty(C) ~= 1) & (isempty(R) ~= 1)
    for loop = 1:size(R,1)
        diffMat_Tool1_Tool2(R(loop),C(loop)) = 0;
    end
end

if Tool21_rows > 1
    maxDiffs      = max(abs(diffMat_Tool1_Tool2));
else
    maxDiffs      = abs(diffMat_Tool1_Tool2);
end
maxDiffs(1,1) = 0;