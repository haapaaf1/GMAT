function [Tool1Folder,Tool2Folder] = orderToolFolder(Tool1Folder,Tool2Folder)

% Order the tools so no repeats can occur (i.e. Having either GMAT_STK or STK_GMAT)
toolOrder = strvcat(Tool1Folder,Tool2Folder);
toolSort = sortrows(lower(toolOrder));

if isempty(strmatch(strcat(toolSort(1,:)),lower(Tool1Folder),'exact'))
    Tool1Folder = strcat(toolOrder(2,:));
    Tool2Folder = strcat(toolOrder(1,:));
else
    Tool1Folder = strcat(toolOrder(1,:));
    Tool2Folder = strcat(toolOrder(2,:));
end