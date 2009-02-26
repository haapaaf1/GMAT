function [countMatches,groupTraj] = saveLaTex_4(countTrajRun,groupTraj,countTraj,maxDiffs2XL,nameFile,mainDir,Tool1Folder,Tool2Folder,scriptDir,Tool2Dir,countMatches,saveFile,saveDir)

texTool1Folder = Tool1Folder;
texTool2Folder = Tool2Folder;
if isempty(strfind(lower(Tool1Folder),'macgmat')) & isempty(strfind(lower(Tool1Folder),'linuxgmat')) & (isempty(strfind(lower(Tool1Folder),'gmat')) == 0);
    texTool1Folder = [Tool1Folder(1:strfind(lower(Tool1Folder),'gmat')-1),'WinGMAT'];
end;
if isempty(strfind(lower(Tool2Folder),'macgmat')) & isempty(strfind(lower(Tool2Folder),'linuxgmat')) & (isempty(strfind(lower(Tool2Folder),'gmat')) == 0);
    texTool2Folder = [Tool2Folder(1:strfind(lower(Tool2Folder),'gmat')-1),'WinGMAT'];
end;

% ### Send variables to the function that creates LaTeX compatible text ###
groupTraj{countTraj,2}  = countTrajRun;

ColumnLabels = { 'Stopping Condition' 'Difference(s)'}.';
loopEnd      = groupTraj{countTraj,2};
Caption      = [texTool1Folder,'/',texTool2Folder,' ',groupTraj{countTraj,1},' ',nameFile,' Test Case Comparison'];
Label        = ['Table: ',texTool1Folder,'-',texTool2Folder,' ',groupTraj{countTraj,1},' ',nameFile,' Table'];
digits       = 10;

Data = cell2mat(maxDiffs2XL(countMatches+1:countMatches+loopEnd,2)); % Group all data related to the current trajectory
for loop = (countMatches+1):(countMatches+loopEnd)
    % ==== Convert underscores to dashes for the current case name, since Latex can't hande underscores
    prevCase = char(maxDiffs2XL(loop,1));
    underLoc  = findstr(prevCase,'_');
    
    if size(underLoc,2) == 1; % Check to see if only one underscore appears in case name
        prevCaseD = [prevCase(1:(underLoc(1)-1)),'-',prevCase((underLoc(1)+1):size(prevCase,2))];
    elseif size(underLoc,2) == 0; % Check to see if no underscores appears in case name
        prevCaseD = prevCase;
    else; % Check to see if more than one underscore appears in case name
        prevCaseD = [prevCase(1:(underLoc(1)-1)),'-'];
        for innerLoop = 1:size(underLoc,2)
            if innerLoop == size(underLoc,2)
                prevCaseD = [prevCaseD,prevCase((underLoc(size(underLoc,2))+1):size(prevCase,2))];
            else
                prevCaseD = [prevCaseD,prevCase((underLoc(innerLoop)+1):(underLoc(innerLoop+1)-1)),'-'];
            end
        end
    end
    % ================================================================

    RowLabels{loop-1,1} = [prevCaseD((size(groupTraj{countTraj,1},2)+2):size(prevCaseD,2))];
end

cd(scriptDir);
if isnumeric(Data)
    BasicLatexTable(Data, ColumnLabels, RowLabels(countMatches:countMatches+loopEnd-1), Caption, Label, digits, saveFile, saveDir)
else
    BasicLatexTable({Data}, ColumnLabels, RowLabels(countMatches:countMatches+loopEnd-1), Caption, Label, digits, saveFile, saveDir)
end
cd(Tool2Dir);
countMatches = countMatches + loopEnd;
% ##########################################################################