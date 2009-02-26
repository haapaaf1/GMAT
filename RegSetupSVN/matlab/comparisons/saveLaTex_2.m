function [] = saveLaTex_2(maxDiffs2XL,mainDir,Tool1Folder,Tool2Folder,scriptDir,Tool2Dir,saveDir)

texTool1Folder = Tool1Folder;
texTool2Folder = Tool2Folder;
if isempty(strfind(lower(Tool1Folder),'macgmat')) & isempty(strfind(lower(Tool1Folder),'linuxgmat')) & (isempty(strfind(lower(Tool1Folder),'gmat')) == 0);
    texTool1Folder = [Tool1Folder(1:strfind(lower(Tool1Folder),'gmat')-1),'WinGMAT'];
end;
if isempty(strfind(lower(Tool2Folder),'macgmat')) & isempty(strfind(lower(Tool2Folder),'linuxgmat')) & (isempty(strfind(lower(Tool2Folder),'gmat')) == 0);
    texTool2Folder = [Tool2Folder(1:strfind(lower(Tool2Folder),'gmat')-1),'WinGMAT'];
end;

% ======== Format data for exporting into a LaTeX document =========
Data{1}      = {};
Data{2}      = {};
Data{3}      = {};
Data{4}      = {};
Data{5}      = {};
RowLabels{1} = {};
for testCase = 1:(size(maxDiffs2XL,1)-1);
    for loop = 1:5
        % ==== Convert underscores to dashes for the current case name, since Latex can't hande underscores
        underLoc    = findstr(char(maxDiffs2XL(testCase+1,1)),'_');

        if size(underLoc,2) == 1; % Check to see if only one underscore appears in case name
            currCase2 = [maxDiffs2XL{testCase+1,1}(1,1:underLoc(1)-1),'-',maxDiffs2XL{testCase+1,1}(1,underLoc(1)+1:size(maxDiffs2XL{testCase+1,1},2))];
        elseif size(underLoc,2) == 0; % Check to see if no underscores appears in case name
            currCase2 = char(maxDiffs2XL(testCase+1,1));
        else; % Check to see if more than one underscore appears in case name
            currCase2 = [maxDiffs2XL{testCase+1,1}(1,1:underLoc(1)-1),'-'];
            for innerLoop = 1:size(underLoc,2)
                if innerLoop == size(underLoc,2)
                    currCase2 = [currCase2,maxDiffs2XL{testCase+1,1}(1,underLoc(innerLoop)+1:size(maxDiffs2XL{testCase+1,1},2))];
                else
                    currCase2 = [currCase2,maxDiffs2XL{testCase+1,1}(1,underLoc(innerLoop)+1:underLoc(innerLoop+1)-1),'-'];
                end
            end
        end
        % ================================================================
        digits      = 10;
        RowLabels{testCase,1} = currCase2;

        switch loop
            case 1
                ColumnLabels{1,loop} = {'Test Case' 'X-Pos (m)' 'Y-Pos (m)' 'Z-Pos (m)'}.';
                Data{loop}    = [Data{loop};[maxDiffs2XL(testCase+1,2),maxDiffs2XL(testCase+1,3),maxDiffs2XL(testCase+1,4)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Coordinate System Dependent Parameter Differences (Position)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CSParams-Pos.tex'];
            case 2
                ColumnLabels{1,loop} = {'Test Case' 'X-Vel (m/s)', 'Y-Vel (m/s)', 'Z-Vel (m/s)'}.';
                Data{loop} = [Data{loop};[maxDiffs2XL(testCase+1,5),maxDiffs2XL(testCase+1,6),maxDiffs2XL(testCase+1,7)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Coordinate System Dependent Parameter Differences (Velocity)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CSParams-Vel.tex'];
            case 3
                ColumnLabels{1,loop} = {'Test Case' 'X-(H) ($m^2/sec$)' 'Y-(H) ($m^2/sec$)' 'Z-(H) ($m^2/sec$)'}.';
                Data{loop} = [Data{loop};[maxDiffs2XL(testCase+1,10),maxDiffs2XL(testCase+1,11),maxDiffs2XL(testCase+1,12)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Coordinate System Dependent Parameter Differences (Specific Angular Momentum)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CSParams-H.tex'];
            case 4
                ColumnLabels{1,loop} = {'Test Case' 'Mag-Vel (m/s)' 'Right Asc. of Vel. (deg)' 'Dec. of Vel. (deg)'}.';
                Data{loop} = [Data{loop};[maxDiffs2XL(testCase+1,8),maxDiffs2XL(testCase+1,9),maxDiffs2XL(testCase+1,15)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Coordinate System Dependent Parameter Differences (Velocity Vector-based)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CSParams-VelVec.tex'];
            case 5
                ColumnLabels{1,loop} = {'Test Case' 'Arg. of Per. (deg)' 'Decl. (deg)' 'Inc. (deg)' 'RA (deg)' 'RAAN (deg)'}.';
                Data{loop} = [Data{loop};[maxDiffs2XL(testCase+1,13),maxDiffs2XL(testCase+1,14),maxDiffs2XL(testCase+1,16),maxDiffs2XL(testCase+1,17),maxDiffs2XL(testCase+1,18)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Coordinate System Dependent Parameter Differences (Angle-based)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CSParams-Angle.tex'];
        end
    end
end
for texLoop = 1:5
    Label       = ['Table: ',texTool1Folder,'-',texTool2Folder,' CS Parameters Set ',num2str(texLoop)];
    if texLoop == 5
        digits = 4;
    end
    cd(scriptDir);
    BasicLatexTable(cell2mat(Data{texLoop}), ColumnLabels{1,texLoop}, RowLabels, Caption{texLoop}, Label, digits, saveFile{texLoop}, saveDir)
    cd(Tool2Dir);
end
% =================================================================