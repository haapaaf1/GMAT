function [rowStartTool1,rowStartTool2,Tool1,Tool2] = dataStart_2(Tool1Folder,Tool2Folder)

    % ##################################################################
    % ======= Determine which tools were selected for comparison =======
    % ============= and configure variables accordingly. ===============
    
    if isempty(findstr(lower(Tool1Folder),'gmat')) == 0
        rowStartTool1 = 1; % GMAT report's first row of numerical data
        Tool1 = 'GMAT';
    elseif isempty(findstr(lower(Tool1Folder),'stk')) == 0
        rowStartTool1 = 6; % STK report's first row of numerical data
        Tool1 = 'STK';
    elseif isempty(findstr(lower(Tool1Folder),'ff')) == 0
        rowStartTool1 = 3; % FF report's first row of numerical data
        Tool1 = 'FF';
    elseif isempty(findstr(lower(Tool1Folder),'od')) == 0
        rowStartTool1 = 0; % OD report's first row of numerical data
        Tool1 = 'OD';
    else
        rowStartTool1 = 0;
        Tool1 = Tool1Folder;
        disp(['Unknown tool selected (',Tool1Folder,'). The first row in the tool''s report file will be used.']);
        disp('** Warning ** This will result in an error if the first row isn''t numerical data.');
        disp(' ');
    end
    if isempty(findstr(lower(Tool2Folder),'gmat')) == 0
        rowStartTool2 = 1; % GMAT report's first row of numerical data
        Tool2 = 'GMAT';
    elseif isempty(findstr(lower(Tool2Folder),'stk')) == 0
        rowStartTool2 = 6; % STK report's first row of numerical data
        Tool2 = 'STK';
    elseif isempty(findstr(lower(Tool2Folder),'ff')) == 0
        rowStartTool2 = 3; % FF report's first row of numerical data
        Tool2 = 'FF';
    elseif isempty(findstr(lower(Tool2Folder),'od')) == 0
        rowStartTool2 = 0; % OD report's first row of numerical data
        Tool2 = 'OD';
    else
        rowStartTool2 = 0;
        Tool2 = Tool2Folder;
        disp(['Unknown tool selected (',Tool2Folder,'). The first row in the tool''s report file will be used.']);
        disp('** Warning ** This will result in an error if the first row isn''t numerical data.');
        disp(' ');
    end