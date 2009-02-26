function BasicLatexTable(Data, ColumnLabels, RowLabels, Caption, Label, digits,saveFile,saveDir)

%  BasicLatexTable(Data, ColumnLabels, RowLabels, Caption, Label, digits )
%
%  Comment:  This function creates a basic latex table given a matrix of real
%  numbers that are the table values, the Column and Row labels.  Caption,
%  Label and digits are optional inputs and are explained below.  
%   
%  See below for more description.
%
%  Variable I/O
% --------------------------------------------------------
%  Variable Name        I/0     Type          Units        Dimens.    Description/Comments
%  Data                  I    cell array     arbitrary      mxn       The Data array is the data that is to appear
%                                                                     in the table. Currently, Data can be a matrix of real
%                                                                     numbers or a cell array with a combination of strings
%                                                                     or real numbers.
%
%  ColumnLabels         I    cell array        N/A         (m+1)x1    This is a cell array containing strings that
%                                                                     are the column labels.  The first element in the 
%                                                                     cell array contains the label for the row labels.
%
%  RowLabels            I    cell array        N/A         nx1        This is a cell array whose elements are strings that 
%                                                                     are the row labels for the table
%
%  Caption              I    string            N/A         N/A        This is an optional argument.  Is must be a string and
%                                                                     it will appear in the caption of the Latex table.
%
%  Label                I    string            N/A         N/A        This is an optional argument.  Is must be a string and
%                                                                     it will appear used inside the \label command to enable\
%                                                                     you to reference the table in Latex.
%
%  digits               I    real number       N/A         1x1        This is a real number that is the desired number of sig. figs
%                                                                     for the numerical data in the table
%
%  External References:  IndStr, contained at the bottom of this file
%
%  Modification History
%
%  07/15/05 - S. Hughes,    Created 
%  02/22/06 - E. Dove,      Added ability for Data variable to be a cell array
%           -       ,       Modified

cd(saveDir);
fid = fopen(saveFile,'w');

%  Determine the number of inputs and set some variables depending upon
%  what the user supplied
if nargin < 6
    digits = 5;
end

if nargin < 5
    Label = '';
end

if nargin < 4
    Caption = '';
end

%  Determine the number of rows and columns of data in the table
NumRows  = size(Data,1);
NumCols = size(Data,2);

%  Open the table environment
% disp('\begin{table}[htb]')
% disp('\centering')
fprintf(fid, '\\begin{table}[htbp!]\n');
fprintf(fid, '\\centering\n');

%  Write the caption if supplied
if ~isempty(Caption)
%     disp([ '\caption{ ' Caption '}']);
    fprintf(fid, [ '\\caption{ ' Caption '}\n']);
end

%  Calculate new indention
I = 1;  
Indstr = feval(@IndentStr, I); 

%  Create the string that chooses how to align data in the columns
CenterStr = '';
for i = 1:NumCols + 1;
    if i == 1
        CenterStr = [CenterStr 'l'];
    else 
        CenterStr = [CenterStr 'c'];
    end
end
I = I + 1;  
Indstr = feval(@IndentStr, I); 
CurrStr = [Indstr '\\begin{tabular}{' CenterStr '}' ];   
% disp(CurrStr); 
fprintf(fid, [CurrStr '\n']);
% disp([Indstr '\hline\hline'])
fprintf(fid, [Indstr '\\hline\\hline\n']);

%  Write the column labels
I = I + 1;  
Indstr = feval(@IndentStr, I); 
CurrStr = Indstr;
for i = 1:NumCols 
    CurrStr = [CurrStr ' ' ColumnLabels{i} ' &' ];
end
CurrStr = [CurrStr ' ' ColumnLabels{i+1} ' \\\\' ];
% disp(CurrStr); disp([Indstr '\hline'])
fprintf(fid, [CurrStr,'\n']);
fprintf(fid, [Indstr '\\hline\n']);

%  Write the data
for i = 1:NumRows;
    
    %  Indent and write the row label
    CurrStr = Indstr;
%     disp([Indstr '%---New Row---%'])
    fprintf(fid, [Indstr '%%---New Row---%%\n']);
    CurrStr = [CurrStr RowLabels{i} ' & '];
    
    for j = 1:NumCols-1
        if iscellstr(Data(i,j))
            CurrStr = [CurrStr char(Data{i,j}(1:size(Data{i,j},2))) ' & '];
        else
            if iscell(Data(i,j))
                CurrStr = [CurrStr num2str(cell2mat(Data(i,j)),digits) ' & '];
            else
                CurrStr = [CurrStr num2str(Data(i,j),digits) ' & '];
            end
        end
    end
    
    if isempty(j)
        j = 0;
    end
    
    if iscellstr(Data(i,j+1))
        CurrStr = [CurrStr char(Data{i,j+1}(1:size(Data{i,j+1},2))) ' \\\\'];
    else
        if iscell(Data(i,j+1))
            CurrStr = [CurrStr num2str(cell2mat(Data(i,j+1)),digits) ' \\\\'];
        else
            CurrStr = [CurrStr num2str(Data(i,j+1),digits) ' \\\\'];
        end
    end
%     disp(CurrStr);
    fprintf(fid, [CurrStr,'\n']);
end
I = I - 1; Indstr = feval(@IndentStr, I); 

%  write the closing horizontal lines
% disp( [Indstr  '\hline\hline' ] )
fprintf(fid, [Indstr  '\\hline\\hline\n' ]);

%  Label the table
if ~isempty(Label)
%     disp([Indstr '\label{' Label '} ']);
    fprintf(fid, [Indstr '\\label{' Label '} \n']);
end

%  close the environments
% disp(   '\end{tabular}'  )
fprintf(fid, '\\end{tabular}\n');
I = I - 1; Indstr = feval(@IndentStr, I); 
% disp(   '\end{table}'   )
fprintf(fid, '\\end{table}\n');
fclose(fid);

%  This function determines what the indention length should be
function Indstr = IndentStr(I)

Indstr = '';

for i = 1:3*I
    Indstr = [Indstr ' '];
end
