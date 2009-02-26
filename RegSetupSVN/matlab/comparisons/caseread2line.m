function names = caseread(filename,endrow)
%CASEREAD Reads casenames from a file.
%   CASEREAD(FILENAME) returns a string matrix of names. FILENAME is 
%   the complete path to the desired file. CASEREAD expects one line per
%   case in the file.
%
%   CASEREAD without inputs displays the File Open dialog box allowing
%   interactive choice of the file.

%   B.A. Jones 2-1-95
%   Copyright 1993-2004 The MathWorks, Inc. 
% $Revision: 1.1 $  $Date: 2007/08/31 17:42:33 $
% Edited by E.Dove to include ability to save up to a user specifed
% number of rows

if nargin == 0
   if (ispc)
      filter = '*.*';
   else
      filter = '*';
   end
   [F,P]=uigetfile(filter);
   if (isequal(F,0)), return, end
   filename = [P,F];
end
fid = fopen(filename,'rt');

if fid == -1
   disp('Unable to open file.');
   return
end

bigM = fread(fid,Inf);

% Depending on where the file was created, the lines may be separated
% by carriage returns, line feeds, or both
havenewline = ismember(char([10 13]),bigM);
if all(havenewline)
   bigM(bigM==10) = [];
   lf = char(13);
elseif havenewline(1)
   lf = char(10);
else
   lf = char(13);
end

fclose(fid);
newlines = find(bigM == lf);
nobs = length(newlines);
if (nobs==0 | newlines(nobs)~=length(bigM))
   newlines = [newlines; (length(bigM)+1)];
   nobs = nobs+1;
end
startlines = newlines;
startlines(endrow) = [];
startlines = [0;startlines];
clength = newlines - startlines;
maxlength = max(clength)-1;
names = ' ';
names = names(ones(endrow,1),ones(maxlength,1));

for k = 1:endrow
    names(k,1:clength(k)-1) = setstr((bigM(startlines(k)+1:startlines(k)+clength(k)-1))');
end
