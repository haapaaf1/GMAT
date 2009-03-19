function GMAT2MATLAB_SC(SCgregorian,SCmodjulian,DataGregRpt,DataModJulRpt)

format long g

tempDir = mfilename('fullpath');

if ispc;
    temp = findstr(tempDir,'\');
    currDir = tempDir(1:temp(size(temp,2))-1);
else
    temp = findstr(tempDir,'/');
    currDir = tempDir(1:temp(size(temp,2))-1);
end;
cd(currDir)

cd ..
cd ..
rootDir = cd;
outputDir = [cd,'/output'];

cd(currDir)

% Process SC save using Gregorian for DateFormat
findSlashes   = findstr(DataGregRpt,'/');
lastSlash     = findSlashes(size(findSlashes,2)) + 1;
SCgregRptName = DataGregRpt(lastSlash:size(DataGregRpt,2));

% Load contents of report file into Matlab as a cell array
fid1 = fopen([rootDir,'/output/SystemTest/',SCgregRptName]);
eval('CheckData2Matlab_SCgreg = textscan(fid1,''%s'',''delimiter'',''\n'',''whitespace'','''');'); % store data as a variable name
fclose(fid1);

sizeSCgregRpt = size(CheckData2Matlab_SCgreg{1,1},1);

% Append the data outputted from the GMAT object, in the form of a Matlab
% structure, to the Matlab cell array
CheckData2Matlab_SCgreg{1,1}{sizeSCgregRpt+1,1} = sprintf('%-30.16s %-30.16s %-30.16s %-30.16f %-30.16f %-30.16f %-30.16f %-30.16f %-30.16f',SCgregorian.Epoch,SCgregorian.Epoch,SCgregorian.Epoch,SCgregorian.X,SCgregorian.Y,SCgregorian.Z,SCgregorian.VX,SCgregorian.VY,SCgregorian.VZ);

% Replace existing contents of report file with the Matlab cell array data
fid1 = fopen([outputDir,'\SystemTest\',SCgregRptName],'w');
for loop = 1:sizeSCgregRpt+1;
    fprintf(fid1,'%s',CheckData2Matlab_SCgreg{1,1}{loop,1});
    if loop ~= sizeSCgregRpt+1;
        fprintf(fid1,'\n');
    end;
end;
fclose(fid1);


% Process SC save using Modified Julian for DateFormat
findSlashes   = findstr(DataModJulRpt,'/');
lastSlash     = findSlashes(size(findSlashes,2)) + 1;
SCmodJulRptName = DataModJulRpt(lastSlash:size(DataModJulRpt,2));

% Load contents of report file into Matlab as a cell array
fid2 = fopen([rootDir,'/output/SystemTest/',SCmodJulRptName]);
eval('CheckData2Matlab_SCmodJul = textscan(fid2,''%s'',''delimiter'',''\n'',''whitespace'','''');'); % store data as a variable name
fclose(fid2);

sizeSCmodJulRpt = size(CheckData2Matlab_SCmodJul{1,1},1);

% Append the data outputted from the GMAT object, in the form of a Matlab
% structure, to the Matlab cell array
CheckData2Matlab_SCmodJul{1,1}{sizeSCmodJulRpt+1,1} = sprintf('%-30.16s %-30.16s %-30.16s %-30.16f %-30.16f %-30.16f %-30.16f %-30.16f %-30.16f',SCmodjulian.Epoch,SCmodjulian.Epoch,SCmodjulian.Epoch,SCmodjulian.X,SCmodjulian.Y,SCmodjulian.Z,SCmodjulian.VX,SCmodjulian.VY,SCmodjulian.VZ);

% Replace existing contents of report file with the Matlab cell array data
fid2 = fopen([outputDir,'\SystemTest\',SCmodJulRptName],'w');
for loop = 1:sizeSCmodJulRpt+1;
    fprintf(fid2,'%s',CheckData2Matlab_SCmodJul{1,1}{loop,1});
    if loop ~= sizeSCmodJulRpt+1;
        fprintf(fid2,'\n');
    end;
end;
fclose(fid2);