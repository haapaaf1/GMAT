function Bug1441_Gmat2Matlab_SC(SCgregorian, SCmodjulian)

format long g

%%@todo - figure out how to locate report files without using output/SystemTest subfolder.
%%        Two reports are generated prior to this function call. (LOJ: 2010.10.12)
%%        The main script sends reports to OUTPUT_PATH from the startup file.
%%cd ('c:/projects/gmat/output/dev');
%% For now GMAT will throw **** ERROR **** Interface Exception Thrown: Error using ==> textscan
%% Invalid file identifier.  Use fopen to generate a valid file identifier.
%% We need Get() command in a future build to resolve this issue. 
%% ex) outPath = Get(OutputPath); cd (outPath);

% Process SC save using Gregorian for DateFormat
SCgregRptName = 'ZMATLAB_Bug1441-CheckData2Matlab_SCgreg.report';

% Load contents of report file into Matlab as a cell array
fid1 = fopen([SCgregRptName]);
eval('CheckData2Matlab_SCgreg = textscan(fid1,''%s'',''delimiter'',''\n'',''whitespace'','''');'); % store data as a variable name
fclose(fid1);

sizeSCgregRpt = size(CheckData2Matlab_SCgreg{1,1},1);

% Append the data outputted from the GMAT object, in the form of a Matlab
% structure, to the Matlab cell array
CheckData2Matlab_SCgreg{1,1}{sizeSCgregRpt+1,1} = sprintf('%-30.16s %-30.16s %-30.16s %-30.16f %-30.16f %-30.16f %-30.16f %-30.16f %-30.16f',SCgregorian.Epoch,SCgregorian.Epoch,SCgregorian.Epoch,SCgregorian.X,SCgregorian.Y,SCgregorian.Z,SCgregorian.VX,SCgregorian.VY,SCgregorian.VZ);

% Replace existing contents of report file with the Matlab cell array data
fid1 = fopen([SCgregRptName],'w');
for loop = 1:sizeSCgregRpt+1;
    fprintf(fid1,'%s',CheckData2Matlab_SCgreg{1,1}{loop,1});
    if loop ~= sizeSCgregRpt+1;
        fprintf(fid1,'\n');
    end;
end;
fclose(fid1);


% Process SC save using Modified Julian for DateFormat
SCmodJulRptName = 'ZMATLAB_Bug1441-CheckData2Matlab_SCmodJul.report';

% Load contents of report file into Matlab as a cell array
fid2 = fopen([SCmodJulRptName]);
eval('CheckData2Matlab_SCmodJul = textscan(fid2,''%s'',''delimiter'',''\n'',''whitespace'','''');'); % store data as a variable name
fclose(fid2);

sizeSCmodJulRpt = size(CheckData2Matlab_SCmodJul{1,1},1);

% Append the data outputted from the GMAT object, in the form of a Matlab
% structure, to the Matlab cell array
CheckData2Matlab_SCmodJul{1,1}{sizeSCmodJulRpt+1,1} = sprintf('%-30.16s %-30.16s %-30.16s %-30.16f %-30.16f %-30.16f %-30.16f %-30.16f %-30.16f',SCmodjulian.Epoch,SCmodjulian.Epoch,SCmodjulian.Epoch,SCmodjulian.X,SCmodjulian.Y,SCmodjulian.Z,SCmodjulian.VX,SCmodjulian.VY,SCmodjulian.VZ);

% Replace existing contents of report file with the Matlab cell array data
fid2 = fopen([SCmodJulRptName],'w');
for loop = 1:sizeSCmodJulRpt+1;
    fprintf(fid2,'%s',CheckData2Matlab_SCmodJul{1,1}{loop,1});
    if loop ~= sizeSCmodJulRpt+1;
        fprintf(fid2,'\n');
    end;
end;
fclose(fid2);

