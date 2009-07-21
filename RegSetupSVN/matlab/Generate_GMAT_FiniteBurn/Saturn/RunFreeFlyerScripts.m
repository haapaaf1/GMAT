% This script will run all the FreeFlyer MissionPlan scripts that generate
% the "truth" Data to compare GMAT data against.  Make sure that the
% filepaths are correct for pulling the files from. Also the correct file
% path for the FreeFlyer Executable needs to be specified.

tic,
ScriptFolder = 'G:\FreeFlyer Scripts\Moon';
Scripts      = fuf('G:\FreeFlyer Scripts\Moon\*.MissionPlan','detail');

missplan = @(mpl) ['"C:\Program Files\a.i. solutions, Inc\FreeFlyer 6.5.1.8215\FF.exe" -mpl "',mpl,'"'];

fid = fopen('MissPlanList.txt','w');
cellfun(@(X) fprintf(fid,['-mp "',regexprep(X,{'%','\\'},{'%%','\\\\'}),'"\n']),Scripts,'uni',false);
stat = fclose(fid);

disp('GO!!!!');
status = dos(missplan([pwd,'\MissPlanList.txt']));
disp('Finish');
delete([pwd,'\MissPlanList.txt']);
toc