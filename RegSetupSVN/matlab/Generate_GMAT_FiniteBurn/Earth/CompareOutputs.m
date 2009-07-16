tic,
FFresults = fuf('G:\FreeFlyer Output\Earth','detail');
GMATresults = fuf('G:\GMAT Output\Earth','detail');

%Filter Out File errors by results file size;
for i = 1:min([length(FFresults),length(GMATresults)]);
    FFinfo = dir(FFresults{i});
    FFsize(i) = FFinfo.bytes;
    GMATinfo = dir(GMATresults{i});
    GMATsize(i) = GMATinfo.bytes;
end
FailedFiles = FFresults(find(FFsize < 29000));
FFresults = FFresults(find(FFsize > 29000));
GMATresults = GMATresults(find(FFsize > 29000));

FFCells = cellfun(@(x) importdata(x),FFresults,'uni',false);
GMATCells = cellfun(@(x) importdata(x),GMATresults,'uni',false);
try
    maxofcols = arrayfun(@(x) max(FFCells{x}-GMATCells{x}),1:length(FFCells),'uni',false);
end
maxfilediffs = cellfun(@max,maxofcols);
disp('Maximum difference in all files is:');
disp(max(maxfilediffs));
disp(GMATresults(find(maxfilediffs == max(maxfilediffs))));
toc