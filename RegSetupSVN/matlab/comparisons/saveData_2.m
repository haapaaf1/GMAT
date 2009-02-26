function [storeFilename,savefile] = saveData_2(maxDiffs,maxDiffsAll,currCase,Tool1Folder,Tool2Folder,DataDir,mat_Tool11,mat_Tool21,mat_header,diffMat_Tool1_Tool2,allLoopChoice,nameFile)

savefile = [Tool1Folder,'_',Tool2Folder,'_',nameFile,'_',currCase,'.mat'];
warning off;
save([DataDir,'/',savefile],'mat_Tool11','mat_Tool21','mat_header','maxDiffs','diffMat_Tool1_Tool2','-v6');
warning on;
storeFilename{allLoopChoice} = savefile;