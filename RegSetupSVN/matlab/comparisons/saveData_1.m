function [maxNormAll,storeFilename] = saveData_1(maxNorm,maxNormAll,currCase,Tool1Folder,Tool2Folder,DataDir,mat_Tool11,mat_Tool21,mat_header,normMat_Tool1_Tool2,allLoopChoice)

maxNormAll(allLoopChoice,:)  = maxNorm;
savefile = [Tool1Folder,'_',Tool2Folder,'_',currCase,'.mat'];
warning off
save([DataDir,'/',savefile],'mat_Tool11','mat_Tool21','mat_header','normMat_Tool1_Tool2','-v6');
warning on
storeFilename{allLoopChoice} = savefile;