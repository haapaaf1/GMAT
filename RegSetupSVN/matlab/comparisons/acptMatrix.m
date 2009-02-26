function [maxNorm2XL] = acptMatrix(maxNorm,maxNorm2XL,currCase,duration,posErrorDrag,posErrorNSG,posErrorSRP,posErrorPMG,max5,max5loc)

% ==== Acceptance Matrix ====
acceptSwitch = 0;
switch acceptSwitch
    case {isempty(findstr(currCase,'JR')), isempty(findstr(currCase,'HP')), isempty(findstr(currCase,'MSISE90')), isempty(findstr(currCase,'MSISE00'))}
        if maxNorm(1,2) <= posErrorDrag
            maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000, duration ,'Pass', posErrorDrag * 1000,['(',num2str(max5(1)*1000,13),' , ',num2str(max5loc(1)),')'],['(',num2str(max5(2)*1000,13),' , ',num2str(max5loc(2)),')'],['(',num2str(max5(3)*1000,13),' , ',num2str(max5loc(3)),')'],['(',num2str(max5(4)*1000,13),' , ',num2str(max5loc(4)),')'],['(',num2str(max5(5)*1000,13),' , ',num2str(max5loc(5)),')'],max5loc(6)}];
        else
            maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000, duration ,'Fail', posErrorDrag * 1000,['(',num2str(max5(1)*1000,13),' , ',num2str(max5loc(1)),')'],['(',num2str(max5(2)*1000,13),' , ',num2str(max5loc(2)),')'],['(',num2str(max5(3)*1000,13),' , ',num2str(max5loc(3)),')'],['(',num2str(max5(4)*1000,13),' , ',num2str(max5loc(4)),')'],['(',num2str(max5(5)*1000,13),' , ',num2str(max5loc(5)),')'],max5loc(6)}];
        end
    case {isempty(findstr(currCase,'JGM')), isempty(findstr(currCase,'EGM')), isempty(findstr(currCase,'LP165P')), isempty(findstr(currCase,'MARS50C')), isempty(findstr(currCase,'MGNP180U')), isempty(findstr(currCase,'ZONALS4'))}
        if maxNorm(1,2) <= posErrorNSG
            maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000, duration ,'Pass', posErrorNSG * 1000,['(',num2str(max5(1)*1000,13),' , ',num2str(max5loc(1)),')'],['(',num2str(max5(2)*1000,13),' , ',num2str(max5loc(2)),')'],['(',num2str(max5(3)*1000,13),' , ',num2str(max5loc(3)),')'],['(',num2str(max5(4)*1000,13),' , ',num2str(max5loc(4)),')'],['(',num2str(max5(5)*1000,13),' , ',num2str(max5loc(5)),')'],max5loc(6)}];
        else
            maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000, duration ,'Fail', posErrorNSG * 1000,['(',num2str(max5(1)*1000,13),' , ',num2str(max5loc(1)),')'],['(',num2str(max5(2)*1000,13),' , ',num2str(max5loc(2)),')'],['(',num2str(max5(3)*1000,13),' , ',num2str(max5loc(3)),')'],['(',num2str(max5(4)*1000,13),' , ',num2str(max5loc(4)),')'],['(',num2str(max5(5)*1000,13),' , ',num2str(max5loc(5)),')'],max5loc(6)}];
        end
    case {isempty(findstr(currCase,'SRP'))}
        if maxNorm(1,2) <= posErrorSRP
            maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000, duration ,'Pass', posErrorSRP * 1000,['(',num2str(max5(1)*1000,13),' , ',num2str(max5loc(1)),')'],['(',num2str(max5(2)*1000,13),' , ',num2str(max5loc(2)),')'],['(',num2str(max5(3)*1000,13),' , ',num2str(max5loc(3)),')'],['(',num2str(max5(4)*1000,13),' , ',num2str(max5loc(4)),')'],['(',num2str(max5(5)*1000,13),' , ',num2str(max5loc(5)),')'],max5loc(6)}];
        else
            maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000, duration ,'Fail', posErrorSRP * 1000,['(',num2str(max5(1)*1000,13),' , ',num2str(max5loc(1)),')'],['(',num2str(max5(2)*1000,13),' , ',num2str(max5loc(2)),')'],['(',num2str(max5(3)*1000,13),' , ',num2str(max5loc(3)),')'],['(',num2str(max5(4)*1000,13),' , ',num2str(max5loc(4)),')'],['(',num2str(max5(5)*1000,13),' , ',num2str(max5loc(5)),')'],max5loc(6)}];
        end
    case {isempty(findstr(currCase,'Earth')),isempty(findstr(currCase,'Sun')),isempty(findstr(currCase,'Luna')),isempty(findstr(currCase,'Mars')),isempty(findstr(currCase,'Venus')),isempty(findstr(currCase,'Mercury')),isempty(findstr(currCase,'Pluto')),isempty(findstr(currCase,'Jupiter')),isempty(findstr(currCase,'Uranus')),isempty(findstr(currCase,'Neptune')),isempty(findstr(currCase,'Saturn')),isempty(findstr(currCase,'AllPlanets'))}
        if maxNorm(1,2) <= posErrorPMG
            maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000, duration ,'Pass', posErrorPMG * 1000,['(',num2str(max5(1)*1000,13),' , ',num2str(max5loc(1)),')'],['(',num2str(max5(2)*1000,13),' , ',num2str(max5loc(2)),')'],['(',num2str(max5(3)*1000,13),' , ',num2str(max5loc(3)),')'],['(',num2str(max5(4)*1000,13),' , ',num2str(max5loc(4)),')'],['(',num2str(max5(5)*1000,13),' , ',num2str(max5loc(5)),')'],max5loc(6)}];
        else
            maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000, duration ,'Fail', posErrorPMG * 1000,['(',num2str(max5(1)*1000,13),' , ',num2str(max5loc(1)),')'],['(',num2str(max5(2)*1000,13),' , ',num2str(max5loc(2)),')'],['(',num2str(max5(3)*1000,13),' , ',num2str(max5loc(3)),')'],['(',num2str(max5(4)*1000,13),' , ',num2str(max5loc(4)),')'],['(',num2str(max5(5)*1000,13),' , ',num2str(max5loc(5)),')'],max5loc(6)}];
        end
    otherwise
end
% ==============