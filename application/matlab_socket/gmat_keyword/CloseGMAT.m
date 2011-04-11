%$Header: /cygdrive/p/dev/cvs/matlab/gmat_keyword/CloseGMAT.m,v 1.3 2005/12/22 19:17:12 jgurgan Exp $
function CloseGMAT()

global gmatChannel;

CallGMAT('Close','');
CloseConnection(gmatChannel);

gmatChannel = 0;
disp('Close;')

end
