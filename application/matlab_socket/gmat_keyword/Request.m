%$Header: /cygdrive/p/dev/cvs/matlab/gmat_keyword/Request.m,v 1.3 2005/12/22 19:17:12 jgurgan Exp $
function data = Request(channel, var)

   Poke(channel,'Request', var);
   SocketWrite(channel,'Idol');
   data = SocketRead(channel);
   disp(['Results:' data]);
   
end
