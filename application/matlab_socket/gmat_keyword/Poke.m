%$Header: /GMAT/dev/cvs/supportfiles/matlab/gmat_keyword/Poke.m,v 1.1 2007/08/22 19:15:16 shughes Exp $
function Poke(channel, dataType, dataStr)

   data = [dataType ',' dataStr]; 
   disp(['Send:' data]);
   
   SocketWrite(channel, data);
   data1 = SocketRead(channel);
   
   disp(['Receice:' data1]);
end
