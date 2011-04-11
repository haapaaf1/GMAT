function OpenGMAT()

global gmatService;
global gmatTopic;
global gmatChannel;

gmatService = '3000';
gmatTopic = 'GMAT_SOCKET_SERVER';

host = 'localhost';
port = 3000;
gmatChannel = ConnectToServer(host, port, 100);

if (gmatChannel == 0)
   disp('GMAT Server has not started. Please start the server first!');
else
   CallGMAT('Open', '');
end
