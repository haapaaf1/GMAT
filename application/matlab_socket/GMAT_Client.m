% make a communication connection to GMAT server:
host = 'localhost';
port = 3000
repeat_times = 10
connection = ConnectToServer(host, port, repeat_times)

% send a request to GMAT server:
request = 'This is my request. Hi there. I am here'
fprintf('Sent request:%s\n',request);
SocketWrite(connection, request);

% receive a result from the server:
results = SocketRead(connection);
fprintf('Result:%s\n', results);

% close the communication connection:
CloseConnection(connection);
