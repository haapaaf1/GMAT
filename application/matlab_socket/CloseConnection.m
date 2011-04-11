function [ isCloseOK ] = CloseConnection( socket_client )
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here
    import java.net.Socket
    import java.io.*

    isCloseOK = false;
    
    socket_client.close;

    isCloseOK = true;
end

