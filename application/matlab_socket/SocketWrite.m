function [ isWriteOK ] = SocketWrite( client_socket, message )
    import java.net.Socket
    import java.io.*
    
    % get a buffered data oupt stream from the socket
    out_stream = client_socket.getOutputStream;
    
    % send message's length
    str_len = length(message);
    dat(1) = str_len;   
    out_stream.write(dat,0,1);
    pause(0.000005);

    % send message
    dat(1) = 0;
    for i= 1:str_len
        dat(i) = message(i);
    end
    dat(str_len+1) = 0;
    out_stream.write(dat,0,str_len);
    pause(0.000005);

    isWriteOK = true;
    
end
