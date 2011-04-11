function [ message ] = SocketRead( client_socket )
    import java.net.Socket
    import java.io.*

    input_stream   = client_socket.getInputStream;
    data_input_stream = DataInputStream(input_stream);
    pause(0.000005);
    message_length = input_stream.available;
%    fprintf(1, 'Reading %d bytes\n', message_length);
    
    message = zeros(1, message_length, 'uint8');
    for i = 1:message_length
        message(i) = data_input_stream.readByte;
    end
    message = char(message);
            
end

