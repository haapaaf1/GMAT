function [ client_socket ] = ConnectToServer( host, port, number_of_retries)
    import java.net.Socket
    import java.io.*

    retry = 0;
    while true
        retry = retry + 1;
        if ((number_of_retries > 0) && (retry > number_of_retries))
            break;
        end
        
        try
            client_socket = Socket(host, port);
            return
        catch
            % pause before retrying
            pause(0.001);
        end
    end
    
    client_socket = 0;
    return
end

