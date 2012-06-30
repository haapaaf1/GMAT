classdef randcurl < measerrs.biases.bias
    % Class for biases whose drift rate is a random constant.
    methods
        function rc = randcurl(P)
            rc = rc@measerrs.biases.bias(P);
            rc.biasStatePartial = [1 0 0];
            rc.stm = measerrs.biases.stms.curlstm;
        end % function
    end % methods
end % classdef