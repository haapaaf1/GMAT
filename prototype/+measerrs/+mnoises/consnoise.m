classdef consnoise < measerrs.mnoises.mnoise
    % Class that handles the constant measurement noise case.
    % Note: this class just aliases the base class in the mnoise
    % subpackage.
    methods
        function cn = consnoise(R)
            cn = cn@measerrs.mnoises.mnoise(R);
        end % function
    end % methods
end % classdef