function stateout = StateConV(statein,InputType,OutPutType,mu)

% Defintion of Types
% Type = 1;  Cartesian
% Type = 2;  Classical Keplerian
% Type = 3;  Modified keplerian
if nargin < 4
    mu = 398600.4415;
end

if     (InputType == 1 & OutPutType == 1)
    stateout = statein;
    return
elseif (InputType == 2 & OutPutType == 2)
    stateout = statein;
    return
elseif (InputType == 3 & OutPutType == 3)
    stateout = statein;
    return
end

if (InputType == 1 & OutPutType == 2)         %  Cartesian to Classical Keplerian
    stateout = ELORB(statein,mu);
elseif (InputType == 2 & OutPutType == 1)     %  Classical Keplerian to Cartesian 
    stateout  = RANDV(statein,mu);
elseif (InputType == 1 & OutPutType == 3)     %  Cartesian to Modified Keplerian  
    tempstate = ELORB(statein,mu);
    if ~isempty(tempstate)
        stateout  = Kep2ModKep(tempstate);
    else 
        stateout = [];
        return
    end
elseif (InputType == 3 & OutPutType == 1)     %  Modified Keplerian to Cartesian 
    tempstate = ModKep2Kep(statein);
    if ~isempty(tempstate)
        stateout  = RANDV(tempstate,mu);
    else
        stateout = [];
        return
    end
elseif (InputType == 2 & OutPutType == 3)     %  Classical Keplerian to Modified Keplerian
    stateout  = Kep2ModKep(statein);
elseif (InputType == 3 & OutPutType == 2)     %  Modified Keplerian to Classical Keplerian
    stateout  = ModKep2Kep(statein);
end