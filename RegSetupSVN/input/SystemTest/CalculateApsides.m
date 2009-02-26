function rdotv = CalculateApsides(state)

if numel(state) < 6
    rdotv = -999999.0000;
    return
end

rmag = sqrt(state(1)^2 + state(2)^2 + state(3)^2);
vmag = sqrt(state(4)^2 + state(5)^2 + state(6)^2);

rdotv=(state(1)*state(4)+state(2)*state(5)+state(3)*state(6))/(rmag*vmag);
