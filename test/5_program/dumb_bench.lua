local i = 0;
local sum = 0;
while i < 100000000 do
    i = i + 1
    sum = bit.band(sum + 5,0xFFF)
end
print(sum)
