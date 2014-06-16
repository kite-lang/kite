n = 200
result = [ (a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

main = print (result)
