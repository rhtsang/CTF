load ctf.hs
test1a = capture ["bWww-b-B-"] 'w' 1
test1b = capture ["bWww-b-B-"] 'w' 2
test1c = capture ["bWww-b-B-"] 'w' 3
test2 n = capture ["-W-w-bbBw"] 'b' n
