"# the_net" 
At this state the code is still a little bit clumsy, but at least the backpropagation works:
the main consists out of two steps:
1. a neural networks is trained with the repeating input [0.3,0.5] and the corresponding repeating target values [0.9, 0.275] 
2. The trained neural network will give out the ouput for the input [0.3, 0.5], which should be of course very close to [0.9,0.275]
