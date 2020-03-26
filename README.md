"# the_net" 
At this state the code is still a little bit clumsy, but at least the backpropagation works and the generation of random neural networks is implemented:
the main shows how to use the important functions:

How to use the existing neural network simple net:

let net = train_data simple_net (replicate 10000  [0.3,0.5]) (replicate 10000  [0.9, 0.275])
                  in output net [0.3,0.5]
                  
1. a neural networks is trained with the repeating input [0.3,0.5] and the corresponding repeating target values [0.9, 0.275] 
2. The trained neural network will give out the ouput for the input [0.3, 0.5], which should be of course very close to [0.9,0.275]

How to generate a random neural network: 

-- generate_random_net witdh depth nr_neuron nr_con
the_net <- generate_random_net 2 4 4 2

This command will generate a random neural_network with corresponding width and depth
(nr_neuron) number of neurons (only counting the hidden-layers)  
(nr_con) number of connection (one connection will be connected backwards the rest forwards) 

Detailed descricptions:

All neurons of a layer can posses connections to random neurons of the following layers.
The active used neurons in a random generated network will be choosen from
a uniform distribution.

For example a network 5x5:

i i i i i\
n n n n n\
n n n n n\
n n n n n\
o o o o o

where i and o stand for the input and output neurons, which are allways active
with 4 active neurons (which posses connections) might look like this:

i i i i i\
a n a n n\
n n n n a\
n a n n m\
o o o o o

This would be a representation of a network generated with 4 neurons (nr_neuron)
While generating a neural network every neurons will generate a random backward connection 
(a forward connection of a node from a previous layer) and (nr_con -1) forward connection,
where the probability of a connction is chosen by the reciproc distance from the neurons 
1/(distance between neurons).

For example choosing between a possible connction A with distance 1/2 and and a possible connection B
with distance 1 the ratio 2 to 1 (1/(1/2) to 1/1) will dertermine the probality for connection a to be 66%. 
