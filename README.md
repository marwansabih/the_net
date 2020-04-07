"# the_net" 
At this state the code is still a little bit clumsy, 
the main shows two simple examples of how to train networks with the given sample data_set:

Before going into full detail a short introduction of all essential functions will be given here:

-- training_batches nr_times bs net sample s

trains the network (net) for nr_times with a batch_size bs using the sample with learningrate s
using mini gradient batch descent
clarification: batch_size means that the network will use a batch of batch_size examples drawn
randomly at once form the sample to minimize the training error.

-- find_best_fully_connected_net nr_nets training_steps bs width depth sample s

will automatically generate nr_nets fully connected networks with spezified width 
(should be 7 cause test_sample has width 7), depth and train all of them
with spezified number of trainings_steps, batch_size bs and learning_rate s

-- find_best_random_net nr_nets training_steps bs width depth nr_neuron nr_con sample s

similar to the previous command, but this time a random net is used for training
for more information see bottom - here nr_neuron is the number of neurons
which will be spread between the input_layer and outputlayer 
and every neuron will first be placed and then create nr_con connections.
One connection to a previous layers and the next connections to 
following layer.
clarification: a network for example of width 7 and depth 5 can have
a maximum of 35 neurons and since 14 of these neurons will be taken by
input and output layer only 21 layers will remain (the maximum number
you can specify for nr_neuron). While creating 
a neural network neurons might generate less connection than specified
(1 connection goes back the rest to the following layers, therefore the number of
possible connections by the number of neurons in the following layers.)

-- update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s

Allows a special kind of training which will be repeated nr_times 
here the goal is to enhance the network structure by iteratively 
alternate the network by removing some neurons and replacing them by new ones.
The idea is to keep most of the network architecture intact by keeping
most of the weights.
The network of the last step will allways compete with his newly altered network,
to make the process of finding a better architectur more easy the network 
from last step will reset the weigths of the connections of the neurons,
which are replaced in the altered network.

-- output net input

allows to generate the prediction of the trained network net given the input

-- alter_neurons nr_neuron nr_cons net

deletes specifified number of neurons from a network and replaces them with new ones who will create 
(if possible, otherwise less) number connections.

-- generate_fully_connected_net width depth

should be self-explanatory 

-- generate_random_net width depth nr_neuron nr_con

clarification: a network for example of width 7 and depth 5 can have
a maximum of 35 neurons and since 14 of these neurons will be taken by
input and output layer only 21 layers will remain (the maximum number
you can specify for nr_neuron). While creating 
a neural network neurons might generate less connection than specified
(1 connection goes back the rest to the following layers, therefore the number of
possible connections by the number of neurons in the following layers.)

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

This would be a representation of a network generated with 4 neurons (nr_neuron).

While generating a neural network every neuron will generate a random backward connection 
(a forward connection of a node from a previous layer) and (nr_con -1) forward connection,
where the probability of a connction is chosen by the reciproc distance from the neurons 
1/(distance between neurons).

For example choosing between a possible connction A with distance 1/2 and and a possible connection B
with distance 1 the ratio 2 to 1 (1/(1/2) to 1/1) will dertermine the probality for connection A to be 66%. 
