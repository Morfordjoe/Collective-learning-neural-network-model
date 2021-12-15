#Script trains solo neural networks at returning a bearing/angle output
#The correct angle is arbitrarily chosen (units = degrees/360)
#Loss function returns cost of squared error
#Neural networks are trained one datapoint at a time, and are tested between every instance of training
import numpy as np
import tensorflow
import keras
import keras.layers
import keras.models
from keras.layers import Input, Dense, concatenate
from keras.models import Model
from keras import optimizers
import keras.backend as K
from numpy.random import seed
import sys

#Number of neural networks trained
ind_n = 250
#Number of training trials for each neural network
Max = 25
#Learning rate hyperparameter in neural networks
LR = 0.05
#Momentum hyperparameter in neural networks
MM = 0
#Decay hyperparameter in neural network
DEC = 0


out_array = np.zeros(shape = (ind_n, Max+1))



for j in range(ind_n):
    #Loop: trains one neural network each iteration

    #seed(j)
    #Can use seed to get replicatable results

    correct_direction1 = np.random.uniform(low = 0, high = 1, size = 1)
    #Arbitrary correct angle chosen for neural network to be trained to. Units=degrees/360

    #Input into neural network. Always set to 1. Does not vary.
    NN_input = np.array([1])
    NN_input = np.asarray(NN_input, dtype=float)
    NN_input = np.reshape(NN_input, (1, 1))
    NN_input = np.ndarray.transpose(NN_input)

    #Ouptut that neural network is trained to
    correct_direction = np.array([correct_direction1])
    correct_direction = np.asarray(correct_direction, dtype=float)
    correct_direction = np.reshape(correct_direction, (1, 1))
    correct_direction = np.ndarray.transpose(correct_direction)

    #Neural network architecture
    visible = Input(shape=(NN_input.shape[1],))
    hidden1 = Dense(4, activation='relu')(visible)
    out = Dense(1, activation='linear')(hidden1)

    #Custom loss function: returns squared difference between correct angle and NN output
    def customLoss1(true, predict):
        predict_0_1 = (predict) % 1
        diff = K.abs(true - predict_0_1)
        new_diff = K.switch(diff>0.5, 1-diff, diff)
        return(K.square(new_diff))

    model = Model(inputs=visible, outputs=out)

    sgd = optimizers.SGD(lr=LR, decay=DEC, momentum=MM)


    model.compile(
    optimizer = sgd,
    loss = customLoss1,
    )

    i = 0

    #Testing neural network before beginning of training:
    print("Solo learner:")
    print(j + 1)
    print("After training trial:")
    print(i)
    print("Correct direction:")
    print(correct_direction1)
    print("Preferred direction:")
    pred = model.predict(NN_input)
    abs_pred1 = pred % 1
    print(abs_pred1)
    diff = np.absolute(correct_direction1 - (abs_pred1 % 1))
    if (diff > 0.5):
        diff = 1 - diff
    print("Error:")
    print(diff)

    out_array[j, i] = diff
    i = 1
    while (i <= Max):
        #Loop: each interation, trains neural network with a single datapoint and tests NN output
        model.fit(
        x = NN_input,
        y = correct_direction,
        validation_split=0,
        batch_size = 1,
        epochs = 1,
        )
        print("Solo learner:")
        print(j + 1)
        print("After training trial:")
        print(i)
        print("Correct direction:")
        print(correct_direction1)
        print("Preferred direction:")
        pred = model.predict(NN_input)
        abs_pred1 = pred % 1
        print(abs_pred1)
        diff = np.absolute(correct_direction1 - (abs_pred1 % 1))
        if (diff > 0.5):
            diff = 1 - diff
        out_array[j, i] = diff
        print("Error:")
        print(diff)

        i += 1
    K.clear_session()



#sys.exit()
#Can exit programme here

#Outputs results to file:
file_path = "/Users/joemorford/Desktop/CL_NNs/Sim_data/Solo_N"+str(ind_n)+"Max"+str(Max)+"LR"+str(LR)+"MM"+str(MM)+"DEC"+str(DEC) + ".csv"
import csv
with open(file_path, "w") as csv_file:
        writer = csv.writer(csv_file, delimiter=',')
        first_row = ["Individual_n", "Trial_n", "Individual"]
        print(first_row)
        writer.writerow(first_row)
        for i_n in range(ind_n):
            for each in range(Max+1):
                row = [i_n+1, each, out_array[i_n, each]]
                writer.writerow(row)
                print(row)
