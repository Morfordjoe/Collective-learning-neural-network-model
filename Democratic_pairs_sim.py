#Script trains paired neural networks at returning a bearing/angle output
#The correct angle is arbitrarily chosen (units = degrees/360)
#A weighted average (weighted by leadership coefficient) determines collective direction output from individual NN outputs
#Loss function returns cost to NN which is squared COLLECTIVE error (not just individual output error)
#Neural networks are trained one datapoint at a time, and are tested between every instance of training
#NNs each have subjective angular system (essentially 0 degrees is in a different random place for each NN)...
#this ensures that there is a uniform distribution (-180 to 180 degrees) of angular difference between initial outputs of pairs of NN

import sys
import numpy as np
import tensorflow as tf
import keras
import keras.layers
import keras.models
from keras.layers import Input, Dense, concatenate
from keras.models import Model
from keras import optimizers
import keras.backend as K
from numpy.random import seed
import matplotlib.pyplot as plt
from numpy.random import seed
from scipy.stats import circmean
import datetime

#Trig function for keras custom loss functions
def atan2(x, y):
  angle = tf.where(tf.greater(x,0.0), tf.atan(y/x), tf.zeros_like(x))
  angle = tf.where(tf.logical_and(tf.less(x,0.0),  tf.greater_equal(y,0.0)), tf.atan(y/x) + np.pi, angle)
  angle = tf.where(tf.logical_and(tf.less(x,0.0),  tf.less(y,0.0)), tf.atan(y/x) - np.pi, angle)
  return angle

#Number of pairs of neural networks trained
pair_n = 250
#Number of training trials for each pair of neural network
Max = 25
#Learning rate hyperparameter in neural networks
LR = 0.05
#Momentum hyperparameter in neural networks
MM = 0
#Decay hyperparameter in neural network
DEC = 0


leadership_coeff = 0.9
#Coefficient of leadership between the pair. Is the weighting of the leader's input in the weighted average
#0.5 <= leadership_coeff < 1

#Output lists
collective_out_list = []
leader_out_list = []
follower_out_list = []
pair_diff_list = []
leader_collective_membership_benefit_list = []
follower_collective_membership_benefit_list = []

for j in range(pair_n):
    #Loop: trains one pair of neural network each iteration

    a = datetime.datetime.now()

    #seed(j)
    #Can use seed to get replicatable results


    #Correct direction for neural network pair
    correct_direction_objective = np.random.uniform(low = 0, high = 1, size = 1)
    #Units: degrees/360

    #Each neural network calibrates its angles differently (0 degrees/360 is in a different place on the circle)
    #correct_direction_objective gives the 'objective' positioning of 0 degrees/360. Could be considered as North.
    #This ensures that the difference between the initial outputs of pairs of neural networks is uniformly distributed...
    #between -180 and 180 degrees, by nullifying the effect of the initial, untrained distribution of neural network outputs
    #in biasing this distibution

    #Correct direction for neural network 1
    correct_direction_ego1 = np.random.uniform(low = 0, high = 1, size = 1)
    #Correct direction for neural network 2
    correct_direction_ego2 = np.random.uniform(low = 0, high = 1, size = 1)
    #The difference between NN1's 0 degrees/360 and the objective 0 degrees/360
    diff_ego1 = correct_direction_ego1 - correct_direction_objective
    #The difference between NN2's 0 degrees/360 and the objective 0 degrees/360
    diff_ego2 = correct_direction_ego2 - correct_direction_objective
    #The difference between NN1's 0 degrees/360 and NN2's 0 degrees/360
    diff_ego1_ego2 = correct_direction_ego1 - correct_direction_ego2

    #Input into both neural networks. Always set to 1. Does not vary.
    NN_input = np.array([1])
    NN_input = np.asarray(NN_input, dtype=float)
    NN_input = np.reshape(NN_input, (1, 1))
    NN_input = np.ndarray.transpose(NN_input)

    #Ouptut that neural network is trained to
    #NN outputs are corrected 'ego'/subjective direction to objective directions in their respective loss functions
    correct_direction = np.array([correct_direction_objective])
    correct_direction = np.asarray(correct_direction, dtype=float)
    correct_direction = np.reshape(correct_direction, (1, 1))
    correct_direction = np.ndarray.transpose(correct_direction)

    #Decision-making weighting for neural network 1
    leader_1 = [leadership_coeff]
    #Decision-making weighting for neural network 2
    leader_2 = [1-leadership_coeff]

    #Neural network 1 architecture
    visible_1 = Input(shape=(NN_input.shape[1],))
    hidden1_1 = Dense(4, activation='relu')(visible_1)
    out_1 = Dense(1, activation='linear')(hidden1_1)

    #Neural network 2 architecture
    visible_2 = Input(shape=(NN_input.shape[1],))
    hidden1_2 = Dense(4, activation='relu')(visible_2)
    out_2 = Dense(1, activation='linear')(hidden1_2)


    model1 = Model(inputs=visible_1, outputs=out_1)
    model2 = Model(inputs=visible_2, outputs=out_2)


    sgd = optimizers.SGD(lr=LR, decay=DEC, momentum=MM)

    collective_results = []
    leader_results = []
    follower_results = []
    pair_diff_results = []
    leader_group_b = []
    follower_group_b = []

    i = 0

    while (i <= Max):
        #Loop: each interation, tests NN output and trains neural network with a single datapoint
        print("NN pair:")
        print(j + 1)
        print("After training trial:")
        print(i)
        print("Objective correct direction:")
        print(correct_direction_objective)
        print("Subjective correct direction (NN1):")
        print(correct_direction_ego1)
        print("Subjective correct direction (NN2):")
        print(correct_direction_ego2)

        print("Preferred (subjective) directions:")
        pred1 = model1.predict(NN_input)
        pred2 = model2.predict(NN_input)
        print(pred1)
        print(pred2)

        abs_pred1 = (pred1 + diff_ego1) % 1
        abs_pred2 = (pred2 + diff_ego2) % 1
        print("Preferred (objective) directions:")
        print(abs_pred1)
        print(abs_pred2)

        #Leader error:
        leader_diff_raw = correct_direction_objective - (abs_pred1 % 1)
        if leader_diff_raw > 0.5:
            leader_diff_raw = -1 + leader_diff_raw
        elif leader_diff_raw < -0.5:
            leader_diff_raw = 1 + leader_diff_raw
        leader_diff_absolute = np.absolute(leader_diff_raw)

        #Follower error:
        follower_diff_raw = correct_direction_objective - (abs_pred2 % 1)
        if follower_diff_raw > 0.5:
            follower_diff_raw = -1 + follower_diff_raw
        elif follower_diff_raw < -0.5:
            follower_diff_raw = 1 + follower_diff_raw
        follower_diff_absolute = np.absolute(follower_diff_raw)

        print("Decision-making weightings:")
        print(leader_1)
        print(leader_2)


        pair_diff = np.absolute(abs_pred1 - abs_pred2)
        if pair_diff > 0.5:
            pair_diff = 1 - pair_diff
        print("Pair difference (in objective output):", pair_diff)

        pair_d = abs_pred1 - abs_pred2
        new_abs_pred1 = abs_pred1
        new_abs_pred2 = abs_pred2
        if pair_d > 0.5:
            new_abs_pred2 = abs_pred2 + 1
        elif pair_d < -0.5:
            new_abs_pred1 = abs_pred1 + 1
        abs_rads = [new_abs_pred1*2*np.pi, new_abs_pred2*2*np.pi]
        mean_cos = np.cos(abs_rads[0]*leader_1 + abs_rads[1]*leader_2)
        mean_sin = np.sin(abs_rads[0]*leader_1 + abs_rads[1]*leader_2)
        x = np.arctan2(mean_sin, mean_cos)/(2*(np.pi))
        if x < 0:
            x = 1 + x
        collective_direction = x
        print("Collective direction:")
        print(collective_direction)

        collective_error = np.abs(collective_direction - correct_direction_objective)
        if collective_error > 0.5:
            collective_error = 1 - collective_error
        print("Collective error:")
        print(collective_error)
        print("Collective error squared:")
        print(collective_error**2)

        leader_collective_membership_benefit = leader_diff_absolute - collective_error
        print("Leader collective membership benefit:", leader_collective_membership_benefit)

        follower_collective_membership_benefit = follower_diff_absolute - collective_error
        print("Follower collective membership benefit:", follower_collective_membership_benefit)

        collective_results.append(collective_error)
        leader_results.append(leader_diff_raw)
        follower_results.append(follower_diff_raw)
        pair_diff_results.append(pair_diff)
        leader_group_b.append(leader_collective_membership_benefit)
        follower_group_b.append(follower_collective_membership_benefit)

        if (i<Max):

            #Loss function for NN1
            #Converts subjective output to objective angle
            #Then determines collective direction with collective decision making rule (weighted average)
            #Then determines squared collective error -> output of loss function
            def customLoss1(true, predict):
                predict_0_1 = (predict + diff_ego1) % 1
                pair_df = predict_0_1 - abs_pred2
                new_pred_0_1 = K.switch(pair_df<(-0.5), predict_0_1+1, predict_0_1)
                cos = K.cos(new_pred_0_1*2*np.pi*leader_1 + new_abs_pred2*2*np.pi*leader_2)
                sin = K.sin(new_pred_0_1*2*np.pi*leader_1 + new_abs_pred2*2*np.pi*leader_2)
                atan = atan2(cos, sin)/(2*np.pi)
                switch_atan = K.switch(atan<0, 1+atan, atan)
                error = K.abs(true - switch_atan)
                corrected_error = K.switch(error>0.5, 1-error, error)
                return(K.square(corrected_error))


            model1.compile(
            optimizer = sgd,
            loss = customLoss1,
            )

            model1.fit(
            x = NN_input,
            y = correct_direction,
            validation_split=0,
            batch_size = 1,
            epochs = 1,
            )

            #Loss function for NN2
            def customLoss2(true, predict):
                predict_0_1 = (predict + diff_ego2) % 1
                pair_df = abs_pred1 - predict_0_1
                new_pred_0_1 = K.switch(pair_df>0.5, predict_0_1 + 1, predict_0_1)
                cos = K.cos(new_pred_0_1*2*np.pi*leader_2 + new_abs_pred1*2*np.pi*leader_1)
                sin = K.sin(new_pred_0_1*2*np.pi*leader_2 + new_abs_pred1*2*np.pi*leader_1)
                atan = atan2(cos, sin)/(2*np.pi)
                switch_atan = K.switch(atan<0, 1+atan, atan)
                error = K.abs(true - switch_atan)
                corrected_error = K.switch(error>0.5, 1-error, error)
                return(K.square(corrected_error))


            model2.compile(
            optimizer = sgd,
            loss = customLoss2,
            )

            model2.fit(
            x = NN_input,
            y = correct_direction,
            validation_split=0,
            batch_size = 1,
            epochs = 1,
            )

        i += 1

    collective_out_list.append(collective_results)
    leader_out_list.append(leader_results)
    follower_out_list.append(follower_results)
    pair_diff_list.append(pair_diff_results)
    leader_collective_membership_benefit_list.append(leader_group_b)
    follower_collective_membership_benefit_list.append(follower_group_b)

    b = datetime.datetime.now()
    c = b - a
    print("\n")
    print("Time to complete NN pair training and testing (s):", c.seconds)
    print("\n")

    K.clear_session()

#sys.exit()
#Can exit programme here

#Outputs results to file:
file_path = "/Users/joemorford/Desktop/CL_NNs/Sim_data/Democratic_pairs_N"+str(pair_n)+"Max"+str(Max)+"LR"+str(LR)+"MM"+str(MM)+"DEC"+str(DEC)+"L"+str(leadership_coeff) +".csv"
import csv
with open(file_path, "w") as csv_file:
        writer = csv.writer(csv_file, delimiter=',')
        first_row = ["Pair_n", "Trial_n", "Collective", "Leader", "Follower", "Pair_diff", "Leader_collective_membership_benefit", "Follower_collective_membership_benefit"]
        print(first_row)
        writer.writerow(first_row)
        for p_n in range(pair_n):
            for each in range(Max+1):
                row = [p_n+1, each, collective_out_list[p_n][each][0][0], leader_out_list[p_n][each][0][0], follower_out_list[p_n][each][0][0], pair_diff_list[p_n][each][0][0], leader_collective_membership_benefit_list[p_n][each][0][0], follower_collective_membership_benefit_list[p_n][each][0][0]]
                writer.writerow(row)
                print(row)
