import math
import os
import tensorflow as tf
import numpy as np

from tsmlstarterbot.common import PLANET_MAX_NUM, PER_PLANET_FEATURES

# Silence any tensorflow output
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '99'
tf.logging.set_verbosity(tf.logging.ERROR)


# Normalize each column of the input.
def normalize_columns(x):
    m = np.expand_dims(x.mean(axis=1), axis=1)
    s = np.expand_dims(x.std(axis=1), axis=1)
    return (x - m) / (s + 1e-6)


class NeuralNet(object):
    FIRST_LAYER_SIZE = 12
    SECOND_LAYER_SIZE = 6

    def _add_layer(self, input_layer, input_size, output_size, use_bias=True, activation=tf.nn.relu):
        """
        Adds one layer with weights and biases.

        :param input_layer
        :param input_size: size of the input
        :param output_size: size of the output
        :param use_bias: whether to use bias or not
        :param activation: activation function for the layer
        :return: fully connected layer activation(Wx + b)
        """

        # These weights are shared among all the planets
        shared_weights = tf.Variable(
            tf.random_normal([input_size, output_size], stddev=1.0 / math.sqrt(input_size + output_size)))
        # Add bias if requested
        bias = tf.Variable(tf.zeros(shape=output_size)) if use_bias else tf.constant(0, dtype=tf.float32)

        layer = tf.add(tf.matmul(input_layer, shared_weights), bias)

        return activation(layer)

    def __init__(self, cached_model=None, seed=None):
        self._graph = tf.Graph()

        with self._graph.as_default():
            if seed is not None:
                tf.set_random_seed(seed)
            self._session = tf.Session()
            self._features = tf.placeholder(dtype=tf.float32, name="input_features",
                                            shape=(None, PLANET_MAX_NUM, PER_PLANET_FEATURES))

            # target_distribution describes what the bot did in a real game.
            # For instance, if it sent 20% of the ships to the first planet and 15% of the ships to the second planet,
            # then expected_distribution = [0.2, 0.15 ...]
            self._target_distribution = tf.placeholder(dtype=tf.float32, name="target_distribution",
                                                       shape=(None, PLANET_MAX_NUM))

            # Combine all the planets from all the frames together in one big vector, so it's easier to share
            # the weights and biases between them in the network.
            flattened_frames = tf.reshape(self._features, [-1, PER_PLANET_FEATURES])

            first_layer = self._add_layer(flattened_frames, PER_PLANET_FEATURES, self.FIRST_LAYER_SIZE)
            second_layer = self._add_layer(first_layer, self.FIRST_LAYER_SIZE, self.SECOND_LAYER_SIZE)

            third_layer = self._add_layer(second_layer, self.SECOND_LAYER_SIZE, 1,
                                          use_bias=False, activation=lambda x: x)

            # Group the planets back in frames.
            logits = tf.reshape(third_layer, [-1, PLANET_MAX_NUM])

            self._prediction_normalized = tf.nn.softmax(logits)

            self._loss = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(
                logits=logits, labels=self._target_distribution))
            self._entropy = tf.reduce_mean(-tf.reduce_sum(
                self._target_distribution * tf.log(tf.clip_by_value(self._target_distribution, 1e-10, 1.0)),
                reduction_indices=[1]))

            self._optimizer = tf.train.AdamOptimizer(learning_rate=1e-4).minimize(self._loss)
            self._saver = tf.train.Saver()

            if cached_model is None:
                self._session.run(tf.global_variables_initializer())
            else:
                self._saver.restore(self._session, cached_model)

    def fit(self, input_data, expected_output_data):
        """
        Perform one step of training on the training data.

        :param input_data: numpy array of shape (number of frames, PLANET_MAX_NUM, PER_PLANET_FEATURES)
        :param expected_output_data: numpy array of shape (number of frames, PLANET_MAX_NUM)
        :return: training loss on the input data
        """
        loss, _ = self._session.run([self._loss, self._optimizer],
                                    feed_dict={self._features: normalize_columns(input_data),
                                               self._target_distribution: expected_output_data})
        return loss

    def predict(self, input_data):
        """
        Given data from 1 frame, predict where the ships should be sent.

        :param input_data: numpy array of shape (PLANET_MAX_NUM, PER_PLANET_FEATURES)
        :return: 1-D numpy array of length (PLANET_MAX_NUM) describing percentage of ships
        that should be sent to each planet
        """
        return self._session.run(self._prediction_normalized,
                                 feed_dict={self._features: normalize_columns(np.array([input_data]))})[0]

    def compute_loss(self, input_data, expected_output_data):
        """
        Compute loss on the input data without running any training.

        :param input_data: numpy array of shape (number of frames, PLANET_MAX_NUM, PER_PLANET_FEATURES)
        :param expected_output_data: numpy array of shape (number of frames, PLANET_MAX_NUM)
        :return: training loss on the input data
        """
        return self._session.run(self._loss,
                                 feed_dict={self._features: normalize_columns(input_data),
                                            self._target_distribution: expected_output_data})

    def compute_entropy(self, target_distribution):
        """
        Compute entropy of the target distribution.
        :param target_distribution: numpy array of shape (number of frames, PLANET_MAX_NUM)
        :return: entropy of the target distribution
        """
        return self._session.run(self._entropy, feed_dict={self._target_distribution: target_distribution})

    def save(self, path):
        """
        Serializes this neural net to given path.
        :param path:
        """
        self._saver.save(self._session, path)

