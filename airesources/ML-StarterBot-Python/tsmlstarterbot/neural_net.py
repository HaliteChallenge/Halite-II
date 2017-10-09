import math
import os
import tensorflow as tf
import numpy as np

from tsmlstarterbot.common import PLANET_MAX_NUM, PER_PLANET_FEATURES

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '99'
tf.logging.set_verbosity(tf.logging.ERROR)


def normalize_columns(x):
    m = np.expand_dims(x.mean(axis=1), axis=1)
    s = np.expand_dims(x.std(axis=1), axis=1)
    return (x - m) / (s + 1e-6)


class NeuralNet(object):
    FIRST_LAYER_PER_PLANET_FEATURES = 12
    SECOND_LAYER_PER_PLANET_FEATURES = 12

    EPSILON = 1e-6

    def _add_layer(self, input_layer, input_features_num, output_features_num, use_bias=True, activation=tf.nn.relu):
        """
        Adds one layer with weights and biases shared among all the planets.

        :param input_layer: always with shape = (size of minibatch, PLANET_MAX_NUM, input_features_num)
        :param input_features_num: number of input features
        :param output_features_num: number of output featurs
        :param use_bias: whether to use bias or not
        :param activation: activation function for the layer
        :return: layer
        """
        # These weights are shared among all the planets
        shared_weights = tf.Variable(
            tf.random_normal([input_features_num, output_features_num], stddev=1.0 / math.sqrt(input_features_num + output_features_num)))

        batch_size = tf.shape(input_layer)[0]

        # tf.matmul does not support broadcasting, so we need batch_size copies of shared_weights
        shared_weights_reshaped = tf.expand_dims(shared_weights, 0)
        shared_weights_broadcasted = tf.tile(shared_weights_reshaped, [batch_size, 1, 1])

        # Add bias if requested
        bias = tf.Variable(tf.zeros(shape=(1, 1, output_features_num))) if use_bias else tf.constant(0, dtype=tf.float32)

        layer = tf.add(tf.matmul(input_layer, shared_weights_broadcasted), bias)

        return activation(layer)

    def __init__(self, cached_model=None, seed=None):
        # Construct neural net
        self._graph = tf.Graph()

        with self._graph.as_default():
            if seed is not None:
                tf.set_random_seed(seed)
            self._session = tf.Session()
            self._features = tf.placeholder(dtype=tf.float32, name="input_features",
                                            shape=(None, PLANET_MAX_NUM, PER_PLANET_FEATURES))
            # expected_distribution describes what the bot did in a real game.
            # For instance, if it sent 20% of the ships to the first planet and 15% of the ships to the second planet,
            # then expected_distribution = [0.2, 0.15 ...]
            self._expected_distribution = tf.placeholder(dtype=tf.float32, name="expected_distribution",
                                                         shape=(None, PLANET_MAX_NUM))

            first_layer = self._add_layer(self._features, PER_PLANET_FEATURES,
                                          self.FIRST_LAYER_PER_PLANET_FEATURES)
            second_layer = self._add_layer(first_layer, self.FIRST_LAYER_PER_PLANET_FEATURES,
                                           self.SECOND_LAYER_PER_PLANET_FEATURES)

            third_layer = self._add_layer(second_layer, self.SECOND_LAYER_PER_PLANET_FEATURES, 1, use_bias=False, activation=tf.identity)

            logits = tf.reshape(third_layer, [-1, PLANET_MAX_NUM])

            self._prediction_normalized = tf.nn.softmax(logits)

            self._loss = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(
                logits=logits, labels=self._expected_distribution))
            self._entropy = tf.reduce_mean(-tf.reduce_sum(
                self._expected_distribution * tf.log(tf.clip_by_value(self._expected_distribution, 1e-10, 1.0)),
                reduction_indices=[1]))

            self._optimizer = tf.train.AdamOptimizer(learning_rate=1e-4).minimize(self._loss)
            self._saver = tf.train.Saver()

            if cached_model is None:
                self._session.run(tf.global_variables_initializer())
            else:
                self._saver.restore(self._session, cached_model)

    # returns loss
    def fit(self, input_data, expected_output_data):
        loss, _ = self._session.run([self._loss, self._optimizer],
                                    feed_dict={self._features: normalize_columns(input_data),
                                               self._expected_distribution: expected_output_data})
        return loss

    # returns probability distribution
    def predict(self, input_data):
        return self._session.run(self._prediction_normalized,
                                 feed_dict={self._features: normalize_columns(input_data)})

    def compute_loss(self, input_data, expected_output_data):
        return self._session.run(self._loss,
                                 feed_dict={self._features: normalize_columns(input_data),
                                            self._expected_distribution: expected_output_data})

    def compute_entropy(self, expected_output_data):
        return self._session.run(self._entropy, feed_dict={self._expected_distribution: expected_output_data})

    def save(self, path):
        self._saver.save(self._session, path)
