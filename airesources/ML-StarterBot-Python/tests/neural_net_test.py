from tsmlstarterbot.neural_net import NeuralNet
from tsmlstarterbot.common import PLANET_MAX_NUM, PER_PLANET_FEATURES

import numpy as np
import unittest

EPSILON = 1e-6
SEED = 0


def equal(a, b):
    return abs(a - b) < EPSILON


class TestNeuralNet(unittest.TestCase):
    def test_invariance(self):
        np.random.seed(SEED)

        # Create a random network
        nn = NeuralNet(seed=SEED)

        # Generate random input
        input_data = np.random.rand(PLANET_MAX_NUM, PER_PLANET_FEATURES)

        # Get predictions
        original_predictions = nn.predict(input_data)

        # Confirm different predictions for planet 0 and 1
        assert not equal(original_predictions[0], original_predictions[1])

        permuted_input_data = input_data
        # Swap planets 0 and 1
        permuted_input_data[[0, 1]] = input_data[[1, 0]]

        permuted_predictions = nn.predict(permuted_input_data)

        # Confirm the predictions are permuted
        assert equal(original_predictions[0], permuted_predictions[1])
        assert equal(original_predictions[1], permuted_predictions[0])
        for i in range(2, PLANET_MAX_NUM):
            assert equal(original_predictions[i], permuted_predictions[i])


if __name__ == "__main__":
    unittest.main()
