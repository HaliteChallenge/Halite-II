"""
Communication with the Game Environment

These functions are used to send/receive data from the game itself. In either
direction, communication is terminated with a newline character.
"""

import logging
import sys

import hlt
from . import game_map


def send_string(s):
    """Send data to the game. Call :function:`done_sending` once finished."""
    sys.stdout.write(s)
    sys.stdout.flush()


def done_sending():
    """Finish sending commands to the game."""
    sys.stdout.write('\n')
    sys.stdout.flush()


def get_string():
    """Read input from the game."""
    result = sys.stdin.readline().rstrip('\n')
    return result


def initialize(name):
    """
    Initialize the bot with the given name.
    :param name: The name of the bot.
    :return: The player tag, map dimensions, and initial map.
    """

    tag = int(get_string())
    hlt.my_tag = tag
    hlt.map_size = [int(x) for x in get_string().strip().split()]
    initial_map = get_string()
    send_string(name)
    done_sending()

    # Set up and truncate the log
    log_file = "{}_{}.log".format(hlt.my_tag, name)
    with open(log_file, 'w'):
        pass
    logging.basicConfig(filename=log_file, level=logging.DEBUG)
    logging.info("Initialized bot {}".format(name))

    hlt.last_map = initial_map

    return tag, hlt.map_size, initial_map


def send_command_queue(command_queue):
    """
    Issue the given list of commands.
    :param command_queue:
    :return:
    """
    for command in command_queue:
        send_string(command)

    done_sending()


def get_map():
    """
    Parse the map given by the engine.
    :return:
    """
    logging.info("---NEW TURN---")
    return game_map.parse(get_string())
