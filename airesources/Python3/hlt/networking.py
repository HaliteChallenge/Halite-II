import sys
import logging
import copy

from . import game_map


class Game:
    """
    :ivar map: Current map representation
    :ivar initial_map: The initial version of the map before game starts
    """
    @staticmethod
    def _send_string(s):
        """
        Send data to the game. Call :function:`done_sending` once finished.

        :param str s: String to send
        :return: nothing
        """
        sys.stdout.write(s)

    @staticmethod
    def _done_sending():
        """
        Finish sending commands to the game.

        :return: nothing
        """
        sys.stdout.write('\n')
        sys.stdout.flush()

    @staticmethod
    def _get_string():
        """
        Read input from the game.

        :return: The input read from the Halite engine
        :rtype: str
        """
        result = sys.stdin.readline().rstrip('\n')
        return result

    @staticmethod
    def send_command_queue(command_queue):
        """
        Issue the given list of commands.

        :param list[str] command_queue: List of commands to send the Halite engine
        :return: nothing
        """
        for command in command_queue:
            Game._send_string(command)

        Game._done_sending()

    @staticmethod
    def _set_up_logging(tag, name):
        """
        Set up and truncate the log

        :param tag: The user tag (used for naming the log)
        :param name: The bot name (used for naming the log)
        :return: nothing
        """
        log_file = "{}_{}.log".format(tag, name)
        logging.basicConfig(filename=log_file, level=logging.DEBUG, filemode='w')
        logging.info("Initialized bot {}".format(name))

    def __init__(self, name):
        """
        Initialize the bot with the given name.

        :param name: The name of the bot.
        """
        self._name = name
        self._send_name = False
        tag = int(self._get_string())
        Game._set_up_logging(tag, name)
        width, height = [int(x) for x in self._get_string().strip().split()]
        self.map = game_map.Map(tag, width, height)
        self.update_map()
        self.initial_map = copy.deepcopy(self.map)
        self._send_name = True

    def update_map(self):
        """
        Parse the map given by the engine.

        :return: new parsed map
        :rtype: game_map.Map
        """
        if self._send_name:
            self._send_string(self._name)
            self._done_sending()
            self._send_name = False
        logging.info("---NEW TURN---")
        self.map._parse(self._get_string())
        return self.map
