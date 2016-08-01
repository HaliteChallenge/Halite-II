import unittest
import MySQLdb

parser = configparser.ConfigParser()
parser.read("../halite.ini")

class WorkerTestCase(unittest.TestCase):
    def setUp(self):
        self.db = MySQLdb.connect(host=parser["database"]["hostname"], user=parser["database"]["username"], passwd=parser["database"]["password"], db=parser["database"]["name"])

    def testGame(self):
        pass
    def tearDown(self):
        pass
