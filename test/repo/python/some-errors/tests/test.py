import unittest

class HelloWorld(unittest.TestCase):
    def test_hello(self):
        self.assert_('Hello, world!' != 'Hello, world!')

    def test_error(self):
        raise Exception('Goodbye, world!')
