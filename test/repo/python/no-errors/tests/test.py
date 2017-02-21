import unittest

class HelloWorld(unittest.TestCase):
    def test_hello(self):
        self.assert_('Hello, world!' == 'Hello, world!')
