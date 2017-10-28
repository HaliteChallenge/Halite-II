import sys, termios
from select import select

class keyboard_detection:
	''' 
	Use in a with statement to enable the appropriate terminal mode to detect keyboard presses
	without blocking for input.  Used this way, the with statement puts a boolean detection
	function in the target variable.  The resulting function can be called any number of times
	until a keypress is detected.  Sample code:

		with keyboard_detection() as key_pressed:
			while not key_pressed():
				sys.stdout.write('.')
				sys.stdout.flush()
				sleep(0.5)
		print 'done'

	Upon exiting the with code block, the terminal is reverted to its calling (normal) state.
	The sys.stdout.flush() is important when in the keyboard detection mode; otherwise, text
	output won't be seen.
	'''
	def __enter__(self):
		# save the terminal settings
		self.fd = sys.stdin.fileno()
		self.new_term = termios.tcgetattr(self.fd)
		self.old_term = termios.tcgetattr(self.fd)

		# new terminal setting unbuffered
		self.new_term[3] = (self.new_term[3] & ~termios.ICANON & ~termios.ECHO)		

		# switch to unbuffered terminal
		termios.tcsetattr(self.fd, termios.TCSAFLUSH, self.new_term)
		
		return self.query_keyboard

	def __exit__(self, type, value, traceback):
		# swith to normal terminal
		termios.tcsetattr(self.fd, termios.TCSAFLUSH, self.old_term)
		
	def query_keyboard(self):
		dr, dw, de = select([sys.stdin], [], [], 0)
		return dr != []		


if __name__ == '__main__':

	from time import sleep

	with keyboard_detection() as key_pressed:
		while not key_pressed():
			sys.stdout.write('.')
			sys.stdout.flush()
			sleep(0.5)

	print ('done')

