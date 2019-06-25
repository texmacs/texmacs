import os
import traceback

class CaptureStdout:
    """Capture output to os.sys.stdout.

    Class in charge of recording the output of the
    statements/expressions entered in the TeXmacs
    session and executed in Python.

    Must be used in a with statement, as in CaptureStdout.capture()
    """

    def __enter__(self):
        """ """
        class Capture:
            def __init__(self):
                self.text = ''
            def write(self, str):
                self.text += str
            def flush(self):
                os.sys.stdout.flush() # Needed?
                self.text = ''
            def getOutput(self):
                return self.text

        self.capt = Capture()
        self.stdout_saved, os.sys.stdout = os.sys.stdout, self.capt        
        return self.capt
    
    def __exit__(self, type, value, traceback):
        os.sys.stdout = self.stdout_saved

    @staticmethod
    def capture (code, env, filename):
        with CaptureStdout() as capt:
            try:
                eval (compile (code, filename, 'exec'), env)
            except Exception as e:
                traceback.print_exc (file = os.sys.stdout, limit = 0)
            return capt.getOutput()

