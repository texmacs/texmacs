import os

class Graph(object):
    name = ""
    message = ""

    def greet(self):
        for x in self.message.split("\n"):
            if len(x) == 0:
                pass
            else:
                texmacs_out("verbatim:" + x + DATA_BEGIN + "prompt#" + self.name + "] " + DATA_END)
        os.sys.stdout.flush()

    def available(self):
        return len(self.message) > 0

    def evaluate(self, code):
        pass

    def get_png_path(self):
        png = os.getenv("HOME") + "/.TeXmacs/system/tmp/" + self.name + ".png"
        if os.path.isfile(png):
            os.remove(png)
        return png
