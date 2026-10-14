"""Regression tests for code sent from TeXmacs to a Jupyter kernel."""

import contextlib
import io
from pathlib import Path
import runpy
import sys
import unittest
from unittest.mock import Mock, patch


CLIENT = Path(__file__).resolve().parents[1] / "tm_jupyter" / "client.py"


class ClientInputTests(unittest.TestCase):
    def execute_requests(self, requests):
        kernel = Mock()
        kernel.kernel_info.return_value = "info"
        kernel.execute.return_value = "execute"
        kernel.get_shell_msg.side_effect = [
            {
                "parent_header": {"msg_id": "info"},
                "header": {"msg_type": "kernel_info_reply", "session": "test"},
                "content": {"banner": "", "language_info": {"name": "python"}},
            },
        ] + [
            {
                "parent_header": {"msg_id": "execute"},
                "header": {"msg_type": "execute_reply"},
                "content": {"status": "ok"},
            }
            for _ in requests
        ]
        kernel.get_iopub_msg.return_value = {
            "content": {"execution_state": "idle"},
        }
        jupyter = Mock()
        jupyter.manager.start_new_kernel.return_value = (Mock(), kernel)
        with patch.dict(sys.modules, {"jupyter_client": jupyter}), \
                patch.object(sys, "argv", [str(CLIENT)]), \
                patch.object(sys, "path", [str(CLIENT.parent)] + sys.path):
            client = runpy.run_path(str(CLIENT))

        lines = []
        for code in requests:
            lines.extend(code.split("\n"))
            lines.append("<EOF>")
        # The client currently exits by raising EOFError when stdin closes.
        lines.append(EOFError())
        with patch("builtins.input", side_effect=lines), \
                patch.object(client["signal"], "signal"), \
                patch.object(client["tp"], "flush",
                             side_effect=lambda *a, **kw: contextlib.nullcontext()), \
                contextlib.redirect_stdout(io.StringIO()):
            with self.assertRaises(EOFError):
                client["main"]()
        return [call.args[0] for call in kernel.execute.call_args_list]

    def test_multiline_string_keeps_blank_lines(self):
        code = 'value = """first\n\nlast"""'
        received, = self.execute_requests([code])
        expected_namespace = {}
        actual_namespace = {}
        exec(code, expected_namespace)
        exec(received, actual_namespace)
        self.assertEqual(actual_namespace["value"], expected_namespace["value"])
        self.assertEqual(received, code)

    def test_code_preserves_whitespace(self):
        for code in (
            "x = 1\n\n\ny = 2",
            "x = 1\n\n",
            "if True:\n    x = 1\n    \n    y = 2",
            "x = 1\ny = 2",
            "42",
        ):
            with self.subTest(code=code):
                self.assertEqual(self.execute_requests([code]), [code])

    def test_eof_marker_separates_requests(self):
        requests = ['value = """first\n\nlast"""', "print(value)\n"]
        self.assertEqual(self.execute_requests(requests), requests)


if __name__ == "__main__":
    unittest.main()
