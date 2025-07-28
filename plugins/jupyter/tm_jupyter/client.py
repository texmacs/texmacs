#!/usr/bin/env python

import os
import sys

import base64
import binascii
import uuid
import argparse
import re
import signal
import time

ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')

# for debug pretty printing
import json

import jupyter_client

# ~ import logging

# ~ logging.basicConfig(filename="/tmp/tm-jupyter.log",
                    # ~ filemode='a',
                    # ~ format='%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                    # ~ datefmt='%H:%M:%S',
                    # ~ level=logging.DEBUG)

# ~ logging.info("Running TeXmacs Jupyter plugin")

# ~ logger = logging.getLogger('tm-jupyter')

parser = argparse.ArgumentParser()
parser.add_argument('-k','--kernel', help='Specify the kernel to run',required=False)
args = parser.parse_args()

from protocol import texmacs as tp

### From tmpy/completion.py
def from_scm_string(s):
    if len(s) > 2 and s[0] == '"' and s[-1] == '"':
        return s[1:-1] 
    return s    

def parse_complete_command(s):
    """HACK"""
    t1 = s.strip().strip('()').split(' ', 1)
    t2 = t1[1].rsplit(' ', 1)
    # Don't use strip('"') in case there are several double quotes
    return [t1[0], from_scm_string(t2[0]), int(t2[1])]
###

def signal_handler(sig, frame):
    with tp.flush("error"):
        print("Received signal Plugin Terminating")
    time.sleep(10)
    sys.exit(0)

def main():
    # According to doc, SIGINT is sent, but QProcess.terminate sends SIGTERM
    # Doesn't seem to work anyhow
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # TeXmacs seems to only accept one data block at plugin startup:
    # send banner and prompt as one block before command output starts
    # if they are sent separately, the first command output can be dropped
    with tp.flush("verbatim",escape=False):
        with tp.flush("prompt"):
            print((">>" if args.kernel == None else args.kernel) + "> ", end='')
        
        try:
            if args.kernel:
                km, kc = jupyter_client.manager.start_new_kernel(kernel_name=args.kernel) # returns KernelManager, BlockingKernelClient
            else:
                km, kc = jupyter_client.manager.start_new_kernel()
            kc.wait_for_ready()

            msg_id = kc.kernel_info()
            reply = kc.get_shell_msg()
            while reply['parent_header']['msg_id'] != msg_id and \
                    reply['header']['msg_type'] != "kernel_info_reply":
                reply = kc.get_shell_msg()
            kc.wait_for_ready()
            
            with tp.flush("verbatim"):
                print(reply['content']['banner'])
                
            with tp.flush("prompt"):
                print(reply['content']['language_info']['name'] + "> ", end='')
            
            with tp.flush("command"):
                print("(jupyter-register-kernel \"" + reply['header']['session'] + "\" \"" + reply['content']['language_info']['name'] + "\")")
        except FileNotFoundError:
            with tp.flush("verbatim"):
                print("Could not start kernel")
                km = None
                kc = None

    while kc != None:
        line = input()
        if not line:
            continue
        elif line[0] == tp.DATA_COMMAND:
            # completion request
            sf = parse_complete_command(line[1:])
            if sf[0] == 'complete':
                req_str = sf[1]
                req_pos = sf[2]
                # ~ logger.info("completing: " + req_str)
                # ~ logger.info("cursor at: " + str(req_pos))
                msg_id = kc.complete(sf[1],sf[2])
                # wait for reply
                reply = kc.get_shell_msg()
                while reply['parent_header']['msg_id'] != msg_id \
                     and reply['header']['msg_type'] != "complete_reply":
                    reply = kc.get_shell_msg()
                output = kc.get_iopub_msg()
                while output['content'].get('execution_state') != "idle":
                    output = kc.get_iopub_msg()
                rep_matches = reply['content']['matches']
                rep_status = reply['content']['status']
                # ~ logger.info("matches: " + ' '.join(rep_matches))
                if (rep_status != "ok") or (len(rep_matches) == 0):
                    with tp.flush("scheme"):
                        print("(tuple)")
                else:
                    rep_pos_start = reply['content']['cursor_start']
                    rep_pos_end = reply['content']['cursor_end']
                    rep_len = rep_pos_end - rep_pos_start
                    with tp.flush("scheme"):
                        print("(tuple \"" + req_str[:req_pos] + "\" " \
                         + ' '.join(["\"" + match[rep_len:] + "\"" for match in rep_matches]) + ")")
                continue

        lines = [line]
        while line != "<EOF>":
            line = input ()
            if line == '':
                continue
            lines.append(line)
        code = '\n'.join(lines[:-1])
        
        msg_id = kc.execute(code)
        # wait for reply
        reply = kc.get_shell_msg()
        while reply['parent_header']['msg_id'] != msg_id and \
               reply['header']['msg_type'] != "execute_reply" and \
               reply['content']['status'] != "ok":
             reply = kc.get_shell_msg()
        
        # We can have multiple responses before the kernel returns `idle`, nest them all in a `verbatim` block
        # Don't escape the output here, otherwise everything inside is escaped twice
        with tp.flush("verbatim",escape=False):
            if 'payload' in reply['content'].keys() and len(reply['content']['payload']) > 0:
                with tp.flush("verbatim"):
                    for pl in reply['content']['payload']:
                        print(pl.get('data').get("text/plain"))
            output = kc.get_iopub_msg()
            # print(json.dumps(output, indent=4, sort_keys=True, default=str))
            while output['content'].get('execution_state') != "idle":
                # print(json.dumps(output, indent=4, sort_keys=True, default=str))
                if output['parent_header']['msg_id'] != msg_id:
                    raise BufferError("output id does not match request")
                elif (output['header']['msg_type'] == "execute_result") \
                    or (output['header']['msg_type'] == "display_data"):
                    if output['content'].get('data').get("image/png") != None:
                        imageformat = "png"
                        image_data = output['content'].get('data').get("image/png")
                        raw = base64.decodebytes(image_data.encode("ascii"))
                        with tp.flush("texmacs"):
                            print("<image|<tuple|<#" + \
                              binascii.hexlify(raw).decode("ascii") \
                              + ">|jupyter-output-" + str(uuid.uuid1()) \
                              + "." + imageformat +  ">|0.618par|||>" )
                    elif output['content'].get('data').get("text/html") != None:
                        with tp.flush("html"):
                            print(output['content'].get('data').get("text/html"))
                    elif output['content'].get('data').get("text/latex") != None:
                        with tp.flush("latex"):
                            print(output['content'].get('data').get("text/latex"))
                    elif output['content'].get('data').get("text/plain") != None:
                        with tp.flush("verbatim"):
                            print(output['content'].get('data').get("text/plain"))
                elif (output['header']['msg_type'] == "error"):
                    with tp.flush("verbatim"):
                        print(output['content'].get('ename'),file=sys.stderr)
                        print(output['content'].get('evalue'),file=sys.stderr)
                elif (output['header']['msg_type'] == "stream") and (output['content'].get('name') == "stdout"):
                    with tp.flush("verbatim"):
                        print(output['content'].get('text'))
                else:
                    pass
                output = kc.get_iopub_msg()

if __name__ == "__main__":
    main()
