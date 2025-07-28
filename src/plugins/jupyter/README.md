# tm-jupyter-client

A [TeXmacs](https://www.texmacs.org/) plugin to run and communicate with [Jupyter kernels](https://github.com/jupyter/jupyter/wiki/Jupyter-kernels).

##### Notes

- [Messaging in Jupyter](https://jupyter-client.readthedocs.io/en/latest/messaging.html)

- [`jupyter_client` package](https://jupyter-client.readthedocs.io/en/latest/api/jupyter_client.html)

- The [`jupyterlab-kernelspy`](https://github.com/jupyterlab-contrib/jupyterlab-kernelspy) plugin is a valuable tool to understand the Jupyter protocol.

- In the `iopub.execute_result` message, a number of mimetypes can be sent at the same time. Kernels decide which mimetypes to send in this "mime bundle".
For example, in Julia, one can do `using IJulia; IJulia.register_mime(MIME("text/x-texmacs"))` and 
```
function Base.show(io::IO, ::MIME"text/x-texmacs", v::MyType)
    print(io,"My Type ", string(v.x))
end
```

- The default mimetypes in Julia are
```
 MIME type text/plain
 MIME type image/svg+xml
 MIME[MIME type image/png, MIME type image/jpeg]
 MIME[MIME type text/markdown, MIME type text/html]
 MIME type text/latex
```
