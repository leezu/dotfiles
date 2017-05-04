### Python
## MxNet
set -x PYTHONPATH "/home/leonard/software/mxnet/python:$PYTHONPATH"

### Torch & Lua
set -x LUA_PATH '/home/leonard/software/torch/install/lib/?.so;/home/leonard/.luarocks/share/lua/5.1/?.lua;/home/leonard/.luarocks/share/lua/5.1/?/init.lua;/home/leonard/software/torch/install/share/lua/5.1/?.lua;/home/leonard/software/torch/install/share/lua/5.1/?/init.lua;./?.lua;/home/leonard/software/torch/install/share/luajit-2.1.0-beta1/?.lua;/usr/local/share/lua/5.1/?.lua;/usr/local/share/lua/5.1/?/init.lua'
set -x LUA_CPATH '/home/leonard/.luarocks/lib/lua/5.1/?.so;/home/leonard/software/torch/install/lib/lua/5.1/?.so;./?.so;/usr/local/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/loadall.so'
set PATH /home/leonard/software/torch/install/bin $PATH
set -x LD_LIBRARY_PATH "/home/leonard/software/torch/install/lib:$LD_LIBRARY_PATH"
set -x DYLD_LIBRARY_PATH "/home/leonard/software/torch/install/lib:$DYLD_LIBRARY_PATH"
