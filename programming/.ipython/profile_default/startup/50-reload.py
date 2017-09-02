# https://ipython.org/ipython-doc/3/api/generated/IPython.lib.deepreload.html
import builtins
from IPython.lib import deepreload
builtins.reload = deepreload.reload
