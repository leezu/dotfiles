### CUDA
set PATH /usr/local/cuda-8.0/bin $PATH
set -x LD_LIBRARY_PATH "/usr/local/cuda-8.0/lib64:/usr/local/cudnn-5.1/lib64:/opt/intel/lib/intel64_lin:$LD_LIBRARY_PATH"
set -x LIBRARY_PATH "/usr/local/cuda-8.0/lib64:/usr/local/cudnn-5.1/lib64:/opt/intel/lib/intel64_lin:$LIBRARY_PATH"
set -x LD_RUN_PATH "/usr/local/cuda-8.0/lib64:/usr/local/cudnn-5.1/lib64:$LD_RUN_PATH"
set -x CPATH "/usr/local/cuda-8.0/include:/usr/local/cudnn-5.1/include:$CPATH"
set -x INCLUDE_PATH "/usr/local/cuda-8.0/include:/usr/local/cudnn-5.1/include:$INCLUDE_PATH"
set -x PKG_CONFIG_PATH "/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
