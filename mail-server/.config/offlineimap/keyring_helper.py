import re
import subprocess

try:
    import keyring
except ImportError:
    keyring = None


def get_gpg_pass(service_name, storage="/home/leonard/.authinfo.gpg"):
    command = ("gpg", "-d", storage)
    output = subprocess.check_output(command)
    for line in output.split('\n'):
        r = re.match(r'.+{}.+ password "(.+)"'.format(service_name), line)
        if r:
            return r.group(1)
    return None


def get_pass(service_name, username):
    if keyring:
        password = keyring.get_password(service_name, username)
        if password is not None:
            return password

    password = get_gpg_pass(username)
    return password
