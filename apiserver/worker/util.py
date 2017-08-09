import subprocess


def kill_processes_as(user, process_name):
    """
    Kill all processes of a given name belonging to a given user.
    :param user:
    :param process_name:
    :return:
    """
    subprocess.call(["sudo", "-H", "-u", user, "-s",
                     "killall", "-9", process_name],
                    stderr=subprocess.PIPE,
                    stdout=subprocess.PIPE)
