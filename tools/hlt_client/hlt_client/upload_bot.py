import os
import sys
import zipfile
import requests
from . import client

_BOT_FILE_NAME_PREPEND = 'MyBot.'
_LANGUGAGE_PROJECT_FILE_IDENTIFIERS = ['cargo.toml', 'project.clj', 'package.swift']
_HALITE_LIBRARY_FOLDER = 'hlt/'


def _bot_exists(user_id):
    """
    Whether a bot has been previously uploaded. Assumes that a user only has bot 0
    :param user_id: The id of the user making the request
    :return: True if created, False otherwise
    """
    return requests.get(client.URI_API_EXISTING_BOT.format(user_id, client.FIRST_BOT_ID)).status_code == client.SUCCESS


def _upload_bot(user_id, api_key, bot_path):
    """
    Uploads the bot to the Halite servers. If the bot exists, simply update using PUT. Otherwise create it with
    POST. NOTE: Assuming only bot is bot 0
    :param user_id: The ID of the user making the request
    :param api_key: The API key provided by the server (acquired in the auth phase)
    :param bot_path: The path wherein the bot is found (expected file)
    :return: The result of the request to the server
    """
    files = {client.BOT_FILE_KEY: open(bot_path, 'rb')}
    headers = {client.API_KEY_HEADER: api_key}
    if _bot_exists(user_id):
        return requests.put(client.URI_API_EXISTING_BOT.format(user_id, client.FIRST_BOT_ID), files=files, headers=headers)
    else:
        return requests.post(client.URI_API_CREATE_BOT.format(user_id), files=files, headers=headers)


def _zip_file_integrity_check(file_path):
    """
    Determines whether the zip file contents are structured correctly to be uploaded.
    That includes the zip being properly zipped and the correct files being placed
    :param file_path:
    :return:
    """
    try:
        zip = zipfile.ZipFile(file_path)
    except zipfile.BadZipFile:
        raise TypeError("The file provided is not a proper zip file")
    except FileNotFoundError:
        raise FileNotFoundError("Could not find the zip file provided")
    if not any((item.startswith(_BOT_FILE_NAME_PREPEND)
                or item.lower() in _LANGUGAGE_PROJECT_FILE_IDENTIFIERS) for item in zip.namelist()):
        raise ValueError("MyBot.* file must be present in the zip's top directory (or cargo.toml in case of Rust).")
    if not any(item.lower().startswith(_HALITE_LIBRARY_FOLDER) for item in zip.namelist()):
        sys.stderr.write("WARNING: Could not find an hlt/ library folder. Proceeding with upload. {}".format(os.linesep))


def upload(bot_path):
    """
    Uploads the bot placed under bot_path. May only be called once Config is properly initialized.
    :param bot_path: The path wherein the bot is located
    :return: Nothing
    """
    _zip_file_integrity_check(bot_path)
    config = client.Config()
    if not bot_path or not os.path.isfile(bot_path):
        raise ValueError("Bot path is not valid or does not exist. Try again.")
    print("Uploading bot...")
    result = _upload_bot(config.user_id, config.api_key, bot_path)
    if result.status_code != client.SUCCESS:
        raise IOError("Unable to upload bot: {}".format(result.text))
    print("Successfully uploaded bot")
