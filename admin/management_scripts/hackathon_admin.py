#!/usr/bin/env python3
"""
Hackathon administration tool for Halite.

Dependencies: Python 3, arrow, requests
"""

import argparse
import getpass
import json
import os
import sys
import urllib.parse

import arrow
import dateutil.tz
import requests

API_KEY_HEADER = "X-Api-Key"
CONFIG_FILE = os.path.join(os.path.split(__file__)[0],
                           "./hackathon_config.json")
CONFIG_CACHE = None
DEFAULT_CONFIG = {
    "api_endpoint": "https://api.halite.io/v1/api/",
}


def get_config():
    if CONFIG_CACHE is not None:
        return CONFIG_CACHE

    try:
        with open(CONFIG_FILE) as f:
            return json.load(f)
    except FileNotFoundError:
        return DEFAULT_CONFIG.copy()
    except json.JSONDecodeError as e:
        print("WARNING: Could not parse config file:", e, file=sys.stderr)
        return DEFAULT_CONFIG.copy()


def write_config(config):
    with open(CONFIG_FILE, "w") as f:
        json.dump(config, f, indent=4)

    global CONFIG_CACHE
    CONFIG_CACHE = config


def auth_main():
    api_key = getpass.getpass(prompt="API Key: ")
    config = get_config()
    config["api_key"] = api_key
    write_config(config)


def parse_date(s):
    return arrow.get(s).replace(tzinfo=dateutil.tz.tzlocal())


def parse_organization(s):
    if s:
        return int(s)
    return None


def get_input(prompt, conversion, default):
    if default is not None:
        print("Current value: ", default)

    while True:
        try:
            value = input(prompt)
            if not value and default is not None:
                return default
            return conversion(value)
        except (ValueError, arrow.parser.ParserError) as e:
            print("Could not interpret value:", e)


def create_main():
    config = get_config()
    url = urllib.parse.urljoin(config["api_endpoint"], "hackathon")

    title = None
    description = None
    start_date = None
    end_date = None
    organization_id = ""

    while True:
        title = get_input(prompt="Hackathon title: ",
                          conversion=lambda x: x,
                          default=title)
        description = get_input(prompt="Hackathon description: ",
                                conversion=lambda x: x,
                                default=description)
        start_date = get_input(prompt="Start date (yyyy-mm-dd hh:mm): ",
                               conversion=parse_date,
                               default=start_date)
        end_date = get_input(prompt="End date (yyyy-mm-dd hh:mm): ",
                             conversion=parse_date,
                             default=end_date)
        organization_id = get_input(prompt="Organization ID (optional): ",
                                    conversion=parse_organization,
                                    default=organization_id)

        if start_date >= end_date:
            print("Start date must be before end date")
            continue

        print("Title       :", title)
        print("Description :", description)
        print("Start date  :", start_date)
        print("End date    :", end_date)
        print("Organization:", organization_id)

        print()

        proceed = input("Proceed [y/n]?")

        if proceed == "y":
            break

        print("Editing values. Press Enter to accept previous value.")

    data = {
        "title": title,
        "description": description,
        "start_date": str(start_date),
        "end_date": str(end_date),
    }

    if organization_id:
        data["organization_id"] = organization_id

    request = requests.post(url, data=data, headers={
        API_KEY_HEADER: config["api_key"]
    })
    print(request.text)


def list_main():
    config = get_config()
    url = urllib.parse.urljoin(config["api_endpoint"], "hackathon")
    request = requests.get(url, headers={
        API_KEY_HEADER: config["api_key"],
    })
    print(request.text)


def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(title="subcommand", dest="subcommand")
    subparsers.required = True

    auth_parser = subparsers.add_parser("auth")
    auth_parser.set_defaults(main_func=auth_main)

    create_parser = subparsers.add_parser("create")
    create_parser.add_argument("--title")
    create_parser.add_argument("--description")
    create_parser.add_argument("--start-date")
    create_parser.add_argument("--end-date")
    create_parser.set_defaults(main_func=create_main)

    list_parser = subparsers.add_parser("list")
    list_parser.set_defaults(main_func=list_main)

    args = parser.parse_args()
    args.main_func()


if __name__ == "__main__":
    main()
