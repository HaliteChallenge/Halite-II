"""
Migrate users from a Halite 1 database to Halite 2.

This script proceeds in two steps: first generating organization entries,
then importing the users themselves.

Without "execute" mode, the script only prints out what new organizations
should be created.
"""

import argparse
import collections
import pprint

import sqlalchemy


SOURCE_DATABASE_URL = ""
TARGET_DATABASE_URL = ""

source_engine = sqlalchemy.create_engine(SOURCE_DATABASE_URL)
source_metadata = sqlalchemy.MetaData(bind=source_engine)
source_users = sqlalchemy.Table("User", source_metadata, autoload=True)

target_engine = sqlalchemy.create_engine(TARGET_DATABASE_URL)
target_metadata = sqlalchemy.MetaData(bind=target_engine)
target_organizations = sqlalchemy.Table("Organization", target_metadata, autoload=True)
target_users = sqlalchemy.Table("User", target_metadata, autoload=True)


# Common email provider domains, exclude from list
GENERIC_DOMAINS = ["gmail.com", "msn.com", "yahoo.com", "live.com", "hotmail.com"]


def find_organizations():
    organizations = collections.defaultdict(lambda: (set(), set()))

    with source_engine.connect() as conn:
        for user in conn.execute(source_users.select()):
            if user["organization"].strip() == "Other":
                continue

            email = user["email"]
            if email:
                if "@" in email:
                    domain = email.split("@")[1].strip().lower()
                    if any(domain == generic for generic in GENERIC_DOMAINS):
                        # Make sure to at least create the org
                        _ = organizations[user["organization"]]
                    else:
                        organizations[user["organization"]][0].add(domain)
                else:
                    print("Weird email", email, user)

            level = user["level"]
            organizations[user["organization"]][1].add(level)

    return organizations


def create_organizations(organizations):
    with target_engine.connect() as target_conn:
        for organization, emails in organizations.items():
            target_conn.execute(target_organizations.insert().values(

            ))


def create_users():
    pass


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Import users from Halite 1 to Halite 2.")
    parser.add_argument("--execute", action="store_true")

    args = parser.parse_args()

    if args.execute:
        create_organizations(find_organizations())
    else:
        pprint.pprint(find_organizations())
